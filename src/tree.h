#ifndef TREE_HEADER
#define TREE_HEADER

#include <vector>
#include "utils.h"
#include "ranges.h"
#include "data.h"
#include "terminal_node.h"
#include "sampling.h"

struct node {
  node() {
    this->left = nullptr;
    this->right = nullptr;
    this->range = node_split<float,int>();
    this->node_id = 1;
  };
  ~node() {
    delete left;
    delete right;
  };
  node_split<float, int> range;
  int node_id;
  node * left, * right;
};

struct encoded {
  encoded() {
    this->observation_id = 0;
    this->node_id = 0;
  };
#ifdef DEBUG
  void print() {
    std::cout << "Node: " << node_id << " Observation: "<< observation_id << std::endl;
  }
#endif
  int observation_id;
  int node_id;
};

struct decoded {
  decoded(){
    this->observation_ids = std::vector<int>(0);
    this->decoded_values = terminal_node<float, int>();
  };
  terminal_node<float, int> decoded_values;
  std::vector<int> observation_ids;
};

std::vector<int> find_tree_path( int terminal_id ) {
  std::vector<int> result;
  // result.reserve(2);
  int current_id = terminal_id;
  while( current_id >= 1 ) {
    result.push_back(current_id);
    current_id =(int) current_id/2;
  }
  reverse(result);
  return result;
}

std::vector<int> match_terminal_nodes( std::vector<encoded> &x,
                                       int terminal_node ) {
  std::vector<int> result;
  for( auto &item:x ) {
    if(item.node_id == terminal_node) {
      result.push_back(item.observation_id);
    }
  }
  return result;
}

terminal_node<float, int> decode_terminal_path( node *tree,
                                                std::vector<int> &path ) {

  terminal_node<float, int> result;
  NumInterval<float> num_val;
  CatSet<int> cat_val;
  node * current_node = tree;
  for( unsigned long long i=1; i < path.size(); i++) {
    // this determines if we go left or right
    if(path[i] % 2 == 0) {
      // determine if we are assigning a categorical or numeric value
      if(current_node->range.type) {
        num_val.set(current_node->range.range.lower_val,
                    current_node->range.range.middle_val);
        result.add(num_val, current_node->range.col);
      }
      else {
        cat_val.set(current_node->range.set.set_vals);
        result.add(cat_val, current_node->range.col);
      }
      current_node = current_node->left;
    }
    // iif we go right instead
    else {
      if(current_node->range.type) {
        num_val.set(current_node->range.range.middle_val,
                    current_node->range.range.upper_val);
        result.add(num_val, current_node->range.col);
      }
      else {
        cat_val.set(current_node->range.set.out_vals);
        result.add(cat_val, current_node->range.col);
      }
      current_node = current_node->right;
    }
  }
  return result;
}

template <typename Numeric, typename Categorical> struct RandomSplitter{
  RandomSplitter<Numeric, Categorical>(){};
  template <class RnGenerator> node_split<Numeric, Categorical> yield(
      int col,
      std::vector<Numeric> &x,
      std::vector<int> &subset,
      RnGenerator & generator) {
    node_split<Numeric, Categorical> result;
    auto data = min_max_subset(x, subset);
    auto middle = sample(data, generator);
    result.range = NumericInterval(data.lower, middle, data.upper);
    result.type = true;
    result.col = col;
    return result;
  }
  template <class RnGenerator> node_split<Numeric, Categorical> yield(
      int col,
      std::vector<Categorical> &x,
      std::vector<int> &subset,
      RnGenerator & generator) {
    node_split<Numeric, Categorical> result;
    auto newset = distinct( x, subset);
    auto split_res = split_set( newset, generator );
    result.set = CategoricalSet<Categorical>( split_res.left, split_res.right );
    result.type = false;
    result.col = col;
    return result;
  }
  template <class RnGenerator> node_split<Numeric, Categorical> operator () (
      int col,
      storage::DataFrame<Numeric, Categorical> &data,
      std::vector<int> &subset,
      RnGenerator & generator) {
    if( col >= data.num_cols) {
      return yield( col, data.cat_data[col - data.num_cols], subset, generator);
    }
    else {
      return yield(col, data.num_data[col], subset, generator);
    }
  }
};

template <class RngGenerator, class Splitter> class Tree {
public:
  Tree(
    storage::DataFrame<float, int> &data,
    RngGenerator & generator,
    Splitter & splitter,
    std::vector<int> rows,
    long long unsigned int max_depth = 8,
    long long unsigned int min_nodesize = 30) : tree_max_depth(max_depth),
    tree_min_nodesize(min_nodesize), row_ids(rows), X(data),
    node_splitter(splitter), gen(generator) {
    nonconst_cols = data.nonconst_cols();
  };
  ~Tree() {
    delete tree;
  };
  node* grow( std::vector<int> &row_ids, std::vector<int> nonconst_cols, int id =1 ) {
    node * tree = new node();
    tree->node_id = id;
    int col=0;
    for( long long unsigned int i=0; i < nonconst_cols.size(); i++ ){
      // if we have no nonconst columns, return
      if( nonconst_cols.size() < 1 ) {
        return tree;
      }
      // First we take the data and generate a candidate split
      col = sample_int_from_set( nonconst_cols, gen );
      // std::cout << " | Using col: " << col;
      if( X.col_is_const( col, row_ids )) {
        nonconst_cols = set_diff(nonconst_cols, col);
      }
      else{
        // if we found a valid column, break
        break;
      }
    }
    // declare split
    // auto split_res = this->node_splitter(col, this->X, row_ids, this->gen);
    // tree->range = split_res;
    tree->range = this->node_splitter(col, this->X, row_ids, this->gen);
    auto res = X.match(tree->range, row_ids);
    // if we fail to produce 2 children, stop
    if( res.left.size() > tree_min_nodesize &&
        res.right.size() > this->tree_min_nodesize &&
        nonconst_cols.size() > 0) {
      tree->left = grow( res.left, nonconst_cols, 2*id );
      tree->right = grow( res.right, nonconst_cols, (2*id)+1 );
    }
    return tree;
  }
  void fit() {
    this->tree = grow( this->row_ids, this->nonconst_cols );
  };
  void encode_recursion( node* current_node,
                         std::vector<int> &current_obs,
                         std::vector<encoded> & encoded_vals,
                         storage::DataFrame<float, int> &newx ) {
    // if we reach NULL, we are in a terminal node
    if( current_node->left ==  nullptr || current_node->right == nullptr ) {
      for( auto &index:current_obs ) {
        // this move should be save - this is not getting reused afterwards
        encoded_vals[index].observation_id = std::move(index);
        encoded_vals[index].node_id = current_node->node_id;
      }
      return;
    }
    // take our results of match and continue searching
    auto cond_match = newx.match( current_node->range, current_obs);
    encode_recursion( current_node->left, cond_match.left, encoded_vals, newx );
    encode_recursion( current_node->right, cond_match.right, encoded_vals, newx );
  }
  // // creating predictions
  std::vector<encoded> encode( storage::DataFrame<float, int> &newx ) {
    auto size = newx.rows();
    auto seq = sequence( size );
    // preallocate newx rows encoding results
    std::vector<encoded> result( size );
    // recurse through them - hopefully the references here make sense
    encode_recursion( this->tree, seq, result, newx);
    return result;
  }
  std::vector<decoded> decode( std::vector<encoded> &terminal_ids ) {
    // find unique terminal values:
    std::unordered_set<int> terminal_set;
    for( auto &terminal_node:terminal_ids ) {
      if( !terminal_set.count(terminal_node.node_id)) {
        // add terminal node to possible list
        terminal_set.insert(terminal_node.node_id);
      }
    }
    auto terminal = set_to_vect(terminal_set);
    // path through the terminal nodes which we need and collect terminal values
    std::vector<std::vector<int>> all_paths;
    for( auto &item:terminal ) {
      all_paths.push_back(find_tree_path(item));
    }
    // return all_paths;

    std::vector<terminal_node<float, int>> decoding_paths;
    decoding_paths.reserve(all_paths.size());

    // over all paths, get the terminal node values
    for( auto &path:all_paths ) {
      decoding_paths.push_back(decode_terminal_path(this->tree, path));
    }

    std::vector<decoded> result(decoding_paths.size());
    for( int i=0; i < decoding_paths.size(); i++ ) {
      result[i].decoded_values = decoding_paths[i];
      result[i].observation_ids = match_terminal_nodes( terminal_ids, terminal[i]);
    }
    return result;
  }
  std::vector<int> get_oob_obs() {
    auto size = this->X.rows();
    auto seq = sequence(size);
    return set_diff(seq, this->row_ids);
  }
#ifdef DEBUG
  void print_recursion( node* current_node, int depth = 1) {
    if( current_node == nullptr ) {
      return;
    }
    for( int i=0; i < depth; i++) {
      std::cout << "--";
    }
    std::cout << current_node->node_id << std::endl;
    print_recursion( current_node->left, depth+1);
    print_recursion( current_node->right, depth+1);
  }
  void print() {
    print_recursion( this->tree, 1);
  }
#endif
  // void predict();
private:
  long long unsigned int tree_max_depth;
  long long unsigned int tree_min_nodesize;
  std::vector<int> row_ids;
  storage::DataFrame<float, int> &X;
  node *tree;
  Splitter &node_splitter;
  std::vector<int> nonconst_cols;
  RngGenerator &gen;
};

#endif
