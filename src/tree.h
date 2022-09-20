#ifndef TREE_HEADER
#define TREE_HEADER

#include <vector>
#include "utils.h"
#include "ranges.h"
#include "data.h"

struct node {
  node() {
    this->left = NULL;
    this->right = NULL;
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
  int observation_id;
  int node_id;
};

template <typename Numeric, typename Categorical> struct RandomSplitter{
  RandomSplitter<Numeric, Categorical>(){};
  template <class RnGenerator> node_split<Numeric, Categorical> yield(
      int col,
      std::vector<Numeric> &x,
      std::vector<int> &subset,
      RnGenerator & generator) {
    node_split<Numeric, Categorical> result;
    auto data = min_max_subset(x, subset);
    result.range = NumericInterval( NumericRange<Numeric>( data.lower, sample(data, generator)));
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
    result.set = sample_distinct(x, subset, generator);
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
    int max_depth = 8,
    int min_nodesize = 30) : X(data), gen(generator), node_splitter(splitter),
    tree_max_depth(max_depth), tree_min_nodesize(min_nodesize){
    nonconst_cols = data.nonconst_cols();
  };
  ~Tree() {
    delete tree;
  };
  node* grow( std::vector<int> &row_ids, std::vector<int> nonconst_cols, int id =1 ) {
    node * tree = new node();
    tree->node_id = id;
    int col=0;
    for( int i=0; i < nonconst_cols.size(); i++ ){
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
    auto split_res = this->node_splitter(col, this->X, row_ids, this->gen);
    tree->range = split_res;
    auto res = X.match(tree->range, row_ids);
    if( res.left.size() > tree_min_nodesize && nonconst_cols.size() > 0) {
      tree->left = grow( res.left, nonconst_cols, 2*id );
    }
    if( res.right.size() > this->tree_min_nodesize && nonconst_cols.size() > 0 ) {
      tree->right = grow( res.right, nonconst_cols, (2*id)+1 );
    }
    // tree.index = row_ids;
    return tree;
  }
  void fit() {
    auto seq = sequence(this->X.rows());
    this->tree = grow( seq, this->nonconst_cols );
  };
  void encode_recursion( node* current_node,
                         std::vector<int> &current_obs,
                         std::vector<encoded> & encoded_vals,
                         storage::DataFrame<float, int> &newx ) {
    // if we reach NULL, we are in a terminal node
    if( current_node->left == NULL || current_node->right == NULL ) {
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
    auto seq = sequence( newx.rows() );
    // preallocate newx rows encoding results
    std::vector<encoded> result( newx.rows() );
    // recurse through them - hopefully the references here make sense
    encode_recursion( this->tree, seq, result, newx);
    return result;
  }
  void decode( ) {

  }
  // void predict();
private:
  int tree_max_depth;
  int tree_min_nodesize;
  storage::DataFrame<float, int> &X;
  node *tree;
  Splitter &node_splitter;
  std::vector<int> nonconst_cols;
  RngGenerator &gen;
};

#endif
