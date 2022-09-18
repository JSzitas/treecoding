#ifndef TREE_HEADER
#define TREE_HEADER

// #include <variant>
#include <vector>
#include "utils.h"
#include "ranges.h"
#include "data.h"
#include "iostream"
#include "stdio.h"

struct node {
  node(){};
  node_split<float, int> range;
  std::vector<int> index;
  // int node_id;
};

struct terminal_node{
  terminal_node(){};
  intervals<float,int> range;
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
    result.range = NumericInterval( NumericRange<Numeric>( data.lower, sample(data, generator)), col);
    result.type = true;
    return result;
  };
  template <class RnGenerator> node_split<Numeric, Categorical> yield(
                                         int col,
                                         std::vector<Categorical> &x,
                                         std::vector<int> &subset,
                                         RnGenerator & generator) {
    node_split<Numeric, Categorical> result;
    result.set = sample_distinct(col, x, subset, generator);
    result.type = false;
    return result;
  };
  template <class RnGenerator> node_split<Numeric, Categorical> operator () (
                                                int col,
                                                storage::DataFrame<Numeric, Categorical> &data,
                                                std::vector<int> &subset,
                                                RnGenerator & generator) {
    if( col > data.num_cols) {
      return yield( col, data.cat_data[col - data.num_cols], subset, generator);
    }
    else {
      return yield(col, data.num_data[col], subset, generator);
    };
  };
};

template <class T> void print_vector( T& x ) {
  if( x.size() == 0 ) {
    return;
  }
  for(int i=0;i<(x.size()-1);i++) {
  std::cout << x[i] << ", ";
  }
  std::cout << x[(x.size()-1)] << std::endl;
}

template <class RngGenerator, class Splitter> class Tree {
  public:
    Tree(
        storage::DataFrame<float, int> &data,
        RngGenerator & generator,
        Splitter & splitter,
        int max_depth = 8,
        int min_nodesize = 30) : X(data), node_splitter(splitter),
        gen(generator), tree_max_depth(max_depth), tree_min_nodesize(min_nodesize){
      nonconst_cols = data.nonconst_cols();
    };
    node grow( std::vector<int> &row_ids, std::vector<int> nonconst_cols ){
      node tree;
      int col;
        for( int i=0; i < nonconst_cols.size(); i++ ){
          // if we have no nonconst columns, return
          if( nonconst_cols.size() < 1 ) {
            tree.index = row_ids;
            return tree;
          }
          // First we take the data and generate a candidate split
          col = sample_int_from_set( nonconst_cols, gen );
          // // check that the column is not all const
          // // and if it is probably just update the nonconst_cols
          // // and GOTO before this happened. which I admit is ugly, but this is
          // // probably the one reasonable case where you want to do that.
          if( X.col_is_const( col, row_ids )) {
            nonconst_cols = set_diff(nonconst_cols, col);
          }
          else{
            // if we found a valid column, break
            break;
          }
        }
        // declare split
        auto split_res = node_splitter(col, X, row_ids, gen);
        tree.range = split_res;
        tree.index = row_ids;
        return tree;
    };
    void fit() {
      // you know how many trees at most there will be based on the max depth ->
      // you will always have at most 2^max_depth total nodes
      // because you start at depth 0 with 1 node, followed by
      // 2 nodes at depth 1, 4 at depth 2, 8 at depth 3...
      // further, you know you can can number these uniquely quite easily
      // thus our tree is a (linearly) laid out vector of nodes
      std::vector<node> tree( 2<<tree_max_depth );
      auto seq = sequence(X.rows());
      // std::cout << "Init tree: " <<tree.size() << std::endl;
      tree[0] = grow( seq, nonconst_cols );
      // tree[0].index = seq;
      // preallocate indices of children, parent, terminal nodes
      // int current_depth = 0;
      int parent;
      std::vector<int> child_indices;
      // std::vector<int> terminal_node_indices;
      std::unordered_set<int> terminal_node_indices;
      std::vector<terminal_node> terminal_nodes;
      // std::cout << "Start of loop: " <<tree.size() << std::endl;
      // std::cout << "Tree max depth" << tree_max_depth << std::endl;
      // return;
      // we can iterate through this and fill out nodes as necessary
      for( int current_depth=0; current_depth <= tree_max_depth; current_depth++ ) {
        // child indices can be inferred from the depth
        child_indices = make_pow2_indices(current_depth);
        // print_vector(child_indices);
        for( auto &index:child_indices ) {
          // find parent as the rounded down half of the index of current child
          // with 1 subtracted for indexing
          parent = (int)(index/2) -1;
          // std::cout << "Parent index: " << parent << std::endl;
          // std::cout << "Parent size: " << tree[parent].index.size() << std::endl;
          // return;
          // move the contents of indices in parent to children - or optionally
          // just clear them if parent terminates - and add parent to terminal nodes
         if( tree[parent].index.size() > tree_min_nodesize && (current_depth < tree_max_depth )) {
           // allocate parents indices to children
           auto res = X.match(tree[parent].range, tree[parent].index);
           // grow left and right subtree
           tree[index-1] = grow( res.left, nonconst_cols );
           // std::cout << "Growing left at node: " << index-1 << std::endl;
           tree[index] = grow(res.right, nonconst_cols);
           // std::cout << "Growing right at node: " << index << std::endl;
         }
         else{
           // add parent to list of terminal nodes
           if( !terminal_node_indices.count(parent) ) {
             // std::cout << "Pushing to terminal node: " << parent << std::endl;
             terminal_node_indices.insert(parent);
             // std::cout << "We currently have " << terminal_node_indices.size()<< " terminal nodes " << std::endl;
           }
          }
        }
      }

      return;
    };
    // creating predictions
    // void predict();
  private:
    int tree_max_depth;
    int tree_min_nodesize;
    storage::DataFrame<float, int> &X;
    std::vector<node> tree;
    Splitter &node_splitter;
    std::vector<int> nonconst_cols;
    RngGenerator &gen;
};

#endif
