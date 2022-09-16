#ifndef TREE_HEADER
#define TREE_HEADER

#include <variant>
#include <vector>
#include "utils.h"
#include "ranges.h"
#include "iostream"
#include "stdio.h"
#include "data.h"

struct node {
  node(){};
  node_split<float, int> range;
  bool terminal = false;
  intervals<float, int> terminal_range;
  int current_depth = 0;
  int node_size;
  std::vector<node> children;
};

template <typename Numeric, typename Categorical> struct RandomSplitter{
  node_split<Numeric, Categorical> yield(std::vector<Numeric> &x,
                                         std::vector<int> &subset) {
    return min_max_subset(x, subset);
  };
  node_split<Numeric, Categorical> yield(std::vector<Categorical> &x) {
    return ;
  };
};

template <class RngGenerator>
class Tree {
  public:
    Tree<RngGenerator>( 
          storage::DataFrame<float, int> data,
          std::vector<int> numeric_cols,
          RngGenerator & generator,
          int max_depth = 8,
          int min_nodesize = 30) {
      tree_max_depth = max_depth;
      tree_min_nodesize = min_nodesize;
      gen = generator;
      X = data;
      cols = X.cols();
      tree = node();
      nonconst_cols = X.nonconst_cols(sequence(data.rows()));
      num_cols = numeric_cols;
    };
    void grow( node &tree,
               std::vector<int> row_ids,
               intervals<float, int> ranges = {},
               int current_depth = 0,
               int tree_arity = 2){
      // the goto tag for when we found nothing to split on or a constant column
      // termination_label:
      // termination
      if( row_ids.size() <= tree_min_nodesize || current_depth >= tree_max_depth || nonconst_cols.size() < 1) {
        // std::cout << "Terminal conditions met: ";
        // std::cout << "Depth: " << current_depth << " Nodesize: " << row_ids.size() << std::endl;
        tree.terminal_range = ranges;
        tree.node_size = row_ids.size();
        // tree.terminal = true;
        return;
      }
      // First we take the data and generate a candidate split -
      // for now we will only use random trees, we can try to extend this
      // later
      // generate column over which we will split
      int col = sample_int_from_set( X.nonconst_cols(row_ids);, gen );
      // // check that the column is not all const
      // // and if it is probably just update the nonconst_cols
      // // and GOTO before this happened. which I admit is ugly, but this is
      // // probably the one reasonable case where you want to do that.

      // node_split<float, int> node_vals;

      // check if col is a numeric column
      // if( belongs( num_cols, col) ) {
      //   tree->range = node_split<float, int>( sel_col, col);
      // }
      // else {
      //   // attemp typecast
      //   tree->range = node_split<int, int>( (int)sel_col, col);
      // }
      // determine where row ids go

      // allocate child nodes
      tree.children = std::vector<node>(tree_arity);
      for( int i = 0; i < tree_arity; i++) {
        // recursive calls for constructing children
        grow(tree.children[i], row_ids, ranges, current_depth+1);
      }
    };
    void fit() {
      grow( tree, sequence(0, (int)(X.rows()), 1), {}, 0 );
    };
    // creating predictions
    // void predict();
  private:
    std::vector<int> num_cols;
    int tree_max_depth;
    int tree_min_nodesize;
    Matrix<float, int> X;
    node tree;
    int cols;
    std::vector<int> nonconst_cols;
    RngGenerator gen;
};



#endif
