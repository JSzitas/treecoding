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
  int current_depth;
  int node_size;
  std::vector<node> children;
};

template <typename Numeric, typename Categorical, class RnGenerator> struct RandomSplitter{
  RandomSplitter<Numeric, Categorical, RnGenerator>(RnGenerator & generator) : gen(generator) {;}
  node_split<Numeric, Categorical> yield(std::vector<Numeric> &x,
                                         std::vector<int> &subset) {
    node_split<Numeric, Categorical> result;
    auto data = min_max_subset(x, subset);
    result.range = std::move(NumericRange<Numeric>( data.lower, sample(data, gen)));
    result.type = false;
    return result;
  };
  node_split<Numeric, Categorical> yield(std::vector<Categorical> &x,
                                         std::vector<int> &subset) {
    node_split<Numeric, Categorical> result;
    result.set = std::move(sample_distinct(x, subset, gen));
    result.type = true;
    return result;
  };
  node_split<Numeric, Categorical> operator () (int col,
                                                storage::DataFrame<Numeric, Categorical> &data,
                                                std::vector<int> &subset) {
    if( col > data.num_cols) {
      return yield( data.cat_data[col - data.num_cols], subset );
    }
    else {
      return yield(data.num_cols[col], subset);
    }
  };
  RnGenerator &gen;
};

template <class RngGenerator, class Splitter>
class Tree {
  public:
    Tree<RngGenerator>( 
          storage::DataFrame<float, int> &data,
          std::vector<int> numeric_cols,
          RngGenerator & generator,
          Splitter & splitter,
          int max_depth = 8,
          int min_nodesize = 30) : X(data), splitter(splitter) {
      tree_max_depth = max_depth;
      tree_min_nodesize = min_nodesize;
      gen = generator;
      cols = data.cols();
      tree = node();
      nonconst_cols = data.nonconst_cols();
      num_cols = numeric_cols;
    };
    void grow( node &tree,
               std::vector<int> &row_ids,
               intervals<float, int> ranges = {},
               int current_depth = 0,
               int tree_arity = 2){
      tree.node_size = row_ids.size();
      // the goto tag for when we found nothing to split on or a constant column
      termination_check:
      // termination
      if( row_ids.size() <= tree_min_nodesize ||
          current_depth >= tree_max_depth ||
          nonconst_cols.size() < 1) {
        tree.terminal_range = ranges;
        tree.node_size = row_ids.size();
        tree.terminal = true;
        return;
      }
      // First we take the data and generate a candidate split -
      // for now we will only use random trees, we can try to extend this
      // later
      // generate column over which we will split
      int col = sample_int_from_set( X.nonconst_cols(nonconst_cols, row_ids), gen );
      // // check that the column is not all const
      // // and if it is probably just update the nonconst_cols
      // // and GOTO before this happened. which I admit is ugly, but this is
      // // probably the one reasonable case where you want to do that.
      if( X.col_is_const( col, row_ids )) {
        nonconst_cols = set_diff(nonconst_cols, col);
        goto termination_check;
      }
      // declare split
      auto split = splitter(col, X, &row_ids);
      tree.range = std::move( split );
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
    storage::DataFrame<float, int> &X;
    node tree;
    Splitter splitter;
    int cols;
    std::vector<int> nonconst_cols;
    RngGenerator &gen;
};



#endif
