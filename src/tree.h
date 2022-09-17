#ifndef TREE_HEADER
#define TREE_HEADER

#include <variant>
#include <vector>
#include "utils.h"
#include "ranges.h"
// #include "iostream"
// #include "stdio.h"
#include "data.h"
#include "memory"

struct node {
  node(){};
  node_split<float, int> range;
  bool terminal = false;
  intervals<float, int> terminal_range;
  int current_depth;
  int node_size;
  std::vector<node> children;
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
    }
  };
};

template <class RngGenerator, class Splitter>
class Tree {
  public:
    Tree<RngGenerator, Splitter>( 
          storage::DataFrame<float, int> &data,
          RngGenerator & generator,
          Splitter & splitter,
          int max_depth = 8,
          int min_nodesize = 30) : X(data), splitter(splitter),
          tree_max_depth(max_depth), tree_min_nodesize(min_nodesize), 
          gen(generator){
      tree = node();
      nonconst_cols = data.nonconst_cols();
    };
    void grow( node &tree,
               std::vector<int> row_ids,
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
        tree.terminal = true;
        return;
      }
      // First we take the data and generate a candidate split -
      // for now we will only use random trees, we can try to extend this
      // later
      // generate column over which we will split
      int col = sample_int_from_set( nonconst_cols, gen );
      // // check that the column is not all const
      // // and if it is probably just update the nonconst_cols
      // // and GOTO before this happened. which I admit is ugly, but this is
      // // probably the one reasonable case where you want to do that.
      if( X.col_is_const( col, row_ids )) {
        nonconst_cols = set_diff(nonconst_cols, col);
        goto termination_check;
      }
      // declare split
      auto split_res = splitter(col, X, row_ids, gen);
      tree.range = split_res;
      // ranges.add(tree.range, col);
      // determine where row ids go
      // std::cout << col << std::endl;
      // auto split_child_ids = X.match( split_res, col, row_ids );
      // allocate child nodes
      // tree.children = std::vector<node>(tree_arity, node());
      return;
      // for( int i= 0; i < tree_arity; i++) {
      //   // tree.children.push_back(node());
      //   grow(tree.children[i], split_child_ids[i], ranges, current_depth+1);
      // }
    };
    void fit() {
      auto seq = sequence(0, (int)(X.rows()), 1);
      grow( tree, seq, {}, 0, 2 );
    };
    // creating predictions
    // void predict();
  // private:
    int tree_max_depth;
    int tree_min_nodesize;
    storage::DataFrame<float, int> &X;
    node tree;
    Splitter splitter;
    std::vector<int> nonconst_cols;
    RngGenerator &gen;
};



#endif
