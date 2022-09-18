#ifndef TREE_HEADER
#define TREE_HEADER

#include <variant>
#include <vector>
#include "utils.h"
#include "ranges.h"
#include "data.h"
#include "memory"

struct node {
  node(){};
  node_split<float, int> range;
  intervals<float, int> terminal_range;
  bool terminal = false;
  int node_size;
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
    }
  };
};

template <class RngGenerator, class Splitter> class Tree {
  public:
    Tree<Splitter, RngGenerator>( 
        storage::DataFrame<float, int> &data,
        RngGenerator & generator,
        Splitter & splitter,
        int max_depth = 8,
        int min_nodesize = 30) : X(data), node_splitter(splitter), 
        gen(generator), tree_max_depth(max_depth), tree_min_nodesize(min_nodesize){
      nonconst_cols = data.nonconst_cols();
    };
    node grow( std::vector<int> row_ids,
               intervals<float, int> ranges = {},
               int current_depth = 0){
      node tree;
      tree.node_size = row_ids.size();
      // the goto tag for when we found nothing to split on or a constant column
      termination_check:
        // termination
        if( row_ids.size() <= tree_min_nodesize ||
            current_depth >= tree_max_depth ||
            nonconst_cols.size() < 1) {
          std::cout << " Current depth: " << current_depth << " and nodesize: " <<
            tree.node_size << std::endl;
          tree.terminal_range = ranges;
          tree.terminal = true;
          return tree;
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
        return tree;
    };
    // void fit() {
    //   // you know how many trees at most there will be based on the max depth ->
    //   // you will always have at most 2^max_depth total nodes
    //   // because you start at depth 0 with 1 node, followed by
    //   // 2 nodes at depth 1, 4 at depth 2, 8 at depth 3...
    //   // further, you know you can can number these uniquely quite easily
    //   // thus our tree is a (linearly) laid out vector of nodes
    //   std::vector<node> tree( tree_max_depth );
    //   auto seq = sequence(X.rows());
    // 
    //   tree[0] = grow( seq );
    //   tree[0].node_id = 1;
    //   // find ids for children
    //   auto splits = X.match(tree[0].range, seq );
    //   int n_children = 2;
    //   int current_depth = 0;
    //   // each parent always has 2 children
    //   std::vector<int> parent_indices = {0};
    //   std::vector<int> child_indices = {1,2};
    // 
    //   // we can iterate through this and fill out nodes as necessary
    //   while(current_depth <= max_depth) {
    //     for( int i=0; i < n_children; i++ )
    //       // current_depth
    // 
    // 
    // 
    // 
    //     }
    //     // the number of potential children always doubles
    //     n_children *= 2;
    //     current_depth += 1;
    //   }
    //   // auto split_child_ids = X.match( split_res, col, row_ids );
    // 
    // 
    // };
    // creating predictions
    // void predict();
  private:
    int tree_max_depth;
    int tree_min_nodesize;
    storage::DataFrame<float, int> &X;
    std::vector<node> tree;
    Splitter node_splitter;
    std::vector<int> nonconst_cols;
    RngGenerator &gen;
};

#endif
