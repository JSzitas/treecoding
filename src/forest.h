#ifndef FOREST_HEADER
#define FOREST_HEADER

#include "tree.h"
#include "data.h"
#include <vector>
#include "utils.h"

template <class RngGenerator, class Splitter,
          typename Numerical, typename Categorical> class Forest {
  Forest( storage::DataFrame<Numerical, Categorical> data,
          RngGenerator & generator,
          Splitter & splitter,
          int max_depth = 4,
          int min_nodesize = 100,
          int num_trees = 100 ) : X(data), gen(generator), node_splitter(splitter),
          tree_max_depth(max_depth), tree_min_nodesize(min_nodesize),
          forest_num_trees(num_trees){};
  void grow(){

    std::vector<Tree<RngGenerator,Splitter>> forest(forest_num_trees);
    for(int i=0; i<this->forest_num_trees; i++) {
      // auto rows = sample_rows( sequence(0, data.rows(), 1), 10);
      // Tree( num_data, cat_data );
    }

  };
  private:
    storage::DataFrame<Numerical, Categorical> X;
    // std::vector<Tree<RngGenerator>> trees;
    RngGenerator &gen;
    Splitter &node_splitter;
    long long unsigned int tree_max_depth;
    long long unsigned int tree_min_nodesize;
    long long unsigned int forest_num_trees;
    std::vector<Tree<RngGenerator,Splitter>> forest;
};

#endif
