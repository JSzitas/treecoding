#ifndef FOREST_HEADER
#define FOREST_HEADER

#include "tree.h"
#include "data.h"
#include <vector>
#include "utils.h"

// struct forest_encoded {
//   forest_encoded(int i) {
//     this->encoded = std::vector<std::vector<encoded>>(i);
//   }
//   void add(std::vector<encoded> x, int i) {
//     this->encoded[i] = x;
//   }
//   std::vector<std::vector<encoded>> encoded;
// };

template <class RngGenerator, class Splitter,
          typename Numerical, typename Categorical> class Forest {
  Forest( storage::DataFrame<Numerical, Categorical> data,
          RngGenerator & generator,
          Splitter & splitter,
          long long unsigned int subsample_size,
          long long unsigned int max_depth = 4,
          long long unsigned int min_nodesize = 100,
          long long unsigned int num_trees = 100 ) : X(data), gen(generator),
          node_splitter(splitter), subsample_size(subsample_size),
          tree_max_depth(max_depth), tree_min_nodesize(min_nodesize),
          forest_num_trees(num_trees) {
    this->forest = std::vector<Tree<RngGenerator,Splitter>>(forest_num_trees);
    for(unsigned long long int i=0; i<this->forest_num_trees; i++) {
      auto rows = sample_rows( sequence(0, X.rows(), 1), this->subsample_size);
      this->forest[i] = Tree( this->X, this->gen, this->node_splitter, rows,
                        this->tree_max_depth, this->tree_min_nodesize);
    }
  }
  void grow(){
    for(unsigned long long int i=0; i<this->forest_num_trees; i++) {
      this->forest[i].grow();
    }
  }
  std::vector<std::vector<encoded>> encode( storage::DataFrame<float, int> &newx ) {
    std::vector<std::vector<encoded>> result(this->forest_num_trees);
    for( unsigned long long int i=0; i < this->forest_num_trees; i++ ) {
      result[i] = this->forest[i].encode(newx);
    }
    return result;
  }
  storage::DataFrame<float, int> decode( std::vector<std::vector<encoded>> x ) {

    // get decoded values from first tree - then reconcile these with the values
    // from the second tree

    // for( auto &tree:forest ) {
    //
    // }

    // std::vector<std::vector<float>> numerics;
    // std::vector<std::vector<int>> categoricals;
    // std::vector<std::vector<float>> targets;

    // storage::DataFrame<float, int> result(  );



  }
  private:
    storage::DataFrame<Numerical, Categorical> X;
    RngGenerator &gen;
    Splitter &node_splitter;
    long long unsigned int subsample_size;
    long long unsigned int tree_max_depth;
    long long unsigned int tree_min_nodesize;
    long long unsigned int forest_num_trees;
    std::vector<Tree<RngGenerator,Splitter>> forest;
};

#endif
