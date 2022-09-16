#ifndef FOREST_HEADER
#define FOREST_HEADER

#include "tree.h"
#include "data.h"
#include <vector>
#include <utils.h>

template <class RngGenerator, typename Numerical, typename Categorical> class Forest {
  Forest(){
    
  };
  Forest<Numerical, Categorical, RngGenerator>( storage::DataFrame<Numerical, Categorical> &data,
                                                RngGenerator & generator ) {
    
  };
  void grow(){
    for(int i=0; i<num_trees; i++) {
      auto rows = sample_rows( sequence(0, data.rows(), 1), 10);
      // Tree( num_data, cat_data );
    }    
    
  };
  void print(){};
  
  
  
  private: 
    int num_trees;
    storage::DataFrame<Numerical, Categorical> data;
    std::vector<Tree<RngGenerator>> trees;
    RngGenerator generator;
};

#endif
