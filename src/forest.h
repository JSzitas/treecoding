#ifndef FOREST_HEADER
#define FOREST_HEADER

#include "tree.h"
#include <vector>

template <class RngGenerator> class Forest {
  Forest(){
    generator = RngGenerator();
    
    
  };
  ~Forest(){};
  void grow(){
    for(int i=0; i<num_trees, i++) {
      rows = sample_rows( data, sample_rate);
      // Tree( num_data, cat_data );
    }    
    
  };
  void print(){};
  
  
  
  private: 
    int num_trees;
    std::vector<Tree<RngGenerator>> trees;
    RngGenerator generator;
};

#endif
