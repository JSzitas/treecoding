#ifndef FOREST_HEADER
#define FOREST_HEADER

#include "tree.h"
#include <vector>

template <class RngGenerator> class Forest {
  Forest(){
    generator = RngGenerator();
    
    
  };
  ~Forest(){};
  void grow(){};
  void print(){};
  
  
  
  private: 
    int num_trees;
    std::vector<Tree<RngGenerator>> trees;
    RngGenerator generator;
    Eigen::MatrixXf num_data;
    Eigen::MatrixXi col_data;
};

#endif
