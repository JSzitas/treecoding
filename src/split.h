
#include "ranges.h"

struct Splitter {
  virtual node_split<float, int> split(){};
};

struct NumericSplit : Splitter {
  node_split<float, int> split( Eigen::MatrixXf X, int col ) {

     node_split<float, int> result;

     auto range = min_max( X.col(col));


     return result;
  }
};

struct CategoricSplit : Splitter {
  node_split<float, int> split( Eigen::MatrixXi X, int col ) {

    node_split<float, int> result;

    // X.col(col)




    return result;
  }
};

