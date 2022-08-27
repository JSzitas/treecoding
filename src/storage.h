#ifndef STORAGE_HEADER
#define STORAGE_HEADER

#include "Eigen/Dense"
#include "typedefs.h"

class TreeDataStorage{
  public:
    //constructor
    TreeDataStorage( Eigen::MatrixXf num_data,
                     Eigen::MatrixXi cat_data,
                     target_variant target = {} ) {
      target = target;
      num_cols = num_data.cols();
      cat_cols = cat_data.cols();
      num_data_mat = num_data;
      cat_data_mat = cat_data;
      ncol = num_cols + cat_cols;
      nrow = num_data.rows();
    }
    // convenient function to get number of cols
    int cols() {
      return ncol;
    }
    // ditto for rows
    int rows() {
      return nrow;
    }
  private:
    Eigen::MatrixXf num_data_mat;
    Eigen::MatrixXi cat_data_mat;
    int num_cols;
    int cat_cols;
    target_variant target;
    int ncol;
    int nrow;
};

#endif
