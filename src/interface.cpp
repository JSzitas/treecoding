#include <Rcpp.h>
using namespace Rcpp;
#include "tree.h"
#include "ranges.h"

// [[Rcpp::plugins("cpp17")]]

// [[Rcpp::export]]
void random_tree( std::vector<float> a_vec, std::vector<int> b_vec ) {

  auto range = min_max(a_vec);

  NumericInterval<float> a = NumericInterval<float>(range, 2);
  CategoricalSet<int> b = CategoricalSet<int>( b_vec, 5 );
  a.print();
  b.print();

  // TreeDataStorage tree_data( num, cat );
  // Eigen::VectorXf res = tree_data.num_col(0);//std::get<0>(tree_data.col(0));
  // return tree_data.cols();
  // return result.left;
  // return a.col;
}


