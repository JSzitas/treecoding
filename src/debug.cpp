#include <Rcpp.h>
using namespace Rcpp;
#include "sampling.h"
// #include "tree_header.h"
#include "utils.h"
#include <variant>
#include "Eigen/Dense"
#include <RcppEigen.h>
using Eigen::VectorXf;
using Eigen::MatrixXf;
#include "ranges.h"
#include <iostream>
#include "stdio.h"


// [[Rcpp::plugins("cpp17")]]

// [[Rcpp::export]]
std::vector<int> debug3( Eigen::MatrixXf X, int col, float comp ) {

  auto result = split_indices_st_cond< std::vector<int>, Eigen::VectorXf, float >( X.col(col), comp );
  return result.left;
}

// [[Rcpp::export]]
std::vector<int> debug4( Eigen::MatrixXi X, int col, std::vector<int> matches ) {

  auto result = split_indices_within_set<std::vector<int>, Eigen::VectorXi, std::vector<int>>( X.col(col), matches );
  return result.left;
}

// [[Rcpp::export]]
int debug5( Eigen::MatrixXf X, Eigen::MatrixXi Y ) {

  auto res = intervals<float, int>( X, Y );
  for( int i=0; i< res.NumericIntervals.size(); i++ ) {
    res.NumericIntervals[i].print();
  }
  for( int i=0; i< res.CategoricalSets.size(); i++ ) {
    res.CategoricalSets[i].print();
  }

  return 0;
}



// RCPP_EXPOSED_CLASS(Tree);
//
// // need to export this too
// RCPP_MODULE(tree_cpp) {
//   // using namespace Rcpp;
//
//   Rcpp::class_<Tree>("Tree")
//     .constructor()
//     .method("setNumData", &Tree::setNumData, "Set numeric feature matrix")
//     // .method("square", &Double::square, "square of value")
//   ;
// }

