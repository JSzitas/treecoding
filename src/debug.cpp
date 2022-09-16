#include <Rcpp.h>
using namespace Rcpp;
#include "sampling.h"
#include "utils.h"
#include "stdio.h"
#include "iostream"
// #include "tree.h"
#include "data.h"
#include "rng.h"

// [[Rcpp::plugins("cpp17")]]

// [[Rcpp::export]]
std::vector<int> sample_rows_cpp( std::vector<int> x, int size, bool repl = false ) {
  
  recurrent gen;
  auto result = sample_rows(x, size, gen);
  return result;
}


// [[Rcpp::export]]
std::vector<int> test_set_diff( std::vector<int> x, std::vector<int> y) {
  return set_diff(x,y);
}

// template <class T> void print_vector( T& x ) {
//   for(int i=0;i<(x.size()-1);i++) {
//   std::cout << x[i] << ", ";
//   }  
//   std::cout << x[(x.size()-1)] << std::endl;
// }


// [[Rcpp::export]]
void load_data( std::vector<std::vector<float>> x, std::vector<std::vector<int>> y ) {
  storage::DataFrame<float,int> mydata( x, y );
  auto seq = sequence(10);
  auto res = mydata.geq( 10, 1, seq );
  // print_vector(res.left);
  auto rs = mydata.nonconst_cols();
  // print_vector(rs);
}



// void debug_tree(Eigen::MatrixXf Xf, std::vector<int> num_cols,
//                 int max_depth = 5,
//                 int min_nodesz = 30) {
//   recurrent rec;
//   // rec.yield();
//   // Tree<recurrent> tree;
//   Tree<recurrent> tree(Xf, num_cols, rec, max_depth, min_nodesz);
//   tree.summary();
//   tree.fit();
// }



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

