#include <Rcpp.h>
using namespace Rcpp;
#include "sampling.h"
#include "utils.h"
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

// [[Rcpp::export]]
void load_data( int x ) {
  storage::DataFrame<float,int> mydata;
  // mydata.summary();
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

