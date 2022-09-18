#include <Rcpp.h>
using namespace Rcpp;
#include "sampling.h"
#include "utils.h"
// #include "stdio.h"
// #include "iostream"
#include "tree.h"
#include "data.h"
#include "rng.h"

// [[Rcpp::plugins("cpp17")]]

// [[Rcpp::export]]
void debug_tree(std::vector<std::vector<float>> num_cols,
                std::vector<std::vector<int>> cat_cols,
                int max_depth = 5,
                int min_nodesz = 30) {
  std::vector<std::vector<float>> targ(0);
  storage::DataFrame<float,int> X( num_cols, cat_cols, targ );
  recurrent rec;
  RandomSplitter<float, int> splittr{};
  Tree tree(X, rec, splittr, max_depth, min_nodesz);
  tree.fit();
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

