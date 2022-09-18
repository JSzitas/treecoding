#include <Rcpp.h>
using namespace Rcpp;
#include "sampling.h"
#include "utils.h"
#include "stdio.h"
#include "iostream"
#include "tree.h"
#include "data.h"
#include "rng.h"

// [[Rcpp::plugins("cpp17")]]

// template <class T> void print_vector( T& x ) {
//   for(int i=0;i<(x.size()-1);i++) {
//   std::cout << x[i] << ", ";
//   }
//   std::cout << x[(x.size()-1)] << std::endl;
// }


// void load_data( std::vector<std::vector<float>> &x, std::vector<std::vector<int>> &y ) {
//   std::vector<std::vector<float>> targ(0);
//   storage::DataFrame<float,int> mydata( x, y, targ );
//   auto seq = sequence(10);
//   auto res = mydata.geq( 10, 1, seq );
//   // print_vector(res.left);
//   auto temp = sequence(0, mydata.cols(), 1);
//   // print_vector(temp);
//   auto rs = mydata.nonconst_cols(temp, seq);
//   // print_vector(rs);
// }


// [[Rcpp::export]]
void debug_tree(std::vector<std::vector<float>> num_cols,
                std::vector<std::vector<int>> cat_cols,
                int max_depth = 2,
                int min_nodesz = 30) {
  std::vector<std::vector<float>> targ(0);
  storage::DataFrame<float,int> X( num_cols, cat_cols, targ );
  recurrent rec;
  RandomSplitter<float, int> splittr{};
  Tree tree(X, rec, splittr, max_depth, min_nodesz);
  tree.fit2();
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

