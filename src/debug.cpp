#include <Rcpp.h>
using namespace Rcpp;
#include "sampling.h"
#include "utils.h"
#include "tree.h"
#include "data.h"
#include "rng.h"
#include "ranges.h"

#include "ioutils.h"

// [[Rcpp::plugins("cpp17")]]

// [[Rcpp::export]]
void debug_ranges() {
  NumericInterval<float> num_x(-0.4, 1.2);
  CategoricalSet<int> cat_x( std::vector<int>{0,2,6} );
  CategoricalSet<int> cat_x2( std::vector<int>{ 1,7,19} );
  NumericInterval<float> num_x2( 1.2, 7.2 );
  
  intervals<float, int> intv;
  intv.add(num_x, 1);
  intv.add(num_x2, 3);
  intv.add(cat_x, 7);
  intv.add(cat_x2, 8);
  
  intv.print();
}

// [[Rcpp::export]]
void tree(std::vector<std::vector<float>> num_cols,
                std::vector<std::vector<int>> cat_cols,
                int max_depth = 5,
                int min_nodesz = 30) {
  std::vector<std::vector<float>> targ(0);
  storage::DataFrame<float,int> X( num_cols, cat_cols, targ );
  recurrent rec;
  RandomSplitter<float, int> splittr{};
  Tree tree(X, rec, splittr, max_depth, min_nodesz);
  tree.fit();
  // encode
  auto res = tree.encode( X );
  for( auto &item:res ) {
    item.print();
  }
  // decode
  
  
  // print
  tree.print();

  // verify decoding results
}

// [[Rcpp::export]]
void test_splitbox( std::vector<int> x ) {
  recurrent rec;
  auto res = split_set(x, rec);
  // return res.left;
  // print_vector(res.left);
  // print_vector(res.right);
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

