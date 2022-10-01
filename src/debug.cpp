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
  // intv.print();
}

void debug_boxed_ranges() {
  terminal_node<float,int> mynode;

  NumInterval<float> num_x(-0.4, 1.2);
  CatSet<int> cat_x( std::vector<int>{0,2,6} );
  CatSet<int> cat_x2( std::vector<int>{ 1,7,19} );
  NumInterval<float> num_x2( 1.2, 7.2 );

  mynode.add(num_x, 1);
  mynode.add(num_x2, 3);
  mynode.add(cat_x, 7);
  mynode.add(cat_x2, 8);
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
  // auto res = tree.encode( X );
  // for( auto &item:res ) {
    // item.print();
  // }
  // decode
  // tree.decode(res);
  // for(auto &range:decoded_ranges) {
  //   print_vector(range);
  // }
  // auto directions = tree.decoding_directions(decoded_ranges);
  // for(auto &range:directions) {
  //   print_vector(range);
  // }

  // print
  // tree.print();

  // verify decoding results
}

// typedef Tree< recurrent, RandomSplitter<float,int> > QrnRandomTree;

class RandomTreeQRN {
public:
  RandomTreeQRN( std::vector<std::vector<float>> num_cols,
                 std::vector<std::vector<int>> cat_cols,
                 int max_depth = 5,
                 int min_nodesz = 30) {
    std::vector<std::vector<float>> targ(0);
    storage::DataFrame<float,int> X( num_cols, cat_cols, targ );
    RandomSplitter<float, int> splittr{};
    recurrent gen;
    tree = new Tree<recurrent, RandomSplitter<float,int>>(X, gen, splittr, max_depth, min_nodesz);
  }
  ~RandomTreeQRN(){
    delete tree;
  }
  void fit(){ tree->fit(); }
private:
  Tree<recurrent, RandomSplitter<float,int>> *tree;
};


RCPP_EXPOSED_CLASS_NODECL(RandomTreeQRN);
RCPP_MODULE(RandomTreeQRN) {
  // using namespace Rcpp;

  Rcpp::class_<RandomTreeQRN>("RandomTreeQRN")
    .constructor<std::vector<std::vector<float>>,
                 std::vector<std::vector<int>>,
                 int, int>( "basic contructor" )
    .method("fit", &RandomTreeQRN::fit, "fit tree");
}

