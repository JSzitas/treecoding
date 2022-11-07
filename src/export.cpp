#include <Rcpp.h>
using namespace Rcpp;
#include "tree.h"
#include "data.h"
#include "rng.h"
#include "utils.h"

// [[Rcpp::plugins("cpp17")]]

class RandomTreeQRN {
public:
  RandomTreeQRN( std::vector<std::vector<float>> num_cols,
                 std::vector<std::vector<int>> cat_cols,
                 int max_depth = 5,
                 int min_nodesz = 30) {
    std::vector<std::vector<float>> targ(0);
    // assign so reference does not dangle after escaping scope of constructor
    this->X = storage::DataFrame<float,int>( num_cols, cat_cols, targ );
    RandomSplitter<float, int> splittr{};
    recurrent gen;
    auto size = this->X.rows();
    auto seq = sequence(size);
    this->tree = new Tree<recurrent, RandomSplitter<float,int>>(this->X, gen,
                                                                splittr, seq,
                                                                max_depth, min_nodesz);
  }
  ~RandomTreeQRN(){
    if( tree != nullptr ) {
      delete tree;
    }
  }
  void fit(){ tree->fit(); }
  Rcpp::DataFrame encode( std::vector<std::vector<float>> num_cols,
                          std::vector<std::vector<int>> cat_cols ){
    std::vector<std::vector<float>> targ(0);
    // assign so reference does not dangle after escaping scope of constructor
    auto newX = storage::DataFrame<float,int>( num_cols, cat_cols, targ );
    auto encoding_res = tree->encode( newX );

    std::vector<int> obs_ids(encoding_res.size());
    std::vector<int> tree_ids(encoding_res.size());
    int p = 0;
    for( auto &elem:encoding_res) {
      obs_ids[p] = elem.observation_id;
      tree_ids[p] = elem.node_id;
      p++;
    }

    Rcpp::List output_list(2);
    output_list[0] = obs_ids;
    output_list[1] = tree_ids;
    Rcpp::CharacterVector col_names(2);
    col_names[0] = "observation_id";
    col_names[1] = "node_id";
    output_list.attr("names") = col_names;

    Rcpp::DataFrame final(output_list);
    return final;
  }
  Rcpp::DataFrame decode( Rcpp::DataFrame ) {
    // struct decoded {
    //   decoded(){
    //     this->observation_ids = std::vector<int>(0);
    //     this->decoded_values = terminal_node<float, int>();
    //   };
    //   terminal_node<float, int> decoded_values;
    //   std::vector<int> observation_ids;
    // };
    Rcpp::DataFrame final(1);
    return final;
  }
private:
  Tree<recurrent, RandomSplitter<float,int>> *tree;
  // note that this is only necessary here because otherwise the data created in
  // the constructor gets passed as a reference, and after escaping the scope of the
  // constructor, the reference dangles, while the data gets destructed.
  // thus we can remove this in e.g. forest where the forest will have the responsibility
  // of storing the data and passing a reference will be safe
  storage::DataFrame<float,int> X;
};

RCPP_EXPOSED_CLASS_NODECL(RandomTreeQRN)
RCPP_MODULE(RandomTreeQRN) {
  // using namespace Rcpp;

  Rcpp::class_<RandomTreeQRN>("RandomTreeQRN")
  .constructor<std::vector<std::vector<float>>,
               std::vector<std::vector<int>>,
               int, int>( "basic contructor" )
  .method("fit", &RandomTreeQRN::fit, "fit tree")
  .method("encode", &RandomTreeQRN::encode, "encode new values");
  // .method("decode", &RandomTreeQRN::decode, "decode encoded values");
}
