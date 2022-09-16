#ifndef MATRIX_HEADER
#define MATRIX_HEADER

#include "vector"

namespace storage {

template <typename T> struct split_result {
  split_result<T>() {
    std::vector<T> left(0);
    std::vector<T> right(0);
  };
  std::vector<T> left;
  std::vector<T> right;
};

template <typename NumericKind, typename CategoricKind> class DataFrame {
  public:
    DataFrame<NumericKind, CategoricKind>() {
      num_data = std::vector<std::vector<NumericKind>>(0);
      cat_data = std::vector<std::vector<CategoricKind>>(0);
      num_cols = 0;
      cat_cols=0;
      initialized = false;
    };
    
    // add constructor from Rcpp DataFrame and maybe 
    // from std::vectors (ie only moves)
    // void init( std::vector<NumericKind> x ) {
    //   if(initialized) {
    //     return;
    //   }
    //   num_data.push_back(x);
    //   ncol++;
    //   nrow = x.size();
    //   num_cols++;
    // }
    // void init( std::vector<CategoricKind> x ) {
    //   if(initialized) {
    //     return;
    //   }
    //   cat_data.push_back(x);
    //   ncol++;
    //   nrow = x.size();
    //   cat_cols++;
    // };
    int cols() {
      return ncol;
    }
    int rows() {
      return nrow;
    }
    void add( std::vector<NumericKind> x ) {
      if( x.size() > nrow || x.size() < nrow ) {
        return;
      }
      num_data.push_back(x);
      ncol++;
      num_cols++;
    }
    void add( std::vector<CategoricKind> x ) {
      if( x.size() > nrow || x.size() < nrow ) {
        return;
      }
      cat_data.push_back(x);
      ncol++;
      cat_cols++;
    }
    split_result<int> match( std::unordered_set<CategoricKind> set, int col, std::vector<int> subset ) {
      split_result<int> result;
      if( col < num_cols ) {
        return result;
      }
      result.left.reserve(subset.size());
      result.right.reserve(subset.size());
      for( auto &index:subset ) {
        if( set.count(cat_data[col][index]) ) {
          result.left.push_back(index);
        }
        else {
          result.right.push_back(index);
        }
      }
      return result;
    }
    split_result<int> seq( NumericKind x, int col, std::vector<int> subset ) {
      split_result<int> result;
      if( col > num_cols ) {
        return result;
      }
      result.left.reserve(subset.size());
      result.right.reserve(subset.size());

      for( auto &index:subset ) {
        if(num_data[col][index] <= x) {
          result.left.push_back(index);
        }
        else {
          result.right.push_back(index);
        }
      }
      return result;
    }
    split_result<int> geq( NumericKind x, int col, std::vector<int> subset ) {
      split_result<int> result;
      if( col > num_cols ) {
        return result;
      }
      result.left.reserve(subset.size());
      result.right.reserve(subset.size());

      for( auto &index:subset ) {
        if(num_data[col][index] >= x) {
          result.left.push_back(index);
        }
        else {
          result.right.push_back(index);
        }
      }
      return result;
    }
private:
  std::vector<std::vector<NumericKind>> num_data;
  std::vector<std::vector<CategoricKind>> cat_data;
  bool initialized;
  int nrow;
  int ncol;
  int num_cols;
  int cat_cols;
};
}

#endif
