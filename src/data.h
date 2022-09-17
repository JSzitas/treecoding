#ifndef MATRIX_HEADER
#define MATRIX_HEADER

#include "vector"
#include "ranges.h"

namespace storage {

template <typename T> struct split_result {
  split_result<T>() {
    std::vector<std::vector<T>> children(2);
  };
  std::vector<T> operator [] (int index) {
    return children[index];
  }
  std::vector<std::vector<T>> children;
};

template <typename NumericKind, typename CategoricKind> class DataFrame {
  public:
    DataFrame<NumericKind, CategoricKind>() {
      num_data = std::vector<std::vector<NumericKind>>(0);
      cat_data = std::vector<std::vector<CategoricKind>>(0);
      targets = std::vector<std::vector<CategoricKind>>(0);
      num_cols = 0;
      cat_cols=0;
      ncol = 0;
      nrow = 0;
    };
    DataFrame<NumericKind, CategoricKind>( std::vector<std::vector<NumericKind>> numerics,
                                           std::vector<std::vector<CategoricKind>> categoricals,
                                           std::vector<std::vector<NumericKind>> targets
                                          ) : num_data(numerics), cat_data(categoricals), targets(targets) {
      num_cols = numerics.size();
      cat_cols = categoricals.size();
      ncol = numerics.size() + categoricals.size();
      nrow = numerics[0].size();
    }
    int cols() {
      return ncol;
    };
    int rows() {
      return nrow;
    };
    void add( std::vector<NumericKind>& x ) {
      if( x.size() > nrow || x.size() < nrow ) {
        return;
      }
      num_data.push_back(x);
      ncol++;
      num_cols++;
    };
    void add( std::vector<CategoricKind>& x ) {
      if( x.size() > nrow || x.size() < nrow ) {
        return;
      }
      cat_data.push_back(x);
      ncol++;
      cat_cols++;
    };
    void add_targets( std::vector<std::vector<NumericKind>> &x ) {
      targets = x;
    }
    split_result<int> set_match( std::vector<CategoricKind> &set, int col,
                                 std::vector<int> &subset ) {
      std::unordered_set<CategoricKind> newset;
      for( auto &val:set ) {
        newset.insert(val);
      }
      split_result<int> result;
      if( col > (cat_cols-1) ) {
        return result;
      }
      result[0].reserve(subset.size());
      result[1].reserve(subset.size());
      for( auto &index:subset ) {
        if( newset.count(cat_data[col][index]) ) {
          result[0].push_back(std::move(index));
        }
        else {
          result[1].push_back(std::move(index));
        }
      }
      return result;
    }
    split_result<int> seq( NumericKind x, int col, std::vector<int> &subset ) {
      split_result<int> result;
      if( col > (num_cols-1) ) {
        return result;
      }
      result[0].reserve(subset.size());
      result[1].reserve(subset.size());

      for( auto &index:subset ) {
        if(num_data[col][index] <= x) {
          result[0].push_back(std::move(index));
        }
        else {
          result[1].push_back(std::move(index));
        }
      }
      return result;
    }
    split_result<int> geq( NumericKind x, int col, std::vector<int> &subset ) {
      split_result<int> result;
      if( col > (num_cols-1) ) {
        return result;
      }
      result[0].reserve(subset.size());
      result[1].reserve(subset.size());
      for( auto &index:subset ) {
        if(num_data[col][index] >= x) {
          // no idea if the moves are a good idea (particularly from the pov of 
          // memory continguency), but its worth a try
          result[0].push_back(std::move(index));
        }
        else {
          result[1].push_back(std::move(index));
        }
      }
      return result;
    };
    std::vector<int> nonconst_cols( std::vector<int> &subset, std::vector<int> &view) {
      std::vector<int> result;
      result.reserve(subset.size());
      for( auto &i:subset) {
        if( i > ncol ) {}
        else if( i < num_cols ) {
            if( !all_const_view( num_data[i], view ) ) {
              result.push_back(std::move(i));
            }
        }
        else{
          if( !all_const_view( cat_data[i-num_cols], view ) ) {
            result.push_back(i);
          }
        }
      }
      return result;
    };
    std::vector<int> nonconst_cols( std::vector<int> &subset) {
      std::vector<int> result;
      result.reserve(subset.size());
      for( auto &i:subset) {
        if( i > ncol ) {}
        else if( i < num_cols ) {
          if( !all_const_view( num_data[i] ) ) {
            result.push_back(std::move(i));
          }
        }
        else{
          if( !all_const_view( cat_data[i-num_cols] ) ) {
            result.push_back(i);
          }
        }
      }
      return result;
    };
    std::vector<int> nonconst_cols() {
      std::vector<int> result;
      result.reserve(ncol);
      for(int i=0; i < ncol; i++) {
        if( i > ncol ) {}
        else if( i < num_cols ) {
          if( !all_const( num_data[i] ) ) {
            result.push_back(std::move(i));
          }
        }
        else{
          if( !all_const( cat_data[i-num_cols] ) ) {
            result.push_back(i);
          }
        }
      }
      return result;
    };
    bool col_is_const( int col, std::vector<int> &view ) {
      if( col > ncol ) {
        return false;
      }
      else if( col < num_cols ) {
        return all_const_view( num_data[col], view );
      }
      return all_const_view( cat_data[col-num_cols], view);
    }
    // there is a silly bug somewhere here
    split_result<int> match( node_split<float, int> &a, 
                            int col,
                            std::vector<int> & subset ) {
      std::vector<int> result;
      if( a.type ) {
        return seq( a.range.upper_val, col, subset );
      }
      else {
        return set_match( cat_data[col-num_cols], col, subset );
      }
    };
  std::vector<std::vector<NumericKind>> num_data;
  std::vector<std::vector<CategoricKind>> cat_data;
  std::vector<std::vector<NumericKind>> targets;
  int nrow;
  int ncol;
  int num_cols;
  int cat_cols;
};
}

#endif
