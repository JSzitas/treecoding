#ifndef MATRIX_HEADER
#define MATRIX_HEADER

#include "vector"
#include "ranges.h"

namespace storage {

template <typename T> struct split_result {
  split_result<T> () {
    this->left = std::vector<T>(0);
    this->right = std::vector<T>(0);
  }
  std::vector<T> left;
  std::vector<T> right;
};

// add a sort of 'tagged union' subview over the DataFrame (to avoid having to work with variants)

template <typename NumericKind, typename CategoricKind, typename TargetKind=float> class DataFrame {
  public:
    DataFrame<NumericKind, CategoricKind, TargetKind>() {
      num_data = std::vector<std::vector<NumericKind>>(0);
      cat_data = std::vector<std::vector<CategoricKind>>(0);
      targets = std::vector<std::vector<TargetKind>>(0);
      num_cols = 0;
      cat_cols=0;
      ncol = 0;
      nrow = 0;
    };
    DataFrame<NumericKind, CategoricKind, TargetKind>( std::vector<std::vector<NumericKind>> numerics,
                                           std::vector<std::vector<CategoricKind>> categoricals,
                                           std::vector<std::vector<TargetKind>> targets
                                          ) : num_data(numerics), cat_data(categoricals), targets(targets) {
      num_cols = numerics.size();
      cat_cols = categoricals.size();
      target_cols = targets.size();
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
    split_result<int> set_match( CategoricalSet<CategoricKind> &set,
                                 int col,
                                 std::vector<int> &subset ) {
      std::unordered_set<CategoricKind> newset;
      for( auto &val:set.set_vals ) {
        newset.insert(val);
      }
      split_result<int> result;
      if( col > (cat_cols-1) ) {
        return result;
      }
      result.left.reserve(subset.size());
      result.right.reserve(subset.size());
      for( auto &index:subset ) {
        if( newset.count(cat_data[col][index]) ) {
          result.left.push_back(std::move(index));
        }
        else {
          result.right.push_back(std::move(index));
        }
      }
      return result;
    }
    split_result<int> seq( NumericKind x, int col, std::vector<int> &subset ) {
      split_result<int> result;
      if( col > (num_cols-1) ) {
        return result;
      }
      result.left.reserve(subset.size());
      result.right.reserve(subset.size());

      for( auto &index:subset ) {
        if(num_data[col][index] <= x) {
          result.left.push_back(std::move(index));
        }
        else {
          result.right.push_back(std::move(index));
        }
      }
      return result;
    }
    split_result<int> geq( NumericKind x, int col, std::vector<int> &subset ) {
      split_result<int> result;
      if( col > (num_cols-1) ) {
        return result;
      }
      result.left.reserve(subset.size());
      result.right.reserve(subset.size());
      for( auto &index:subset ) {
        if(num_data[col][index] >= x) {
          // no idea if the moves are a good idea (particularly from the pov of
          // memory continguency), but its worth a try
          result.left.push_back(std::move(index));
        }
        else {
          result.right.push_back(std::move(index));
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
        if( i < num_cols ) {
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
    split_result<int> match( node_split<float, int> &x,
                            std::vector<int> & subset ) {
      if( x.type ) {
        return seq( x.range.upper_val, x.range.col_id, subset );
      }
      else {
        return set_match( x.set, x.set.col_id - num_cols, subset );
      }
    };
  std::vector<std::vector<NumericKind>> num_data;
  std::vector<std::vector<CategoricKind>> cat_data;
  std::vector<std::vector<TargetKind>> targets;
  int nrow;
  int ncol;
  int num_cols;
  int cat_cols;
  int target_cols;
};
}

#endif
