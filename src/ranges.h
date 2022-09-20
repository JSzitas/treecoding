#ifndef RANGE_HEADER
#define RANGE_HEADER

#include <vector>
#include "utils.h"

template <typename T> struct NumericRange{
  NumericRange(){};
  NumericRange(T a, T b){
    lower = a;
    upper = b;
  };
  NumericRange(std::vector<T> a) {
    NumericRange<T> rng = min_max(a);
    upper = rng.upper;
    lower = rng.lower;
  }
  T upper, lower;
};

template <class T, typename U=float> NumericRange<U> min_max( T &a) {
  NumericRange<U> result;

  result.lower = a[0];
  result.upper = a[0];

  for(int i=1; i < a.size(); i++ ) {
    if( a[i] < result.lower) {
      result.lower = a[i];
    }
    if( a[i] > result.upper) {
      result.upper = a[i];
    }
  }
  return result;
}

template <class T, typename U=float> NumericRange<U> min_max_subset( T &a,
                                                                     std::vector<int> & subset) {
  NumericRange<U> result;

  result.lower = a[subset[0]];
  result.upper = a[subset[0]];

  for( int i=1; i < subset.size(); i++ ) {
    if( a[i] < result.lower) {
      result.lower = a[subset[i]];
    }
    if( a[i] > result.upper) {
      result.upper = a[subset[i]];
    }
  }
  return result;
}

template <typename T> struct NumericInterval{
  T lower_val, upper_val;
  NumericInterval(){};
  NumericInterval(T lower, T upper) {
    lower_val = lower;
    upper_val = upper;
  };
  NumericInterval( NumericRange<T> range ){
    lower_val = range.lower;
    upper_val = range.upper;
  };
  NumericInterval( std::vector<T> vec ){
    auto range = min_max(vec);
    lower_val = range.lower;
    upper_val = range.upper;
  };
};

template <typename T> struct CategoricalSet{
  std::vector<T> set_vals;
  int col_id;
  CategoricalSet(){};
  CategoricalSet(std::vector<T> set) {
    set_vals = distinct(set);
  };
  int size(){
    return set_vals.size();
  };
  T operator[] (int i) {
    return set_vals[i];
  };
  void push_back(int x) {
    set_vals.push_back(x);
  };
  void reserve(int x) {
    set_vals.reserve(x);
  }
};

template <typename NumericKind, typename CategoricKind> struct node_split {
  node_split<NumericKind, CategoricKind>(){};
  NumericInterval<NumericKind> range;
  CategoricalSet<CategoricKind> set;
  int col;
  bool type = false;
};

template <typename NumericKind, typename CategoricKind > struct intervals {
  std::vector<NumericInterval<NumericKind>> NumericIntervals;
  std::vector<CategoricalSet<CategoricKind>> CategoricalSets;

  intervals(){};
  intervals( int num_reserved, int cat_reserved){
    NumericIntervals = std::vector<NumericInterval<NumericKind>>(num_reserved);
    CategoricalSets = std::vector<CategoricalSet<CategoricKind>>(cat_reserved);
  };
  // intervals( Eigen::Matrix<NumericKind, Eigen::Dynamic, Eigen::Dynamic> num_data,
  //            Eigen::Matrix<CategoricKind, Eigen::Dynamic, Eigen::Dynamic> cat_data ) {
  //   NumericIntervals.reserve(num_data.cols());
  //   for(int i = 0; i < num_data.cols(); i++) {
  //     NumericIntervals.push_back(
  //       // this basically says - take the column, compute range, push that into
  //       // the numeric interval
  //       NumericInterval<NumericKind>( min_max(num_data.col(i)), i)
  //     );
  //   }
  //   CategoricalSets.reserve(cat_data.cols());
  //
  //   int total_num_cols = num_data.cols();
  //   for(int i=0; i < cat_data.cols();i++) {
  //     CategoricalSets.push_back(
  //       // this basically says - take the column, compute range, push that into
  //       // the numeric interval
  //       CategoricalSet<CategoricKind>( distinct(cat_data.col(i)), i+total_num_cols)
  //     );
  //   }
  // }
  void reserve(int num_size, int cat_size) {
    NumericIntervals.reserve(num_size);
    CategoricalSets.reserve(cat_size);
  };
  void add(NumericInterval<NumericKind> x, int col) {
    NumericIntervals[col] = x;
  };
  void add(CategoricalSet<CategoricKind> x, int col) {
    CategoricalSets[col] = x;
  };
  // and a merge method (for merging multiple of these?)
};


#endif
