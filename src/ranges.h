#ifndef RANGE_HEADER
#define RANGE_HEADER

#include <vector>
// #include <optional>
#include "Eigen/Dense"

// #include <iostream>
// #include <stdio.h>
#include "utils.h"

// what if this is rather a tagged union, or maybe an Eigen::MatrixXf which is
// handled according to a flag?
#include <variant>

typedef std::variant<Eigen::VectorXf,
                     Eigen::VectorXi,
                     Eigen::MatrixXf,
                     Eigen::MatrixXi,
                     std::monostate> target_variant;

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

template <class T, typename U=float> NumericRange<U> min_max( T a) {
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

template <typename T> struct NumericInterval{
  T lower_val, upper_val;
  int col_id;
  NumericInterval(){};
  NumericInterval(T lower, T upper, int col) {
    col_id = col;
    lower_val = lower;
    upper_val = upper;
  };
  NumericInterval( NumericRange<T> range, int col ){
    lower_val = range.lower;
    upper_val = range.upper;
    col_id = col;
  };
  void print() {
    std::cout << "Column: " << col_id << " with range " << lower_val << " to " << upper_val << "\n";
  };
};

template <typename T> struct CategoricalSet{
  std::vector<T> set_vals;
  int col_id;
  CategoricalSet(){};
  CategoricalSet(std::vector<T> set, int col) {
    col_id = col;
    set_vals = set;
  };
  void print() {
    std::cout << "Column: " << col_id << " with items: ";
    for(auto &item: set_vals){
      std::cout << " " << item << " ";
    }
    std::cout << "\n";
  };
};

template <typename NumericKind, typename CategoricKind > struct intervals {
  std::vector<NumericInterval<NumericKind>> NumericIntervals;
  std::vector<CategoricalSet<CategoricKind>> CategoricalSets;

  intervals(){};
  intervals( Eigen::Matrix<NumericKind, Eigen::Dynamic, Eigen::Dynamic> num_data,
             Eigen::Matrix<CategoricKind, Eigen::Dynamic, Eigen::Dynamic> cat_data ) {
    NumericIntervals.reserve(num_data.cols());
    for(int i = 0; i < num_data.cols(); i++) {
      NumericIntervals.push_back(
        // this basically says - take the column, compute range, push that into
        // the numeric interval
        NumericInterval<NumericKind>( min_max(num_data.col(i)), i)
      );
    }
    CategoricalSets.reserve(cat_data.cols());

    int total_num_cols = num_data.cols();
    for(int i=0; i < cat_data.cols();i++) {
      CategoricalSets.push_back(
        // this basically says - take the column, compute range, push that into
        // the numeric interval
        CategoricalSet<CategoricKind>( distinct(cat_data.col(i)), i+total_num_cols)
      );
    }
  }
  void reserve(int num_size, int cat_size) {
    NumericIntervals.reserve(num_size);
    CategoricalSets.reserve(cat_size);
  };
  // maybe an update method
  void add(NumericInterval<NumericKind> x) {
    NumericIntervals.push_back(x);
  };
  void add(CategoricalSet<CategoricKind> x) {
    CategoricalSets.push_back(x);
  };
  // and a merge method (for merging multiple of these?)
};

template <typename NumericKind, typename CategoricKind> struct node_split {
  union variant {
    NumericInterval<NumericKind> range;
    CategoricalSet<CategoricKind> set;
  };
  bool type = false;
};

template <typename T, class U> T sample( NumericRange<T> x, U & generator ) {
  return (T)((T)generator.yield() * (x.upper - x.lower)) + x.lower;
}

template <typename T> void swap(T &a, T &b) {
  a = a+b;
  b = a-b;
  a = a-b;
}

template <class T, class U> void shuffle( T &a, U & generator ) {
  int b;
  for( int i=0; i< a.size(); i++ ) {
      // find something to swap with
      b = sample_int_from_set( a, generator );
      // call swap
      swap(a[i], b);
  }
}


template <typename T, class U> CategoricalSet<T> sample( CategoricalSet<T> x, U & generator) {
  CategoricalSet<T> result;
  auto cutoff = (T)generator.yield();
  result.reserve(cutoff);

  // for( int=0; i<cutoff; i++ ) {
  //
  // }



  return result;
}


// template <typename T, typename U> split< T,U > make_split() {
//
// }



#endif
