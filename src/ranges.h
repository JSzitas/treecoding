#ifndef RANGE_HEADER
#define RANGE_HEADER

#include <vector>
#include <optional>
#include "Eigen/Dense"

#include <iostream>
#include <stdio.h>
#include "utils.h"

template <typename T> struct NumericRange{
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

template <typename NumericKind, typename CategoricKind > struct interval {
  std::vector<NumericInterval<NumericKind>> NumericIntervals;
  std::vector<CategoricalSet<CategoricKind>> CategoricalIntervals;

  interval(){};
  interval( Eigen::Matrix<NumericKind, Eigen::Dynamic, Eigen::Dynamic> num_data,
            Eigen::Matrix<CategoricKind, Eigen::Dynamic, Eigen::Dynamic> cat_data ) {
    NumericIntervals.reserve(num_data.cols());
    for(int i = 0; i < num_data.cols(); i++) {
      NumericIntervals.push_back(
        // this basically says - take the column, compute range, push that into
        // the numeric interval
        NumericInterval<NumericKind>( min_max(num_data.col(i)), i)
      );
    }
    CategoricalIntervals.reserve(cat_data.cols());

    int total_num_cols = num_data.cols();
    for(int i=0; i < cat_data.cols();i++) {
      CategoricalIntervals.push_back(
        // this basically says - take the column, compute range, push that into
        // the numeric interval
        CategoricalSet<CategoricKind>( distinct(cat_data.col(i)), i+total_num_cols)
      );
    }
  }
};

#endif
