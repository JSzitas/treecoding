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
  NumericInterval(){};
  NumericInterval(T lower, T upper) {
    lower_val = lower;
    upper_val = upper;
    middle_val = (upper-lower)/2;
  };
  NumericInterval(T lower, T middle, T upper) {
    lower_val = lower;
    upper_val = upper;
    middle_val = middle;
  };
  NumericInterval( NumericRange<T> range ){
    lower_val = range.lower;
    upper_val = range.upper;
    middle_val = (range.upper-range.lower)/2;
  };
  NumericInterval( std::vector<T> vec ){
    auto range = min_max(vec);
    lower_val = range.lower;
    upper_val = range.upper;
    middle_val = (range.upper-range.lower)/2;
  };
  void print() {
    std::cout << "Range with values: " << lower_val << " to " << upper_val;
  }
  T lower_val, middle_val, upper_val;
};

template <typename T> struct CategoricalSet{
  CategoricalSet(){};
  CategoricalSet(std::vector<T> set) {
    set_vals = distinct(set);
    out_vals = std::vector<T>(0);
  };
  CategoricalSet( std::vector<T> inset, std::vector<T> outset ) {
    set_vals = inset;
    out_vals = outset;
  }
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
  void print() {
    std::cout << "Set with values: ";
    for( auto &item:set_vals ) {
      std::cout << item << ", ";
    }
  }
  std::vector<T> set_vals;
  std::vector<T> out_vals;
};

template <typename NumericKind, typename CategoricKind> struct node_split {
  node_split<NumericKind, CategoricKind>(){};
  NumericInterval<NumericKind> range;
  CategoricalSet<CategoricKind> set;
  int col;
  bool type = false;
};

template <class T> struct interval_box{
  interval_box() {
    this->interval = T();
    this->col = 0;
  }
  void print() {
    std::cout << "Interval ";
    interval.print();
    std::cout << " at column " << col << std::endl;
  }
  T interval;
  int col;
};

template <typename NumericKind, typename CategoricKind > struct intervals {
  intervals(){
    this->NumericIntervals = std::vector<interval_box<NumericInterval<NumericKind>>>(0);
    this->CategoricalSets = std::vector<interval_box<CategoricalSet<CategoricKind>>>(0);
  };
  void add(NumericInterval<NumericKind> x, int col) {
    interval_box<NumericInterval<NumericKind>> newbox;
    newbox.interval = x;
    newbox.col = col;
    for( int i=0; i < NumericIntervals.size(); i++ ) {
      if( col == NumericIntervals[i].col) {
        NumericIntervals[i] = newbox;
        return;
      }
    }
    // if not present, add it to the set
    NumericIntervals.push_back(newbox);
  };
  void add(CategoricalSet<CategoricKind> x, int col) {
    interval_box<CategoricalSet<CategoricKind>> newbox;
    newbox.interval = x;
    newbox.col = col;
    for( int i=0; i < CategoricalSets.size(); i++ ) {
      if( col == CategoricalSets[i].col) {
        CategoricalSets[i] = newbox;
        return;
      }
    }
    // if not present, add it to the set
    CategoricalSets.push_back(newbox);
  };
  void print() {
    for( int i=0; i < NumericIntervals.size(); i++ ) {
      NumericIntervals[i].print();
    }
    for( int i=0; i < CategoricalSets.size(); i++ ) {
      CategoricalSets[i].print();
    }
  }
  private:
    std::vector<interval_box<NumericInterval<NumericKind>>> NumericIntervals;
    std::vector<interval_box<CategoricalSet<CategoricKind>>> CategoricalSets;
};

#endif
