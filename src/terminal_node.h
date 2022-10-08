#ifndef TERMINAL_HEADER
#define TERMINAL_HEADER

#include <vector>
#include "ranges.h"

template <typename Numeric> struct NumInterval {
  NumInterval(){};
  NumInterval<Numeric>( Numeric upper, Numeric lower ) {
    this->lower = lower;
    this->upper = upper;
  };
  void set( Numeric upper, Numeric lower ) {
    this->lower = lower;
    this->upper = upper;
  }
  Numeric upper, lower;
};

template <typename Categorical> struct CatSet {
  CatSet(){
    this->val_set = std::vector<Categorical>(0);
  };
  CatSet<Categorical>( std::vector<Categorical> set ) {
    this->val_set = set;
  }
  void set(std::vector<Categorical> set) {
    this->val_set = set;
  }
  std::vector<Categorical> val_set;
};

template <typename Numeric, typename Categorical> struct terminal_node {
  terminal_node(){
    this->NumIntervals = std::vector<interval_box<NumInterval<Numeric>>>(0);
    this->CatSets = std::vector<interval_box<CatSet<Categorical>>>(0);
  };
  void add(NumInterval<Numeric> x, int col) {
    interval_box<NumInterval<Numeric>> newbox;
    newbox.interval = x;
    newbox.col = col;
    for( long long unsigned int i=0; i < NumIntervals.size(); i++ ) {
      if( col == NumIntervals[i].col) {
        NumIntervals[i] = newbox;
        return;
      }
    }
    // if not present, add it to the set
    NumIntervals.push_back(newbox);
  };
  void add(CatSet<Categorical> x, int col) {
    interval_box<CatSet<Categorical>> newbox;
    newbox.interval = x;
    newbox.col = col;
    for( long long unsigned int i=0; i < CatSets.size(); i++ ) {
      if( col == CatSets[i].col) {
        CatSets[i] = newbox;
        return;
      }
    }
    // if not present, add it to the set
    CatSets.push_back(newbox);
  };
private:
  std::vector<interval_box<NumInterval<Numeric>>> NumIntervals;
  std::vector<interval_box<CatSet<Categorical>>> CatSets;
};

#endif
