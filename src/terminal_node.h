#ifndef TERMINAL_HEADER
#define TERMINAL_HEADER

#include "ranges.h"

template <typename Numeric> struct NumInterval {
  NumInterval(){};
  NumInterval<Numeric>( Numeric upper, Numeric lower ) : upper(upper), lower(lower){};
  Numeric upper, lower;
};

template <typename Categorical> struct CatSet {
  CatSet(){};
  CatSet<Categorical>( std::vector<Categorical> set ) : set(set) {};
  std::vector<Categorical> set;
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
    for( int i=0; i < NumIntervals.size(); i++ ) {
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
    for( int i=0; i < CatSets.size(); i++ ) {
      if( col == CatSets[i].col) {
        CatSets[i] = newbox;
        return;
      }
    }
    // if not present, add it to the set
    CatSets.push_back(newbox);
  };
  void print() {
    for( int i=0; i < NumIntervals.size(); i++ ) {
      NumIntervals[i].print();
    }
    for( int i=0; i < CatSets.size(); i++ ) {
      CatSets[i].print();
    }
  }
private:
  std::vector<interval_box<NumInterval<Numeric>>> NumIntervals;
  std::vector<interval_box<CatSet<Categorical>>> CatSets;
};

#endif
