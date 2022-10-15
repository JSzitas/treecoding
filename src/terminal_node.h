#ifndef TERMINAL_HEADER
#define TERMINAL_HEADER

#include <vector>
#include "ranges.h"
#include "utils.h"

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
    for( long long unsigned int i=0; i < this->NumIntervals.size(); i++ ) {
      if( col == NumIntervals[i].col) {
        this->NumIntervals[i] = newbox;
        return;
      }
    }
    // if not present, add it to the set
    this->NumIntervals.push_back(newbox);
  };
  void add(CatSet<Categorical> x, int col) {
    interval_box<CatSet<Categorical>> newbox;
    newbox.interval = x;
    newbox.col = col;
    for( long long unsigned int i=0; i < this->CatSets.size(); i++ ) {
      if( col == CatSets[i].col) {
        this->CatSets[i] = newbox;
        return;
      }
    }
    // if not present, add it to the set
    this->CatSets.push_back(newbox);
  };
  void reconcile(NumInterval<Numeric> x, int col) {
    for( long long unsigned int i=0; i < this->NumIntervals.size(); i++ ) {
      if( col == NumIntervals[i].col) {
        // intersect 2 sets given by intervals using their minima and maxima
        // ie the infimum of the reconciled set must be such a value that surely
        // all values fit into it - and if this is not the case, we need to shrink the set
        // 'upwards'. the same holds for the supremum in converse - we need to shrink it 'downwards'
        this->NumIntervals[i].interval.lower = max(x.lower, this->NumIntervals[i].interval.lower);
        this->NumIntervals[i].interval.upper = min(x.upper, this->NumIntervals[i].interval.upper);
        return;
      }
    }
    // if not present, add it to the set
    interval_box<NumInterval<Numeric>> newbox;
    newbox.interval = x;
    newbox.col = col;
    NumIntervals.push_back(newbox);
  }
  void reconcile(CatSet<Categorical> x, int col) {
    for( long long unsigned int i=0; i < this->CatSets.size(); i++ ) {
      if( col == CatSets[i].col) {
        // find the values that are both in the current set and also in the
        // new set to be added via intersection - e.g. if a set A contains
        // (a,b,c) and set B containts (b,c,d), then their intersection is set C
        // (b,c) >>
        this->CatSets[i].val_set = intersect<Categorical>(this->CatSets[i].val_set, x.val_set);
        return;
      }
    }
    // if not present, add it to the set
    interval_box<NumInterval<Numeric>> newbox;
    newbox.interval = x;
    newbox.col = col;
    NumIntervals.push_back(newbox);
  }
private:
  std::vector<interval_box<NumInterval<Numeric>>> NumIntervals;
  std::vector<interval_box<CatSet<Categorical>>> CatSets;
};

#endif
