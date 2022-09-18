#ifndef SAMPLING_HEADER
#define SAMPLING_HEADER

#include <vector>
#include <set>
#include "utils.h"
#include "ranges.h"

template <typename T> std::vector<T> set_to_vect(std::unordered_set<T> &x) {
  std::vector<T> result;
  result.reserve(x.size());
  for (auto it = x.begin(); it != x.end(); ) {
    result.push_back(std::move(x.extract(it++).value()));
  }
  return result;
}

template <typename T, class Generator> std::vector<T> sample_rows( const std::vector<T> &x,
                                                                   long unsigned int size,
                                                                   Generator & gen,
                                                                   const bool replace = false) {
  if( size == x.size()) {
    return x;
  }
  std::vector<T> result;
  result.reserve(size);
  if( replace ) {
    for(long unsigned int i=0; i < size; i++) {
      result.push_back( std::move(sample_int_from_set(x, gen)));
    }
    return result;
  }
  std::unordered_set<T> uniques;

  T sample;
  for(long unsigned int i=0; i< x.size(); i++) {
    sample = x[(int)(gen.yield() * x.size())];
    if( !uniques.count(sample) ) {
      uniques.insert(sample);
    }
    if( uniques.size() == size ) {
      return set_to_vect(uniques);
    }
  }
  return result;
}

template <class T, class U> int sample_int_from_set( T set, U & generator ) {
  // the -1 is absolutely necessary - if you try to access set[ set.size() ],
  // this explodes
  return set[ (int)(generator.yield() * ((float)set.size() - 1))];
}

template <typename CategoricKind, class Rng> CategoricalSet<CategoricKind> sample_distinct(
    int col,
    std::vector<CategoricKind> &x,
    std::vector<int> &view,
    Rng & generator,
    float balance = 0.5) {
  CategoricalSet<CategoricKind> result;
  auto distinct_vals = distinct( x, view );
  for( auto &val:distinct_vals ) {
    if( generator.yield() > balance) {
      result.push_back(std::move(val));
    }
  }
  result.col_id = col;
  return result;
}

template <typename T, class U> T sample( NumericRange<T> x, U & generator ) {
  return (T)((T)generator.yield() * (x.upper - x.lower)) + x.lower;
}

#endif
