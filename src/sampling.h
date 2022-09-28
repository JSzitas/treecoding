#ifndef SAMPLING_HEADER
#define SAMPLING_HEADER

#include <vector>
#include <set>
#include "utils.h"
#include "ranges.h"

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
  return set[ (int)(generator.yield() * (float)set.size())];
}

template <typename CategoricKind, class Rng> CategoricalSet<CategoricKind> sample_distinct(
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
  return result;
}

template <typename T, class U> T sample( NumericRange<T> x, U & generator ) {
  return (T)((T)generator.yield() * (x.upper - x.lower)) + x.lower;
}

template <class T> struct split_box {
  T left, right;
};

template <class T, class Generator> split_box<T> split_set( T &set,
                                                            Generator &gen,
                                                            float balance = 0.5) {
    split_box<T> result;
    auto distinct_vals = distinct( set);
    for( auto &val:distinct_vals ) {
      if( gen.yield() > balance) {
        result.left.push_back(std::move(val));
      }
      else{
        result.right.push_back(std::move(val));
      }
    }
    return result;
}

#endif
