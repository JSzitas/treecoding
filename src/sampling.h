#ifndef SAMPLING_HEADER
#define SAMPLING_HEADER

#include <vector>
#include <set>
#include "utils.h"

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

template <typename T, class Generator> T runif( T a, T b, Generator & gen ) {
  return ((b - a)*gen.yield()) + a;
}

template <class T, typename U = int> T distinct( T &a ) {
  T result;
  result.reserve(a.size());
  // reserving for all uniques might be overtly pessimistic, but
  // I am not sure what the impact on performance of push_back would be
  // otherwise... if its slow I will consider a fix :)
  // result.reserve(a.size());
  // using std::set
  std::unordered_set<U> uniques;
  int current_size = 0;
  for( auto &item : a ) {
    uniques.insert(item);
    if( uniques.size() > current_size ) {
      current_size++;
      result.push_back(item);
    }
  }
  return result;
}

#endif
