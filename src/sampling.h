#ifndef SAMPLING_HEADER
#define SAMPLING_HEADER

#include <vector>
// #include <random>
// #include <algorithm>
#include <set>
#include "utils.h"

template <class T, class Rng> T sample_rows( T x,
                                             const int size,
                                             Rng & generator,
                                             const bool replace = false)
{
  T result;
  result.reserve(size);
  if( replace ) {
    result.reserve(size);
    for(int i=0; i < size; i++) {
      result.push_back( sample_int_from_set(x, generator));
    }
  }
  // TODO: shuffling is way too slow, figure out how to make a nicer generator
  else {
    shuffle(x, generator);
    x.resize(size);
    x = result;
  }

  return result;
}

template <class T, class Rng> T sample_rows2( T x,
                                             const int size,
                                             Rng & generator,
                                             const bool replace = false)
{
  if( size == x.size()) {
    return x;
  }
  T result;
  result.reserve(size);
  if( replace ) {
    result.reserve(size);
    for(int i=0; i < size; i++) {
      result.push_back( sample_int_from_set(x, generator));
    }
  }
  // TODO: shuffling is way too slow, figure out how to make a nicer generator
  else {
    std::unordered_set<int> hash_map;
    hash_map.reserve(size);
    int current_size = 0;
    while(true){
      auto item = sample_int_from_set(x, generator);
      if( !hash_map.count(item)) {
        result.push_back(item);
        current_size++;
      }
      hash_map.insert(item);
      if( current_size == size) {
        break;
      }
    }
  }
  return result;
}


// template <class T, class Rng> T sample_rows3( T x,
//                                               const int size,
//                                               Rng & generator,
//                                               const bool replace = false)
// {
//   T result;
//   result.reserve(size);
//   if( replace ) {
//     result.reserve(size);
//     for(int i=0; i < size; i++) {
//       result.push_back( sample_int_from_set(x, generator));
//     }
//   }
//   // TODO: shuffling is way too slow, figure out how to make a nicer generator
//   else {
//     for( int i = 0; i < size; i++) {
//       int j = (int)(x.size() * generator.yield());
//
//       result.push_back(j);
//     }
//     generator.yield();
//   }
//
//   return result;
// }



// template <class T, typename U = int> T distinct( T a ) {
//   T result;
//   result.reserve(a.size());
//   // reserving for all uniques might be overtly pessimistic, but
//   // I am not sure what the impact on performance of push_back would be
//   // otherwise... if its slow I will consider a fix :)
//   // result.reserve(a.size());
//   // using std::set
//   std::unordered_set<U> uniques;
//   int current_size = 0;
//   for( auto item : a ) {
//     uniques.insert(item);
//     if( uniques.size() > current_size ) {
//       current_size++;
//       result.push_back(item);
//     }
//   }
//   return result;
// }

// float sample_from_numeric_kinds( const std::vector<float> & a ) {
//   auto min_a = min_elem(a);
//   auto max_a = max_elem(a);
//   std::mt19937 generator(std::random_device{}());
//   std::uniform_real_distribution<float> distr(min_a, max_a);
//   auto sample = distr(generator);
//   return sample;
// }
//
// std::vector<int> sample_from_categoric_kinds( const std::vector<int> &a ) {
//   // find distinct elements of a in any order
//   auto set = distinct<std::vector<int>>(a);
//   // generate a random split of distinct elements
//   std::mt19937 generator(std::random_device{}());
//   std::uniform_int_distribution<> distr(0, int(set.size()-1));
//   int sample = distr(generator);
//
//   std::vector<int> result;
//   result.reserve(sample + 1);
//
//   for( int i=0; i <= sample; i++ ) {
//       result.push_back(set[i]);
//   }
//
//   return result;
// }

#endif
