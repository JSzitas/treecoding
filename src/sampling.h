#ifndef SAMPLING_HEADER
#define SAMPLING_HEADER

#include <vector>
#include <random>
#include <algorithm>
#include <set>
#include "utils.h"

// std::vector<int> sample_indices( const int n,
//                                  const int size)
// {
//   std::vector<int> result(n);
//   for(int i=0; i<n;i++) {
//     result[i] = i;
//   }
//   std::mt19937 generator(std::random_device{}());
//
//   std::shuffle( result.begin(),
//                 result.end(),
//                 generator);
//   result.resize(size);
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

float sample_from_numeric_kinds( const std::vector<float> & a ) {
  auto min_a = min_elem(a);
  auto max_a = max_elem(a);
  std::mt19937 generator(std::random_device{}());
  std::uniform_real_distribution<float> distr(min_a, max_a);
  auto sample = distr(generator);
  return sample;
}

std::vector<int> sample_from_categoric_kinds( const std::vector<int> &a ) {
  // find distinct elements of a in any order
  auto set = distinct<std::vector<int>>(a);
  // generate a random split of distinct elements
  std::mt19937 generator(std::random_device{}());
  std::uniform_int_distribution<> distr(0, int(set.size()-1));
  int sample = distr(generator);

  std::vector<int> result;
  result.reserve(sample + 1);

  for( int i=0; i <= sample; i++ ) {
      result.push_back(set[i]);
  }

  return result;
}

template <class T> T reshuffle( T &a) {
  std::mt19937 generator(std::random_device{}());
  std::shuffle(a.begin(), a.end(), generator);
}

#endif
