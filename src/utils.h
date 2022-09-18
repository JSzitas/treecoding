#ifndef UTIL_HEADER
#define UTIL_HEADER

#include <vector>

template <class T, typename U = int> std::vector<U> distinct( T &a ) {
  std::vector<U> result;
  result.reserve(a.size());
  // reserving for all uniques might be overtly pessimistic, but
  // I am not sure what the impact on performance of push_back would be
  // otherwise... if its slow I will consider a fix :)
  // result.reserve(a.size());
  // using std::set
  std::unordered_set<U> uniques;
  int current_size = 0;
  for( int i=0; i< a.size(); i++) {
    uniques.insert(a[i]);
    if( uniques.size() > current_size ) {
      current_size++;
      result.push_back(a[i]);
    }
  }
  return result;
}

template <class T, typename U = int> std::vector<U> distinct( T &a, std::vector<int> &view ) {
  std::vector<U> result;
  result.reserve(a.size());
  // reserving for all uniques might be overtly pessimistic, but
  // I am not sure what the impact on performance of push_back would be
  // otherwise... if its slow I will consider a fix :)
  // result.reserve(a.size());
  // using std::set
  std::unordered_set<U> uniques;
  int current_size = 0;
  for( auto &i:view) {
    uniques.insert(a[i]);
    if( uniques.size() > current_size ) {
      current_size++;
      result.push_back(a[i]);
    }
  }
  return result;
}

std::vector<int> sequence( const int size ) {
  std::vector<int> result( size );
  result[0] = 0;
  for( int i = 1; i < size; i++) {
    result[i] = result[i-1] + 1;
  }
  return result;
}

template <typename T> bool is_same( T &a, T &b, float tol = 0.000001 ) {
  return (float)(a - b) < tol;
}

template <class Container, typename Element> bool belongs( Container x, Element y ) {
  for( int i=0; i< x.size(); i++ ) {
    if( is_same( x[i], y)) {
      return true;
    }
  }
  return false;
}
// finds item in first which are not in second >> S(A) - S(B)
template < class T, typename U = int > T set_diff( T &x, T &y ) {
  std::unordered_set<U> myset(y.begin(),y.end());
  T result;
  for( auto &item : x ) {
    if( !myset.count(item) ) {
      result.push_back(std::move(item));
    }
  }
  return result;
}

template <class T, typename U=int> T set_diff( T &x, U &y) {
  T result;
  for( auto &item : x ) {
    if( !is_same(item, y) ){
      result.push_back(std::move(item));
    }
  }
  return result;
}
// I think this should be, in theory, slower than e.g. an unordered map search
// expect for cases where we are searching over small data... which is what
// I anticipate we will be doing. Hopefully you can return to this later and
// check this assummption
template <class T> bool all_const( T &a ) {
  for( int i=1; i < a.size(); i++) {
    if( !is_same(a[i], a[0]) ) {
      return false;
    }
  }
  return true;
}

template <class T> bool all_const_view( T &a, std::vector<int> & view ) {
  for( int i=0; i< view.size();i++) {
    if( !is_same(a[view[i]], a[view[0]]) ) {
      return false;
    }
  }
  return true;
}

std::vector<int> make_pow2_indices( int power_of_2 ) {
  // these indices are always the numbers between the power of 2 (leftmost node)
  // until the first number preceding the next power of 2 (rightmost node)
  int lower = 2<<power_of_2;
  int upper = 2<<(power_of_2+1);

  std::vector<int> result;
  result.reserve(upper-lower-1);
  for(int i = lower; i <upper; i ++ ) {
    result.push_back(i);
  }
  return result;
}


#endif
