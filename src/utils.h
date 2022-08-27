#ifndef UTIL_HEADER
#define UTIL_HEADER

#include <vector>

template <class T> auto min_elem( T &a) {
  auto result = a[0];
  for( int i = 1; i < a.size(); i++ ) {
    if(a[i] < result) {
      result = a[i];
    }
  }
  return result;
}

template <class T > auto max_elem( T &a) {
  auto result = a[0];
  for( int i = 1; i < a.size(); i++ ) {
    if(a[i] > result) {
      result = a[i];
    }
  }
  return result;
}

template <class T, typename U = int> std::vector<U> distinct( T a ) {
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

template <typename T> std::vector<T> sequence( const T from, const T to, const T by ) {
  int size = (int)(to-from)/by;
  std::vector<T> result( size );
  result[0] = from;
  for( int i = 1; i < size; i++) {
    result[i] = result[i-1] + by;
  }
  return result;
}

template < class T, typename U > std::vector<bool> smaller_than( T x, U val ) {
  std::vector<bool> result;
  result.reserve(x.size());

  for( auto & value : val) {
    if( value < val) {
      result.push_back(true);
    }
    else{
      result.push_back(false);
    }
  }
  return result;
}

template < class T > T which( std::vector<bool> x, bool invert = false ) {
  T result;
  result.reserve(x.size());
  int total_size = 0;
  for( int i=0; i< x.size(); i++) {
    if( x[i] ) {
      result.push_back(i);
      total_size += 1;
    }
    else if( invert ) {
      result.push_back(i);
      total_size += 1;
    }
  }
  result.resize(total_size);
  return result;
}

template <class T> struct disjunct_indices {
  T left;
  T right;
};

template < class T > disjunct_indices<T> split_indices( std::vector<bool> x ) {
  T result_left, result_right;
  result_left.reserve(x.size());
  result_right.reserve(x.size());

  int left_size = 0, right_size = 0;
  for( int i=0; i< x.size(); i++) {
    if( x[i] ) {
      result_left.push_back(i);
      left_size += 1;
    }
    else {
      result_right.push_back(i);
      right_size += 1;
    }
  }
  result_left.resize(left_size);
  result_right.resize(right_size);

  disjunct_indices<T> result;
  result.left = result_left;
  result.right = result_right;

  return result;
}

template < class IndexKind,
           class VectorKind,
           typename NumCond > disjunct_indices<IndexKind> split_indices_st_cond( VectorKind x,
                                                                                 NumCond &y ) {
  IndexKind result_left, result_right;
  result_left.reserve(x.size());
  result_right.reserve(x.size());

  int left_size = 0, right_size = 0;
  for( int i=0; i< x.size(); i++) {
    if( x[i] < y ) {
      result_left.push_back(i);
      left_size += 1;
    }
    else {
      result_right.push_back(i);
      right_size += 1;
    }
  }
  result_left.resize(left_size);
  result_right.resize(right_size);

  disjunct_indices<IndexKind> result;
  result.left = result_left;
  result.right = result_right;

  return result;
}

template < class IndexKind,
           class VectorKind,
           class SetKind = VectorKind,
           typename SetType = int> disjunct_indices<IndexKind>
split_indices_within_set( VectorKind x,
                          SetKind y ) {
  disjunct_indices<IndexKind> result;
  IndexKind result_left, result_right;
  result_left.reserve(x.size());
  result_right.reserve(x.size());

  int left_size = 0, right_size = 0;
  std::unordered_set<SetType> uniques( y.begin(), y.end() );
  for(int i=0;i<x.size();i++) {
    // if set contains x[i]
    if( uniques.count(x[i]) ) {
      result_left.push_back(i);
      left_size += 1;
    }
    else {
      result_right.push_back(i);
      right_size += 1;
    }

  }
  result_left.resize(left_size);
  result_right.resize(right_size);

  result.left = result_left;
  result.right = result_right;
  return result;
}

template < class Numeric, class Categorical > auto find_column( const Numeric &x,
                                                                const Categorical &y,
                                                                const int column ) {
  if( column > x.cols() ) {
    column -= x.cols();
    return y.col(column);
  }
  else {
    return x.col(column);
  }
}

template <class VectorIndex > auto split_vector_indices( disjunct_indices<VectorIndex> reindex_indices,
                                                         VectorIndex original_indices ) {
  disjunct_indices<VectorIndex> result;
  VectorIndex result_left, result_right;
  result_left.reserve(reindex_indices.left.size());
  result_right.reserve(original_indices.right.size());

  int i = 0;
  for(; i< reindex_indices.left.size(); i++) {
    result_left.push_back( original_indices[reindex_indices.right[i]] );
  }
  i = 0;
  for(; i< reindex_indices.left.size(); i++) {
    result_left.push_back( original_indices[reindex_indices.left[i]] );
  }

  result.left = result_left;
  result.right = result_right;

  return result;
}

#endif
