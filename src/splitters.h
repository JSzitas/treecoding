#ifndef SPLITTER_HEADER
#define SPLITTER_HEADER

#include <vector>
#include "utils.h"
#include "ranges.h"
#include "data.h"
#include "terminal_node.h"
#include "sampling.h"


template <typename Numeric, typename Categorical> struct RandomSplitter{
  RandomSplitter<Numeric, Categorical>(){};
  template <class RnGenerator> node_split<Numeric, Categorical> yield(
      int col,
      std::vector<Numeric> &x,
      std::vector<int> &subset,
      RnGenerator & generator) {
    node_split<Numeric, Categorical> result;
    auto data = min_max_subset(x, subset);
    auto middle = sample(data, generator);
    result.range = NumericInterval(data.lower, middle, data.upper);
    result.type = true;
    result.col = col;
    return result;
  }
  template <class RnGenerator> node_split<Numeric, Categorical> yield(
      int col,
      std::vector<Categorical> &x,
      std::vector<int> &subset,
      RnGenerator & generator) {
    node_split<Numeric, Categorical> result;
    auto newset = distinct( x, subset);
    auto split_res = split_set( newset, generator );
    result.set = CategoricalSet<Categorical>( split_res.left, split_res.right );
    result.type = false;
    result.col = col;
    return result;
  }
  template <class RnGenerator> node_split<Numeric, Categorical> operator () (
      int col,
      storage::DataFrame<Numeric, Categorical> &data,
      std::vector<int> &subset,
      RnGenerator & generator) {
    if( col >= data.num_cols) {
      return yield( col, data.cat_data[col - data.num_cols], subset, generator);
    }
    else {
      return yield(col, data.num_data[col], subset, generator);
    }
  }
};

template <typename Numeric, typename Categorical> struct ExtraTreeSplitter{
  ExtraTreeSplitter<Numeric, Categorical>(){};
  template <class RnGenerator> node_split<Numeric, Categorical> yield(
      int col,
      std::vector<Numeric> &x,
      std::vector<int> &subset,
      RnGenerator & generator) {
    node_split<Numeric, Categorical> result;
    auto data = min_max_subset(x, subset);
    auto middle = sample(data, generator);
    result.range = NumericInterval(data.lower, middle, data.upper);
    result.type = true;
    result.col = col;
    return result;
  }
  template <class RnGenerator> node_split<Numeric, Categorical> yield(
      int col,
      std::vector<Categorical> &x,
      std::vector<int> &subset,
      RnGenerator & generator) {
    node_split<Numeric, Categorical> result;
    auto newset = distinct( x, subset);
    auto split_res = split_set( newset, generator );
    result.set = CategoricalSet<Categorical>( split_res.left, split_res.right );
    result.type = false;
    result.col = col;
    return result;
  }
  template <class RnGenerator> node_split<Numeric, Categorical> operator () (
      int col,
      storage::DataFrame<Numeric, Categorical> &data,
      std::vector<int> &subset,
      RnGenerator & generator) {
    if( col >= data.num_cols) {
      return yield( col, data.cat_data[col - data.num_cols], subset, generator);
    }
    else {
      return yield(col, data.num_data[col], subset, generator);
    }
  }
private:
  int split_samples;
  int col_samples;
};



template <typename Numeric, typename Categorical> struct GreedySplitter{
  GreedySplitter<Numeric, Categorical>(){};
  template <class RnGenerator> node_split<Numeric, Categorical> yield(
      int col,
      std::vector<Numeric> &x,
      std::vector<int> &subset,
      RnGenerator & generator) {
    node_split<Numeric, Categorical> result;
    auto data = min_max_subset(x, subset);
    auto middle = sample(data, generator);
    result.range = NumericInterval(data.lower, middle, data.upper);
    result.type = true;
    result.col = col;
    return result;
  }
  template <class RnGenerator> node_split<Numeric, Categorical> yield(
      int col,
      std::vector<Categorical> &x,
      std::vector<int> &subset,
      RnGenerator & generator) {
    node_split<Numeric, Categorical> result;
    auto newset = distinct( x, subset);
    auto split_res = split_set( newset, generator );
    result.set = CategoricalSet<Categorical>( split_res.left, split_res.right );
    result.type = false;
    result.col = col;
    return result;
  }
  template <class RnGenerator> node_split<Numeric, Categorical> operator () (
      int col,
      storage::DataFrame<Numeric, Categorical> &data,
      std::vector<int> &subset,
      RnGenerator & generator) {
    
    auto cols = sequence(data.cols());
    
    for( int i=0; i < this->col_samples; i++ ) {
      // sample;
    }
    
    // if( col >= data.num_cols) {
    //   return yield( col, data.cat_data[col - data.num_cols], subset, generator);
    // }
    // else {
    //   return yield(col, data.num_data[col], subset, generator);
    // }
  }
private:
  int col_samples;
};

#endif
