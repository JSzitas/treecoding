#include <Rcpp.h>
using namespace Rcpp;
#include <vector>
#include "quasirng.h"

// [[Rcpp::export]]
std::vector<float> halton_gen( int samples = 100) {
  halton_generator s;
  std::vector<float> result;
  result.reserve(samples);
  for( int i=0;i < samples;i++) {
    result.push_back(s.yield());
  }
  return result;
}

// [[Rcpp::export]]
std::vector<float> rec_gen( int samples = 100) {
  reccurent_generator s;
  std::vector<float> result;
  result.reserve(samples);
  for( int i=0;i < samples;i++) {
    result.push_back(s.yield());
  }
  return result;
}

// [[Rcpp::export]]
std::vector<float> xos_gen( int samples = 100) {
  xoshiro_generator s;
  s.init();
  // s.set( 31856786762345,  14124124124774, 352345235238674, 124134124135683);
  std::vector<float> result;
  result.reserve(samples);
  for( int i=0;i < samples;i++) {
    result.push_back(s.yield());
  }
  return result;
}

// [[Rcpp::export]]
std::vector<float> xor_gen( int samples = 100) {
  xorshift_generator s;
  s.init();
  // s.set( 31856786762345,  14124124124774);
  std::vector<float> result;
  result.reserve(samples);
  for( int i=0;i < samples;i++) {
    result.push_back(s.yield());
  }
  return result;
}

// [[Rcpp::export]]
std::vector<float> sm_gen( int samples = 100) {
  splitmix_generator s;
  s.set( 31856786762345 );
  std::vector<float> result;
  result.reserve(samples);
  for( int i=0;i < samples;i++) {
    result.push_back(s.yield());
  }
  return result;
}





