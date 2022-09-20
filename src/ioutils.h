#ifndef IO_UTILS
#define IO_UTILS

#include "stdio.h"
#include "iostream"

template <class T> void print_vector( T& x ) {
  if( x.size() == 0 ) {
    return;
  }
  for(int i=0;i<(x.size()-1);i++) {
    std::cout << x[i] << ", ";
  }
  std::cout << x[(x.size()-1)] << std::endl;
}

#endif
