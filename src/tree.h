#ifndef TREE_HEADER
#define TREE_HEADER

#include <functional>
#include <optional>
#include <variant>
#include <vector>
#include "Eigen/Dense"
#include "utils.h"
#include "storage.h"
// #include "split.h"
#include "ranges.h"

struct node {
  // maybe this can be just a std::vector<float>? where we check
  // its size and if its size is > 1 we can cast to int and solve an easier problem
  // std::vector<float> split;
  // maybe really actually use a variant ie - the visitor might not be so bad
  split<float, int> range;
  bool terminal = false;
  intervals<float, int> terminal_range;
  int node_size;
  node *left;
  node *right;
};

class Tree {
  public:
    // intializer
    void initTree( TreeDataStorage data,
                   int max_depth,
                   int min_nodesize
                   // spltter will be a callable
                   // optional extratree k
                   // std::optional<int> k,
                   // optional extratree mtry
                   // std::optional<int> mtry
                   ) {
      data = data;
      max_depth = max_depth;
      min_nodesize = min_nodesize;
    };
    void grow(){

    };
    // growing a tree
    // void grow_tree();
    // creating predictions
    // void predict();
  private:
    int max_depth;
    int min_nodesize;
    TreeDataStorage data;
    node tree;
};



#endif
