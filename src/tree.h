#ifndef TREE_HEADER
#define TREE_HEADER

// #include <functional>
// #include <optional>
// #include <variant>
// #include <vector>
// #include "Eigen/Dense"
#include "utils.h"
#include "ranges.h"

struct node {
  // maybe this can be just a std::vector<float>? where we check
  // its size and if its size is > 1 we can cast to int and solve an easier problem
  // std::vector<float> split;
  // maybe really actually use a variant ie - the visitor might not be so bad
  split<float, int> range;
  bool terminal = false;
  intervals<float, int> terminal_range;
  int current_depth = 0;
  int node_size;
  node *left;
  node *right;
};

class Tree {
  public:
    Tree( Eigen::MatrixXf num_data,
          Eigen::MatrixXi cat_data,
          target_variant target = {},
          int max_depth = 8,
          int min_nodesize = 30) {
      num_data = num_data;
      cat_data = cat_data;
      max_depth = max_depth;
      min_nodesize = min_nodesize;
    };
    void grow( node * tree,
               std::vector<int> row_ids,
               intervals<float, int> ranges,
               int current_depth = 0){
      // termination
      if( row_ids.size() <= min_nodesize || current_depth >= max_depth ) {
        tree->terminal_range = ranges;
        tree->node_size = row_ids.size();
        tree->terminal = true;
        return;
      }


      // First we take the data and generate a candidate split -
      // for now we will only use random trees, we can try to extend this
      // later
      // for(int i=0; i < data.cols(); i++) {
        // after you select a column, compute a range and fill it in our
        // current node
        // tree->range = split<float, int>();
      // }



      // recursive calls to the left and right
      // grow(tree->left, left_ids, ranges, current_depth+1);
      // grow(tree->right, right_ids, ranges, current_depth+1);
    };
    // growing a tree
    // void grow_tree();
    // creating predictions
    // void predict();
  private:
    int max_depth;
    int min_nodesize;
    Eigen::MatrixXf num_data;
    Eigen::MatrixXi cat_data;
    node tree;
    int ncol;
    int nrow;
    int num_cols;
    int cat_cols;
};



#endif
