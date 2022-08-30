#ifndef TREE_HEADER
#define TREE_HEADER

// #include <functional>
// #include <optional>
#include <variant>
// #include <vector>
// #include "Eigen/Dense"
#include "utils.h"
#include "ranges.h"
typedef std::variant<Eigen::MatrixXf, Eigen::MatrixXi> col_variant;


struct node {
  // maybe this can be just a std::vector<float>? where we check
  // its size and if its size is > 1 we can cast to int and solve an easier problem
  // std::vector<float> split;
  // maybe really actually use a variant ie - the visitor might not be so bad
  node_split<float, int> range;
  bool terminal = false;
  intervals<float, int> terminal_range;
  int current_depth = 0;
  int node_size;
  node *left;
  node *right;
};

template <class RngGenerator> class Tree {
  public:
    Tree( Eigen::MatrixXf num_data,
          Eigen::MatrixXi cat_data,
          RngGenerator & generator,
          target_variant target = {},
          int max_depth = 8,
          int min_nodesize = 30) {
      num_data = num_data;
      cat_data = cat_data;
      num_cols = num_data.cols();
      cat_cols = cat_data.cols();
      total_cols= num_cols + cat_cols;
      max_depth = max_depth;
      min_nodesize = min_nodesize;
      &generator = generator;
      nonconst_cols = sequence<int>(0, total_cols, 1);
    };
    ~Tree(){};
    void grow( node * tree,
               std::vector<int> row_ids,
               intervals<float, int> ranges,
               int current_depth = 0){
      // there should be a goto tag to go **HERE** most likely (if we find 
      // we have no nonconst cols)
      
      
      // termination
      if( row_ids.size() <= min_nodesize || current_depth >= max_depth || nonconst_cols.size() < 1) {
        tree->terminal_range = ranges;
        tree->node_size = row_ids.size();
        tree->terminal = true;
        return;
      }

      
      // First we take the data and generate a candidate split -
      // for now we will only use random trees, we can try to extend this
      // later
      int col = sample_int_from_set( nonconst_cols, generator );
      if( col > num_cols) {
        col = col - num_cols;
      }
      
      col_variant sel_col;
      if( col > num_cols) {
        sel_col = cat_data.col(col);
      } 
      else {
        sel_col = num_data.col(col);
      }
      // check that the column is not all const
      // and if it is probably just update the nonconst_cols 
      // and GOTO before this happened. which I admit is ugly, but this is 
      // probably the one reasonable case where you want to do that. 
      
      
      // all_const();
      
      tree->range = node_split<float, int>( sel_col, col);
      
      
      // for(int i=0; i < total_cols; i++) {
        // after you select a column, compute a range and fill it in our
        // current node
        // tree->range = split<float, int>();
      // }



      // recursive calls to the left and right
      grow(tree->left, row_ids, ranges, current_depth+1);
      grow(tree->right, row_ids, ranges, current_depth+1);
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
    // int ncol;
    // int nrow;
    int num_cols;
    int cat_cols;
    int total_cols;
    std::vector<int> nonconst_cols;
    RngGenerator & generator;
};



#endif
