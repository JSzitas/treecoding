#ifndef TREE_HEADER
#define TREE_HEADER

// #include <functional>
#include <variant>
#include <vector>
// #include "Eigen/Dense"
#include "utils.h"
#include "ranges.h"
#include "iostream"
#include "stdio.h"
// typedef std::variant<Eigen::MatrixXf, Eigen::MatrixXi> col_variant;

struct node {
  node(){};
  node_split<float, int> range;
  bool terminal = false;
  intervals<float, int> terminal_range;
  int current_depth = 0;
  int node_size;
  // node *left, *right;
  std::vector<node> children;
};

// void grab_nodes( node *tree, std::set<node*> *gathered_nodes) {
//   if(tree->terminal == false) {
//     return;
//   }
//   gathered_nodes->insert(tree);
//
//   grab_nodes(tree->left, gathered_nodes);
//   grab_nodes(tree->right, gathered_nodes);
// }


// node* newNode(int data)
// {
//   node* temp = new node;
//   temp->data = data;
//   temp->left = temp->right = NULL;
//   return temp;
// }

template <typename T, typename U> struct Splitter{
  node_split<T,U> operator () () {

  };
};


template <class RngGenerator>
class Tree {
  public:
    Tree<RngGenerator>( Eigen::MatrixXf data,
          std::vector<int> numeric_cols,
          RngGenerator & generator,
          int max_depth = 8,
          int min_nodesize = 30) {
      tree_max_depth = max_depth;
      tree_min_nodesize = min_nodesize;
      gen = generator;
      X = data;
      cols = X.cols();
      // allocate first tree node
      // node *tree = new node;
      for( int i=0; i< cols; i++ ) {
        if( !all_const( X.col(i) )) {
          nonconst_cols.push_back(i);
        }
      }
      num_cols = numeric_cols;
    };
    void grow( node &tree,
               std::vector<int> row_ids,
               intervals<float, int> ranges = {},
               int current_depth = 0,
               int tree_arity = 2){
      // the goto tag for when we found nothing to split on or a constant column
      // termination_label:
      // termination
      if( row_ids.size() <= tree_min_nodesize || current_depth >= tree_max_depth || nonconst_cols.size() < 1) {
        std::cout << "Terminal conditions met: ";
        std::cout << "Depth: " << current_depth << " Nodesize: " << row_ids.size() << std::endl;
        tree.terminal_range = ranges;
        tree.node_size = row_ids.size();
        tree.terminal = true;
        return;
      }
      // First we take the data and generate a candidate split -
      // for now we will only use random trees, we can try to extend this
      // later
      // generate column over which we will split
      // int col = sample_int_from_set( nonconst_cols, gen );
      // // check that the column is not all const
      // // and if it is probably just update the nonconst_cols
      // // and GOTO before this happened. which I admit is ugly, but this is
      // // probably the one reasonable case where you want to do that.
      // auto sel_col = X.col(col);
      // if( all_const( sel_col )) {
      //   // erase takes iterator
      //   auto v = nonconst_cols.begin();
      //   std::advance(v, col-1);
      //   nonconst_cols.erase(v);
      //
      //   goto termination_label;
      // }
      // std::cout << current_depth << "\n";
      //
      // node_split<float, int> node_vals;

      // check if col is a numeric column
      // if( belongs( num_cols, col) ) {
      //   tree->range = node_split<float, int>( sel_col, col);
      // }
      // else {
      //   // attemp typecast
      //   tree->range = node_split<int, int>( (int)sel_col, col);
      // }
      // determine where row ids go

      // allocate child nodes
      // tree->left = new node;
      // tree->right = new node;
      tree.children = std::vector<node>(tree_arity);
      for( int i = 0; i < tree_arity; i++) {
        // recursive calls for constructing children
        grow(tree.children[i], row_ids, ranges, current_depth+1);
      }
      // update ranges

      // recursive calls to the left and right
      // grow(tree.children[0], row_ids, ranges, current_depth+1);
      // grow(tree.children[1], row_ids, ranges, current_depth+1);
    };
    void fit() {
      std::cout << "Growing." << "\n";
      grow( tree, sequence(0, (int)(X.rows()), 1), {}, 0 );
    };
    void summary() {
      std::cout << "Max depth: " << tree_max_depth << "\n" ;
      std::cout << "Min nodesize: " << tree_min_nodesize << "\n";
      if( num_cols.size() > 0 ) {
        std::cout << "Numeric columns: " << "\n";
        for( auto & item : num_cols ) {
          std::cout << item << " ";
        }
        std::cout << "\n";
      }
      if( nonconst_cols.size() > 0 ) {
        std::cout << "Nonconst cols: " << "\n";
        for( auto & item : nonconst_cols) {
          std::cout << item << " ";
        }
      }
    };
    // creating predictions
    // void predict();
  private:
    std::vector<int> num_cols;
    int tree_max_depth;
    int tree_min_nodesize;
    Eigen::MatrixXf X;
    node tree;
    int cols;
    std::vector<int> nonconst_cols;
    RngGenerator gen;
};



#endif
