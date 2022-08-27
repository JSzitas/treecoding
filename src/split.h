#ifndef SPLIT_HEADER
#define SPLIT_HEADER


#include <variant>
#include <vector>
#include "Eigen/Dense"
#include "utils.h"
#include "tree.h"

// template <typename Splitter> void grow( Tree *tree, Splitter splitter ) {
// 
// 
// 
// 
// }

// void split( node *current_node,
//             std::vector<int> row_indices,
//             int current_depth,
//             int max_depth,
//             int min_nodesize ) {
//   // terminate if
//   if( row_indices.size() <= min_nodesize || current_depth >= max_depth) {
//     return;
//   }
//   // otherwise, carry out the splitting nonsense
//
//
//
//
// }



// disjunct_indices<std::vector<int>> find_split_lr( float split,
//                                                   Eigen::VectorXf col,
//                                                   std::vector<int> row_indices){
//   auto index_set = split_indices_st_cond<std::vector<int>, Eigen::VectorXf, float>( col, split );
//   // assign rows to left and right
//
//
//
//
//
//
// }
//
// disjunct_indices<std::vector<int>> find_split_lr( std::vector<int> split,
//                                                   Eigen::VectorXf col,
//                                                   std::vector<int> row_indices){
//   auto index_set = split_indices_within_set<std::vector<int>, Eigen::VectorXi, std::vector<int>>(col, split);
//
//
//
// }


// void split_on_node( node * current_node,
//                     Eigen::MatrixXf num_data,
//                     Eigen::MatrixXi cat_data,
//                     std::vector<int> row_indices ) {
//   auto col = find_column( num_data, cat_data, col_index );
//   // the visitor
//
//   // assign indices to nodes
//
//
// }





#endif
