#ifndef TYPEDEF_HEADER
#define TYPEDEF_HEADER

#include "Eigen/Dense"
#include <variant>

typedef std::variant<Eigen::VectorXf,
                     Eigen::VectorXi,
                     Eigen::MatrixXf,
                     Eigen::MatrixXi,
                     std::monostate> target_variant;

#endif
