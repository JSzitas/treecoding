#ifndef MATRIX_HEADER
#define MATRIX_HEADER

template <typename T> struct split_result {
  split_result<T>() {
    std::vector<T> left(0);
    std::vector<T> right(0);
  };
  std::vector<T> left;
  std::vector<T> right;
};

template <typename NumericKind, typename CategoricKind> class Matrix {
  public:
    Matrix() {
      num_data = std::vector<NumericKind>(0);
      cat_data = std::vector<CategoricKind>(0);
      num_cols = 0;
      cat_cols=0;
      initialized = false;
    };
    // add constructors from eigen matrices
    void init( std::vector<NumericKind> x ) {
      if(initialized) {
        return;
      }
      num_data.push_back(x);
      ncol++;
      nrow = x.size();
      num_cols++;
    }
    void init( std::vector<CategoricKind> x ) {
      if(initialized) {
        return;
      }
      cat_data.push_back(x);
      ncol++;
      nrow = x.size();
      cat_cols++;
    }
    void add( std::vector<NumericKind> x ) {
      if( x.size() > nrow || x.size() < nrow ) {
        std::cout << "Attemp to add a column with differing number of rows: failed" << std::endl;
        return;
      }
      num_data.push_back(x);
      ncol++;
      num_cols++;
    }
    void add( std::vector<CategoricKind> x ) {
      if( x.size() > nrow || x.size() < nrow ) {
        std::cout << "Attemp to add a column with differing number of rows: failed" << std::endl;
      }
      cat_data.push_back(x);
      ncol++;
      cat_cols++;
    }
    split_result<int> match( std::vector<CategoricKind> set, int col, std::vector<int> subset ) {

    }
    split_result<int> seq( NumericKind x, int col, std::vector<int> subset ) {
      split_result<int> result;
      if( col > num_cols ) {
        return result;
      }
      result.left.reserve(subset.size());
      result.right.reserve(subset.size());

      for( auto &index:subset ) {
        if(num_data[col][index] <= x) {
          result.left.push_back(num_data[col][index]);
        }
        else {
          result.right.push_back(num_data[col][index]);
        }
      }
      return result;
    }
    split_result<int> geq( NumericKind x, int col, std::vector<int> subset ) {
      split_result<int> result;
      if( col > num_cols ) {
        return result;
      }
      result.left.reserve(subset.size());
      result.right.reserve(subset.size());

      for( auto &index:subset ) {
        if(num_data[col][index] >= x) {
          result.left.push_back(num_data[col][index]);
        }
        else {
          result.right.push_back(num_data[col][index]);
        }
      }
      return result;
    }
private:
  std::vector<NumericKind> num_data;
  std::vector<CategoricKind> cat_data;
  bool initialized;
  int nrow;
  int ncol;
  int num_cols;
  int cat_cols;
};

#endif
