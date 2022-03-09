c_factor <- function(n) {
  if (n <= 2) return(n-1)
  H <- log(n - 1) + 0.5772156649
  return(2 * H - (2*(n - 1)/n))
}

traversal_isolation_score <- function(tree, row, current_depth = 0) {
  if (is.character(tree$rule)) {
    return(current_depth + c_factor(tree$n) )
  }
  # if the column is of type numeric
  if (is.numeric(tree$rule$rule)) {
    # check whether we go left (value is smaller than the upper boundary of our rule)
    if (row[tree$rule$column] <= tree$rule$rule) {
      traversal_isolation_score(tree$left, row, current_depth + 1)
    }
    # or right - value greater than that.
    else {
      traversal_isolation_score(tree$right, row, current_depth + 1)
    }
  }
  # if column is of a different type - ie character
  else {
    # either the row is in the current set of rules (goes left)
    if (row[tree$rule$column] %in% tree$rule$rule) {
      traversal_isolation_score(tree$left, row, current_depth + 1)
    } else {
      # or it goes right
      traversal_isolation_score(tree$right, row, current_depth + 1)
    }
  }
}

#' Isolation forest score
#' @description Calculate an isolation score from an object (generic method)
#' @param object The object to use
#' @param X The data to compute for.
#' @param ... Additional arguments passed to individual methods.
#' @return An isolation score object.
#' @export
#' @rdname isolation
isolation <- function(object, ...) {
  UseMethod("isolation", object)
}
#' @export
#' @rdname isolation
isolation.random_tree <- function(object, X, ...) {
  isolation_scores <- rep(NA, nrow(X))
  for (row in seq_len(nrow(X))) {
    isolation_scores[row] <- traversal_isolation_score(object, X[row, ])
  }
  return(isolation_scores)
}
#' @export
#' @rdname isolation
isolation.encoder_forest <- function(object, X, ...) {
  forest_scores <- furrr::future_map(
    object$forest,
    function(tree) {
      isolation(tree, X, ...)
    }
  )
  forest_scores <- do.call(cbind, forest_scores)
  2^(-rowMeans(forest_scores)/c_factor(object$Phi))
}
