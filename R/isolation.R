c_factor <- function(n) {
  if (n <= 2) {
    return(n - 1)
  }
  H <- log(n - 1) + 0.5772156649
  return(2 * H - (2 * (n - 1) / n))
}
traversal_isolation_score_vec <- function(tree, X, id = seq_len(nrow(X)), current_depth = 0) {
  if (length(id) == 0) {
    return()
  }
  if (is.character(tree$rule)) {
    return(data.frame(
      id = id,
      score = current_depth + c_factor(tree$n)
    ))
  }
  # if the column is of type numeric
  if (is.numeric(tree$rule$rule)) {
    left_going <- X[id, tree$rule$column] <= tree$rule$rule
  }
  # if column is of a different type - ie character
  else {
    left_going <- X[id, tree$rule$column] %in% tree$rule$rule
  }
  rbind(
    traversal_isolation_score_vec(tree$left, X, id = id[left_going], current_depth + 1),
    traversal_isolation_score_vec(tree$right, X, id = id[!left_going], current_depth + 1)
  )
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
  result <- traversal_isolation_score_vec(object[["tree"]], X)
  # result
  result[order(result$id), ]$score
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
  2^(-rowMeans(forest_scores) / c_factor(object$Phi))
}
