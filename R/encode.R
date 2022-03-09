traversal_id <- function(tree, row) {
  if (is.character(tree$rule)) {
    return(tree$node_id)
  }
  # if the column is of type numeric
  if (is.numeric(tree$rule$rule)) {
    # check whether we go left (value is smaller than the upper boundary of our rule)
    if (row[tree$rule$column] <= tree$rule$rule) {
      traversal_id(tree$left, row)
    }
    # or right - value greater than that.
    else {
      traversal_id(tree$right, row)
    }
  }
  # if column is of a different type - ie character
  else {
    # either the row is in the current set of rules (goes left)
    if (row[tree$rule$column] %in% tree$rule$rule) {
      traversal_id(tree$left, row)
    } else {
      # or it goes right
      traversal_id(tree$right, row)
    }
  }
}
#' Encoder
#' @description Calculate an encoding from an object (generic method)
#' @param object The object to use
#' @param X The data to encode.
#' @param ... Additional arguments passed to individual methods.
#' @return An encoded object.
#' @export
#' @rdname encoder
encode <- function(object, ...) {
  UseMethod("encode", object)
}
#' @export
#' @rdname encoder
encode.random_tree <- function(object, X, ...) {
  encoded <- rep(NA, nrow(X))
  for (row in seq_len(nrow(X))) {
    encoded[row] <- traversal_id(object, X[row, ])
  }
  return(encoded)
}
#' @export
#' @rdname encoder
encode.encoder_forest <- function(object, X, ...) {
  furrr::future_map(
    object$forest,
    function(tree) {
      encode(tree, X, ...)
    }
  )
}
