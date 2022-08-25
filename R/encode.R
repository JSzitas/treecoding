# possibly further speedups by passing X in an environment and doing environment lookup
# which should give us 'by reference' behaviour and thus avoid copying
traversal_id_vec <- function(tree, X, id = seq_len(nrow(X))) {
  if (length(id) == 0) {
    return()
  }
  if (is.character(tree$rule)) {
    return(data.frame(id = id, node_id = tree$node_id))
  }
  mia <- is.na(X[id, tree$rule$column])
  # if the column is of type numeric
  if (is.numeric(tree$rule$rule)) {
    left_going <- id[X[, tree$rule$column][id] <= tree$rule$rule]
    right_going <- id[X[, tree$rule$column][id] > tree$rule$rule]
  }
  # if column is of a different type - ie character
  else {
    left_going <- id[X[id, tree$rule$column] %in% tree$rule$rule]
    right_going <- id[!(X[id, tree$rule$column] %in% tree$rule$rule)]
  }
  if (!as.logical(tree$rule$na_dir)) {
    left_going <- c(left_going, which(mia))
  }
  else {
    right_going <- c(right_going, which(mia))
  }
  rbind(
    traversal_id_vec(tree$left, X, id = left_going),
    traversal_id_vec(tree$right, X, id = right_going)
  )
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
  result <- traversal_id_vec(object[["tree"]], X)
  result[order(result$id), ]$node_id
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
