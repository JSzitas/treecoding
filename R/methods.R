is_encoder_forest <- function(x) {
  class(x) == "encoder_forest"
}

is_random_tree <- function(x) {
  class(x) == "random_tree"
}

#' @export
`[.encoder_forest` <- function(x, j, drop = FALSE) {
  if (!is_encoder_forest(x)) {
    return(NextMethod())
  }
  j_arg <- substitute(j)
  if (missing(j)) {
    j <- NULL
    j_arg <- NULL
  } else if (is.null(j)) {
    j <- integer()
  }
  if (drop) {
    return(unclass(x)[["forest"]][j][[1]])
  }
  structure(list(
    forest = unclass(x)[["forest"]][j],
    Phi = x[["Phi"]]
  ),
  class = "encoder_forest"
  )
}
