within <- function(x, range) {
  if (is.numeric(range)) {
    return(x <= range[2] && x > range[1])
  } else {
    return(x %in% range[[1]])
  }
}
check_ranges <- function(row, ranges) {
  any(unlist(purrr::map2(as.list(row), ranges, ~ within(.x, .y))))
}

ranges_violated <- function(X, id, ranges) {
  purrr::map_dfr(X[id, , drop = FALSE], ~ check_ranges(.x, ranges))
}

within <- function(x, range) {
  if (is.numeric(range[[1]])) {
    return(x <= range[[1]][2] & x >= range[[1]][1])
  } else {
    return(x %in% range[[1]])
  }
}

return_uncertainty <- function(id) {
  if (length(id) > 0) {
    return(data.frame(id = id))
  }
}

traversal_sampling_uncertain <- function(tree, X, id = seq_len(nrow(X)), predict_fun) {
  if (length(na.omit(id)) == 0 || all(is.na(id))) {
    return()
  }
  if (is.character(tree$rule)) {
    pred <- predict_fun(X, id, tree)
    if (is.null(pred) || length(pred) == 0) pred <- rep(NA, length(id))
    return(suppressWarnings(cbind(id = id, as.data.frame(t(cbind(pred))))))
  }
  # find mia rows
  mia <- is.na(X[id, tree$rule$column])
  id <- id[!mia]

  left_going <- id[within(X[id, tree$rule$column], tree$rule$ranges$left)]
  right_going <- id[within(X[id, tree$rule$column], tree$rule$ranges$right)]
  nowhere_going <- setdiff(id, c(left_going, right_going))
  data.table::rbindlist(list(
    traversal_sampling_uncertain(tree$left, X, id = left_going, predict_fun),
    traversal_sampling_uncertain(tree$right, X, id = right_going, predict_fun),
    return_uncertainty(nowhere_going)
  ),
  fill = TRUE
  )
}
#' @export
#' @rdname prediction
predict.uncertain_tree <- function(x, newdata, predict_fun = predictor_identity, ...) {
  traversal_sampling_uncertain(
    tree = x[["tree"]],
    X = newdata,
    id = seq_len(nrow(newdata)),
    predict_fun = predict_fun
  )
}
