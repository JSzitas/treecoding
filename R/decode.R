find_terminal_values <- function(tree, terminals = list(), ...) {
  if (is.character(tree$rule)) {
    # the casting might look a bit awkward, but it lets us keep a list column
    # without warnings or errors - which is what we really want
    result <- as.data.frame(cbind(
      id = tree$node_id,
      column = as.numeric(names(tree$terminal_rules)),
      values = tree$terminal_rules
    ))
    return(result)
  }
  rbind(
    find_terminal_values(tree$left),
    find_terminal_values(tree$right)
  )
}
flatten_encoded_forest <- function(forest) {
  data.table::rbindlist(
    purrr::imap(forest, function(tree, id) {
      data.frame(row_id = seq_len(length(tree)), tree_id = id, node_id = tree)
    })
  )
}
decode_terminal_nodes <- function(terminal_node_df, ...) {
  as.data.frame(
    cbind(
      column = unlist(terminal_node_df$column),
      node_id = unlist(terminal_node_df$id),
      value = terminal_node_df$values
    )
  )
}
# the tree specific version calls mean_sample on the individual values first
# this would be bad across a forest (where you can reconcile intervals ala
# the maximal compatible rule) but for individual
# trees this is no worse that anything else
# since I want to avoid overhead in sapply calls ( even passing identity would
# cause some ) I wrote 2 distinct versions of this function
decode_terminal_nodes_tree <- function(terminal_node_df, ...) {
  as.data.frame(
    cbind(
      column = unlist(terminal_node_df$column),
      node_id = unlist(terminal_node_df$id),
      value = sapply(terminal_node_df$values, mean_sample)
    )
  )
}
reconcile_intervals <- function(x) {
  if (is_numeric(x[[1]])) {
    # x <- do.call( rbind, x )
    # return( mean(max(x[,1]), min(x[,2])) )
    return(stats::median(unlist(x)))
  }
  Reduce(intersect, x)[1]
}
#' Decoder
#' @description Calculate an decoding from an object (generic method)
#' @param object The object to use
#' @param terminal_ids Either a vector of terminal node ids (for an individual tree),
#' or a list of vectors of indvidual ids (produced by encoding from a while forest).
#' @param ... Additional arguments passed to individual methods.
#' @return A decoded object.
#' @export
#' @rdname decoder
decode <- function(object, ...) {
  UseMethod("decode", object)
}
#' @export
#' @rdname decoder
decode.random_tree <- function(object, terminal_ids, ...) {
  # appease data.table
  node_id <- column <- NULL
  # terminal_values <- find_terminal_values(object)
  result <- decode_terminal_nodes_tree(
    find_terminal_values(object)
  )
  # coerce terminal_ids into a data.frame for joins
  flattened_tree <- data.table::data.table(
    node_id = as.integer(terminal_ids),
    row_id = seq_len(length(terminal_ids))
  )
  # merge flattened terminal_ids with result of terminal value decoding
  result <- data.table::as.data.table(result)
  result[, node_id := as.integer(node_id)]
  result[, column := as.integer(column)]
  result <- merge(result,
    flattened_tree,
    on = c("node_id"), allow.cartesian = TRUE
  )
  result <- result[, -c(1), drop = FALSE]
  result <- data.table::dcast(result, row_id ~ column, value.var = "value")
  return(result[, -c(1)])
}
#' @export
#' @rdname decoder
#' @importFrom data.table .SD :=
decode.encoder_forest <- function(object, terminal_ids, ...) {
  # appease data.table
  column <- row_id <- value <- node_id <- tree_id <- NULL
  terminal_values <- purrr::map(object$forest, find_terminal_values, ...)
  # decode all terminal nodes
  result <- data.table::rbindlist(
    purrr::imap(terminal_values, function(values, index) {
      cbind(
        tree_id = index,
        decode_terminal_nodes(values)
      )
    })
  )
  result[, column := as.integer(unlist(column))]
  result[, node_id := as.integer(unlist(node_id))]
  result[, tree_id := as.integer(tree_id)]
  # flatten forest generated ids to a table
  flattened_forest <- flatten_encoded_forest(terminal_ids)
  flattened_forest[, tree_id := as.integer(tree_id)]
  flattened_forest[, node_id := as.integer(node_id)]
  result <- merge(result, flattened_forest, on = c("node_id", "tree_id"), allow.cartesian = TRUE)
  # return(result)
  result[, (c("node_id", "tree_id")) := NULL]
  # result <- tidyr::pivot_wider( result, names_from = "column", values_from = "value", values_fn = reconcile_intervals )
  # # result <- data.table::dcast( result, row_id ~ column, value.var = "value", fun.aggregate = reconcile_intervals)
  # return(result)
  return(result)

  # reconcile and cast to wide
  result <- result[, as.character(reconcile_intervals(value)), by = list(column, row_id)]
  result <- data.table::dcast(result, row_id ~ column, value.var = "V1")
  result <- result[order(result$row_id), ]
  result <- result[, lapply(.SD, function(col) {
    if (is_numeric(col)) {
      col <- as.numeric(col)
    } else {
      col <- as.character(col)
    }
    return(col)
  }), .SDcols = grep(x = colnames(result), invert = TRUE, pattern = "row_id", value = TRUE)]

  return(result)
}
