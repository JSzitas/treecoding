find_terminal_values <- function(tree, terminals = list(), ...) {
  if (is.character(tree$rule)) {
    # the casting might look a bit awkward, but it lets us keep a list column
    # without warnings or errors - which is what we really want
    result <- as.data.frame(cbind(
      id = tree$node_id,
      column = as.numeric(names(tree$terminal_rules)),
      values = tree$terminal_rules
    ))
    return(result[order(unlist(result$column)), ])
  }
  rbind(
    find_terminal_values(tree$left),
    find_terminal_values(tree$right)
  )
}
flatten_encoded_forest <- function( forest ) {
  data.table::rbindlist(
    purrr::imap( forest, function(tree, id) {
      data.frame( row_id = seq_len(length(tree)),tree_id = id, node_id = tree )
    })
  )
}

decode_terminal_nodes <- function(terminal_node_df,
                                  method = list(mean_sample, random_sample, first, last)[[1]],
                                  ...) {
  as.data.frame(
    cbind( column = unlist(terminal_node_df$column),
           node_id = unlist(terminal_node_df$id),
           value = sapply(terminal_node_df$values, method)
    )
  )
}
reconcile_column <- function( x ) {
  if (is.numeric(x)) {
    return(mean(x, na.rm = TRUE))
  }
  names(table(x))[1]
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
  column <- row_id <- value <- NULL
  # terminal_values <- find_terminal_values(object)
  result <- decode_terminal_nodes(
    find_terminal_values(object)
    )
  # coerce terminal_ids into a data.frame for joins
  flattened_tree <- data.frame( node_id = terminal_ids,
                                row_id = seq_len(length(terminal_ids)))
  # merge flattened terminal_ids with result of terminal value decoding
  result <- merge( data.table::as.data.table(result),
                   flattened_tree, on = c("node_id"), allow.cartesian = TRUE )
  # drop tree_id and node_id
  result[, ("node_id") := NULL]
  # reconcile the values
  result <- result[, reconcile_column(value), by= list(row_id, column)]
  # and cast to a wider format
  result <- data.table::dcast(result, row_id~column, value.var = "V1")
  # drop index and return
  return(as.data.frame(result[,-c(1)]))
}
#' @export
#' @rdname decoder
#' @importFrom data.table .SD
decode.encoder_forest <- function(object, terminal_ids, ...) {
  # appease data.table
  column <- row_id <- value <- NULL
  terminal_values <- purrr::map(object$forest, find_terminal_values, ...)
  # decode all terminal nodes
  result <- data.table::rbindlist(
    purrr::imap( terminal_values, function(values, index) cbind(tree_id = index,
                                                              decode_terminal_nodes(values))
               )
  )
  # flatten forest generated ids to a table
  flattened_forest <- flatten_encoded_forest(terminal_ids)
  # merge flattened terminal_ids with result of terminal value decoding
  result <- merge( result, flattened_forest, on = c("node_id","tree_id"), allow.cartesian = TRUE )
  # drop tree_id and node_id
  result[, (c("tree_id","node_id")) := NULL]
  # reconcile the values
  result <- result[, reconcile_column(value), by= list(row_id, column)]
  # and cast to a wider format
  result <- data.table::dcast(result, row_id~column, value.var = "V1")
  # drop index and return
  return(as.data.frame(result[,-c(1)]))
}
