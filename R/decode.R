
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

decode_terminal_node <- function(terminal_node_df,
                                 method = list(mean_sample, random_sample, first, last)[[1]],
                                 ...) {
  values <- terminal_node_df$values
  result <- lapply(values, method)

  stats::setNames(
    data.frame(result),
    terminal_node_df$column
  )
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
  terminal_values <- find_terminal_values(object)

  dplyr::bind_rows(
           furrr::future_map(terminal_ids, function(id) {
             decode_terminal_node( terminal_values[terminal_values$id == id, ],
                                   ... )
             }, .options = furrr::furrr_options(seed = TRUE)
             )
  )
}
#' @export
#' @rdname decoder
decode.encoder_forest <- function(object, terminal_ids, ...) {
  terminal_values <- furrr::future_map(object, find_terminal_values)
  return(terminal_values)
  furrr::future_map2(
    terminal_ids, terminal_values,
    function(id, values) {
      return(NULL)
      # dplyr::bind_rows( purrr::map( id,
      #                               ~ decode_terminal_node( terminal_node_df = values[values$id == .x, ],
      #                                                       ...)
      #                               )
      #                   )
    }, .options = furrr::furrr_options(seed = TRUE)
  )
}
