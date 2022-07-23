sample_from_range2 <- function(X, row_id, available_columns, ranges, ...) {
  new_weights <- ranges_to_weights(ranges)
  column <- sample(available_columns, 1,
    prob = new_weights / sum(new_weights)
  )

  row_id <- row_id[!is.na(X[row_id, column])]

  if (length(row_id) == 0) {
    return(
      list(
        column = column,
        rule = NULL,
        left = NULL,
        right = NULL,
        mia_dir = NULL
      )
    )
  }

  range <- ranges[column]

  if (is.numeric(unlist(range))) {
    range <- unlist(range)
    position <- runif(1, range[1], range[2])

    left <- row_id[X[row_id, column] <= position]
    right <- row_id[X[row_id, column] > position]

    left_range <- unname(c(range[1], position))
    right_range <- unname(c(position, range[2]))
  } else {
    # randomly permute all levels
    permutation <- sample(unlist(range), length(range))
    # find 'position' of rule
    # sample a single level
    position <- ceiling(runif(1, min = 0, max = length(range)))
    left_range <- permutation[seq_len(length(permutation)) <= position]
    right_range <- permutation[seq_len(length(permutation)) > position]

    left <- row_id[X[row_id, column] %in% left_range]
    right <- row_id[X[row_id, column] %in% right_range]
  }

  list(
    column = column,
    left = left_range,
    right = right_range,
    left_id = left,
    right_id = right
  )
}

# find rule - currently only supports numeric columns
find_rule2 <- function(X, row_id, available_columns, ranges, ...) {

  # sample from given range
  rule <- sample_from_range2(X, row_id, available_columns, ranges)
  # update ranges and return
  left_ranges <- ranges
  left_ranges[[rule$column]] <- rule$left
  right_ranges <- ranges
  right_ranges[[rule$column]] <- rule$right

  return(
    list(
      column = rule$column,
      left_ranges = left_ranges,
      right_ranges = right_ranges,
      left_id = rule$left_id,
      right_id = rule$right_id
    )
  )
}

split2 <- function(X,
                   max_depth = 4,
                   row_id,
                   split_finder = find_rule2,
                   node_id = 1,
                   parameter_sampler = sampler_nada,
                   min_nodesize = 15,
                   available_columns = seq_len(ncol(X)),
                   ranges,
                   current_depth = 0,
                   ...) {
  # move col to within split finder
  rule <- split_finder(X, row_id, available_columns, ranges, ...)
  # reaching a terminal node - you run out of data, or you reach max_depth, or
  # you have a constant column
  if (is.null(rule$column) || length(row_id) <= min_nodesize || current_depth >= max_depth) {
    return(list(
      rule = "terminal_node",
      # but in principle this is fine - we resolve terminal rules
      # pruning rules that have become redundant (ie for each column just
      # take the last available set of rules)
      ranges = ranges, # resolve_terminal_rules(terminal_rules),
      node_id = node_id,
      n = row_id,
      parameter_estimates = parameter_sampler(X, row_id, ...)
    ))
  }

  list(
    rule = list(
      column = rule$column,
      ranges = list(
        left = rule$right_ranges[rule$column],
        right = rule$left_ranges[rule$column]
      )
    ),
    left = split2(X,
      max_depth = max_depth,
      row_id = rule$left_id,
      split_finder = split_finder,
      # nodes use a simple numbering scheme - the left ones are even
      # while the right ones are odd (hence 2k and 2k+1)
      2 * node_id,
      # pass the rules to the terminal node - this is much easier for
      # things like reconstruction and saves some time
      parameter_sampler = parameter_sampler,
      available_columns = available_columns,
      ranges = rule$left_ranges,
      current_depth = current_depth + 1,
      ...
    ),
    right = split2(X,
      max_depth = max_depth,
      row_id = rule$right_id,
      split_finder = split_finder,
      2 * node_id + 1,
      parameter_sampler = parameter_sampler,
      available_columns = available_columns,
      ranges = rule$right_ranges,
      current_depth = current_depth + 1,
      ...
    ),
    node_id = node_id
  )
}

#' Build an uncertain tree
#' @description Fit a random tree to your data.
#' @param X The data to use - currently only supports a matrix.
#' @param max_depth The maximal depth of the tree (though the tree might be shorter - this is an upper bound).
#' @param nosplit_columns Columns to ignore when splitting - but which are nonetheless propagated to
#' the terminal node.
#' @param row_id A subset of rows to use for growing the tree - if **NULL**, use all rows.
#' @param chronological_oob Use only observations which came **after** the training set for errors?
#' @param ... Additional arguments.
#' @return A fitted tree
#' @export
uncertain_tree <- function(X, max_depth = 5, split_finder = find_rule2,
                           nosplit_columns = NULL, row_id = NULL,
                           chronological_errors = TRUE, ...) {
  available_columns <- colnames(X)
  if (!is.null(nosplit_columns)) {
    available_columns <- available_columns[-parse_nosplit_columns(X, nosplit_columns)]
  }
  if (is.null(row_id)) {
    row_id <- seq_len(nrow(X))
  }
  ranges <- compute_ranges(X[row_id, available_columns])
  result <- list(tree = split2(X,
    max_depth = max_depth,
    row_id = row_id, split_finder,
    available_columns = available_columns,
    ranges = ranges, ...
  ))
  maybe_oob_obs <- X[list(
    setdiff(seq_len(nrow(X)), row_id),
    which(seq_len(nrow(X)) > max(row_id))
  )[[chronological_errors + 1]], ]
  if (nrow(maybe_oob_obs) > 0) {
    result <- c(oob_obs = maybe_oob_obs)
  }

  structure(
    result,
    class = "uncertain_tree"
  )
}
