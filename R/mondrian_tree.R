sample_from_range <- function(X, row_id, available_columns, ranges) {
  new_weights <- ranges_to_weights(ranges)
  # new_weights <- new_weights / sum(new_weights)
  # sample column, using available ranges weights
  column <- sample(available_columns, 1,
    prob = new_weights / sum(new_weights)
  )

  mia <- is.na(X[row_id, column])
  row_id <- row_id[!mia]
  # compute E by sampling from exponential distribution with rate sum(ranges)
  E <- stats::rexp(1, rate = sum(new_weights))

  if (length(row_id) == 0 || (E > tau)) {
    return(
      list(
        column = column,
        tau = sum(tau, E),
        rule = NULL,
        left = NULL,
        right = NULL,
        mia_dir = NULL
      )
    )
  }

  tau <- sum(tau, E)
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
  mia_dir <- stats::rbinom(1, size = 1, prob = 0.5)
  if (mia_dir == TRUE) {
    # 1 is for right
    right <- c(right, which(mia))
  } else {
    # 0 is for left
    left <- c(left, which(mia))
  }

  list(
    column = column,
    tau = tau,
    left = left_range,
    right = right_range,
    left_id = left,
    right_id = right,
    mia = mia_dir
  )
}

# find rule - currently only supports numeric columns
mondrian_find_rule <- function(X, row_id, tau = NULL, ranges, ...) {

  # sample from given range
  rule <- sample_from_range(X, row_id, seq_len(length(ranges)), ranges)
  # update ranges and return
  left_ranges <- ranges
  left_ranges[[rule$column]] <- rule$left
  right_ranges <- ranges
  right_ranges[[rule$column]] <- rule$right

  return(
    list(
      tau = rule$tau,
      column = rule$column,
      left_ranges = left_ranges,
      right_ranges = right_ranges,
      left_id = rule$left_id,
      right_id = rule$right_id,
      mia_dir = rule$mia
    )
  )
}

sampler_classifier <- function(X, row_id, ...) {
  table(X[row_id, "Class"]) / length(row_id)
}

mondrian_split <- function(X,
                           row_id,
                           tau = NULL,
                           split_finder = mondrian_find_rule,
                           node_id = 1,
                           parameter_sampler = sampler_classifier,
                           min_nodesize = 15,
                           available_columns = seq_len(ncol(X)),
                           ranges,
                           ...) {
  # move col to within split finder
  rule <- split_finder(X, row_id, tau = tau, ranges)
  # return(rule)
  # reaching a terminal node - you run out of data, or you reach max_depth, or
  # you have a constant column
  if (is.null(rule$column) || length(stats::na.omit(row_id)) <= min_nodesize) {
    # consider a nicer way to denote terminal nodes than just having them be a character
    return(list(
      rule = "terminal_node",
      # but in principle this is fine - we resolve terminal rules
      # pruning rules that have become redundant (ie for each column just
      # take the last available set of rules)
      ranges = ranges, # resolve_terminal_rules(terminal_rules),
      node_id = node_id,
      n = length(row_id),
      parameter_estimates = parameter_sampler(X, row_id, ...)
    ))
  }

  list(
    rule = list(
      column = rule$column,
      rule = distill_rule(rule$left_ranges[rule$column]),
      na_dir = rule$mia_dir,
      tau = rule$tau
    ),
    left = mondrian_split(X,
      row_id = rule$left_id,
      tau = rule$tau,
      split_finder = split_finder,
      # nodes use a simple numbering scheme - the left ones are even
      # while the right ones are odd (hence 2k and 2k+1)
      2 * node_id,
      # pass the rules to the terminal node - this is much easier for
      # things like reconstruction and saves some time
      parameter_sampler = parameter_sampler,
      available_columns = available_columns,
      ranges = rule$left_ranges
    ),
    right = mondrian_split(X,
      row_id = rule$right_id,
      tau = rule$tau,
      split_finder = split_finder,
      2 * node_id + 1,
      parameter_sampler = parameter_sampler,
      available_columns = available_columns,
      ranges = rule$right_ranges
    ),
    node_id = node_id
  )
}
# compute dimension wide min and max and create a 'ranges' object that we
# can update later
compute_ranges <- function(X) {
  ranges <- list()
  if (!is.null(colnames(X))) {
    cols <- colnames(X)
  } else {
    cols <- seq_len(ncol(X))
  }

  for (j in cols) {
    if (is.numeric(X[, j])) {
      ranges[[j]] <- c(min(X[, j], na.rm = TRUE), max(X[, j], na.rm = TRUE))
    } else {
      ranges[[j]] <- c(unique(X[, j]))
    }
  }
  structure(ranges, class = "ranges")
}

ranges_to_weights <- function(ranges) {
  sapply(ranges, function(range) {
    if (is.numeric(range)) {
      range[2] - range[1]
    } else {
      length(range) - 0
    }
  })
}

#' Build a mondrian tree
#' @description Fit a random tree to your data.
#' @param X The data to use - currently only supports a matrix.
#' @param lambda The maximal depth of the tree (though the tree might be shorter - this is an upper bound).
#' @param nosplit_columns Columns to ignore when splitting - but which are nonetheless propagated to
#' the terminal node.
#' @param row_id A subset of rows to use for growing the tree - if **NULL**, use all rows.
#' @param chronological_oob Use only observations which came **after** the training set for errors?
#' @param ... Additional arguments.
#' @return A fitted tree
#' @export
mondrian_tree <- function(X, lambda = NULL,
                          nosplit_columns = NULL, row_id = NULL,
                          chronological_oob = TRUE, ...) {
  available_columns <- colnames(X)
  if (!is.null(nosplit_columns)) {
    available_columns <- available_columns[-parse_nosplit_columns(X, nosplit_columns)]
  }
  if (is.null(row_id)) {
    row_id <- seq_len(nrow(X))
  }
  ranges <- compute_ranges(X[row_id, available_columns])
  result <- list(tree = mondrian_split(X,
    row_id = row_id, tau = lambda, mondrian_find_rule,
    available_columns = available_columns,
    ranges = ranges, ...
  ))
  maybe_oob_obs <- X[list(
    setdiff(seq_len(nrow(X)), row_id),
    which(seq_len(nrow(X)) > max(row_id))
  )[[chronological_oob + 1]], ]
  if (nrow(maybe_oob_obs) > 0) {
    result <- c(oob_obs = maybe_oob_obs)
  }

  structure(
    result,
    class = "mondrian_tree"
  )
}
