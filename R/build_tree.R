# find rule - currently only supports numeric columns
find_rule <- function(X, row_id, available_columns, ...) {

  column <- sample(available_columns, 1)

  mia <- is.na( X[row_id, column] )
  row_id <- row_id[!mia]
  if( length(row_id) == 0 ) {
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

  # numeric rules
  if (is.numeric(X[, column])) {
    left_rule <- min(X[row_id, column])
    right_rule <- max(X[row_id, column] )

    rule <- stats::runif(1, left_rule, right_rule)
    if( is.na(rule) ) {
      print(mia)
      print(row_id)
      return(list(rule = NULL))
    }

    left <- row_id[X[row_id, column] <= rule]
    right <- row_id[X[row_id, column] > rule]

    rule <- list(
      left = c(left_rule, rule),
      right = c(rule, right_rule)
    )
  } else {
    unique_levels <- unique(X[row_id, column])
    # randomly permute all levels
    permutation <- sample(unique_levels, length(unique_levels))
    # find 'position' of rule
    # sample a single level - ensure this is not the rightmost one
    position <- ceiling(stats::runif(1, 0, length(permutation)))
    left_rule <- permutation[seq_len(length(permutation)) <= position]
    right_rule <- permutation[seq_len(length(permutation)) > position]

    left <- row_id[X[row_id, column] %in% left_rule]
    right <- row_id[X[row_id, column] %in% right_rule]
    rule <- list(
      left = left_rule,
      right = right_rule
    )
  }
  mia_dir <- rbinom( 1, 1, 0.5 )
  # either send mia observations left or right, with equal probability
  if(  mia_dir == TRUE ) {
    # 1 is for right
    right <- c(right, which(mia))
  }
  else {
    # 0 is for left
    left <- c(left, which(mia))
  }

  list(
    column = column,
    rule = rule,
    left = left,
    right = right,
    mia_dir = mia_dir
  )
}
distill_rule <- function(rule) {
  if (is.numeric(rule)) {
    return(max(rule))
  }
  return(rule)
}
add_rule <- function(ruleset = list(),
                     rule = list()) {
  ruleset[[as.character(rule$column)]] <- rule$rule
  return(ruleset)
}
sampler_nada <- function(...) {
  NULL
}
split <- function(X,
                  row_id = seq_len(nrow(X)),
                  max_depth = 5,
                  split_finder = find_rule,
                  current_depth = 0,
                  node_id = 1,
                  terminal_rules = list(),
                  parameter_sampler = sampler_nada,
                  min_nodesize = 15,
                  available_columns = seq_len(ncol(X)),
                  ...) {
  # move col to within split finder
  rule <- split_finder(X, row_id, available_columns)
  # return(rule)
  # reaching a terminal node - you run out of data, or you reach max_depth, or
  # you have a constant column
  if (current_depth == max_depth || length(na.omit(row_id)) <= min_nodesize || is.null( rule$rule )) {
    # consider a nicer way to denote terminal nodes than just having them be a character
    return(list(
      rule = "terminal_node",
      # but in principle this is fine - we resolve terminal rules
      # pruning rules that have become redundant (ie for each column just
      # take the last available set of rules)
      terminal_rules = terminal_rules, # resolve_terminal_rules(terminal_rules),
      node_id = node_id,
      n = length(row_id),
      parameter_estimates = parameter_sampler(X, row_id, ...)
    ))
  }

  list(
    rule = list(
      column = rule$column,
      rule = distill_rule(rule$rule$left),
      na_dir = rule$mia_dir
    ),
    left = split(X,
      rule$left,
      max_depth,
      split_finder = split_finder,
      current_depth + 1,
      # nodes use a simple numbering scheme - the left ones are even
      # while the right ones are odd (hence 2k and 2k+1)
      2 * node_id,
      # pass the rules to the terminal node - this is much easier for
      # things like reconstruction and saves some time
      terminal_rules = add_rule(terminal_rules,
        rule = list(
          column = rule$column,
          rule = rule$rule$left,
          na_dir = rule$mia_dir
        )
      ),
      parameter_sampler = parameter_sampler,
      min_nodesize = min_nodesize,
      available_columns = available_columns,
      ...
    ),
    right = split(X,
      rule$right,
      max_depth,
      split_finder = split_finder,
      current_depth + 1,
      2 * node_id + 1,
      terminal_rules = add_rule(terminal_rules,
        rule = list(
          column = rule$column,
          rule = rule$rule$right,
          na_dir = rule$mia_dir
        )
      ),
      parameter_sampler = parameter_sampler,
      min_nodesize = min_nodesize,
      available_columns = available_columns,
      ...
    ),
    node_id = node_id
  )
}

parse_nosplit_columns <- function(X, nosplit_columns) {
  if (is.numeric(nosplit_columns)) {
    return(nosplit_columns)
  } else {
    return(which(colnames(X) %in% nosplit_columns))
  }
}
#' Build a random tree
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
random_tree <- function(X, max_depth = 10, split_finder = find_rule,
                        nosplit_columns = NULL, row_id = NULL,
                        chronological_oob = TRUE, ...) {
  available_columns <- colnames(X)
  if (!is.null(nosplit_columns)) {
    available_columns <- available_columns[-parse_nosplit_columns(X, nosplit_columns)]
  }
  if(is.null(row_id)) {
    row_id <- seq_len(nrow(X))
  }

  result <- list( tree = split(X, row_id = row_id, max_depth = max_depth, split_finder,
                               available_columns = available_columns, ...)
  )

  maybe_oob_obs <- X[list( setdiff( seq_len(nrow(X)), row_id ),
                           which(seq_len(nrow(X)) > max(row_id))
  )[[chronological_oob + 1]],]

  if( nrow(maybe_oob_obs) > 0) {
    result <- c( result, list(oob_obs = maybe_oob_obs ))
  }

  structure(
    result,
    class = "random_tree"
  )
}
