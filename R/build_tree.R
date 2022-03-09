# find rule - currently only supports numeric columns
find_rule <- function(X, column) {

  # numeric rules
  if (is.numeric(X[, column])) {
    rule <- stats::runif(1, min(X[, column]), max(X[, column]))

    left <- X[X[, column] <= rule, , drop = FALSE]
    right <- X[X[, column] > rule, , drop = FALSE]

    left_rule <- min(X[, column])
    right_rule <- max(X[, column])
    rule <- list(
      left = c(left_rule, rule),
      right = c(rule, right_rule)
    )
  } else {
    unique_levels <- unique(X[, column])
    # randomly permute all levels
    permutation <- sample(unique_levels, length(unique_levels))
    # find 'position' of rule
    # sample a single level - ensure this is not the rightmost one
    position <- ceiling(stats::runif(1, 0, length(permutation)))
    left_rule <- permutation[seq_len(length(permutation)) <= position]
    right_rule <- permutation[seq_len(length(permutation)) > position]

    left <- X[X[, column] %in% left_rule, , drop = FALSE]
    right <- X[X[, column] %in% right_rule, , drop = FALSE]
    rule <- list(
      left = left_rule,
      right = right_rule
    )
  }

  list(
    column = column,
    rule = rule,
    left = left,
    right = right
  )
}

distill_rule <- function( rule ) {
  if( is.numeric(rule) ) {
    return(max(rule))
  }
  return(rule)
}

add_rule <- function(ruleset = list(),
                     rule = list()) {
  ruleset[[as.character(rule$column)]] <- rule$rule
  return(ruleset)
}

split <- function(X, max_depth = 5, current_depth = 1, node_id = 1,
                  terminal_rules = list(), ...) {
  col <- sample(seq_len(ncol(X)), 1)
  # reaching a terminal node - you run out of data, or you reach max_depth, or
  # you have a constant column
  if (current_depth == max_depth || nrow(X) < 2){ #|| ( length(unique( X[, col] )) < 2)) {
    # consider a nicer way to denote terminal nodes than just having them be a character
    return(list(
      rule = "terminal_node",
      # but in principle this is fine - we resolve terminal rules
      # pruning rules that have become redundant (ie for each column just
      # take the last available set of rules)
      terminal_rules = terminal_rules, # resolve_terminal_rules(terminal_rules),
      node_id = node_id,
      n = nrow(X)
    ))
  }
  rule <- find_rule(X, col)

  list(
    rule = list(
      column = rule$column,
      rule = distill_rule(rule$rule$left)
    ),
    left = split(rule$left,
      max_depth,
      current_depth + 1,
      # nodes use a simple numbering scheme - the left ones are even
      # while the right ones are odd (hence 2k and 2k+1)
      2 * node_id,
      # pass the rules to the terminal node - this is much easier for
      # things like reconstruction and saves some time
      terminal_rules = add_rule(terminal_rules,
        rule = list(
          column = rule$column,
          rule = rule$rule$left
        )
      )
    ),
    right = split(rule$right,
      max_depth,
      current_depth + 1,
      2 * node_id + 1,
      terminal_rules = add_rule(terminal_rules,
        rule = list(
          column = rule$column,
          rule = rule$rule$right
        )
      )
    ),
    node_id = node_id
  )
}

#' Build a random tree
#' @description Fit a random tree to your data.
#' @param X The data to use - currently only supports a matrix.
#' @param max_depth The maximal depth of the tree (though the tree might be shorter - this is an upper bound).
#' @return A fitted tree
#' @export
random_tree <- function(X, max_depth = 10) {
  structure(
    split(X, max_depth = max_depth),
    class = "random_tree"
  )
}
