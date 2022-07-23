# split idea - split to minimise some criterion, e.g. total variance for numeric variable
# so lets have X[,j] a numeric variable, and X[i,j] the observations available at any particular
# split within a tree

# then we can propose k distinct splits, sampled at random from e.g. runif( 1, min(X[i,j]), max(X[i,j]) )
# (which is similar to what we already do for completely random splits)
# but select the 'best' split by e.g. a minimal variance criterion. would this work?

x <- c( rnorm(30, - 3), rnorm(40, 2), rnorm(30, -5))
# x <- (10-(((-49:50)/20) ^2) + rnorm(100))

# set k = 5
splits <- runif( 5, min(x), max(x) )
# evaluate all splits - they create subgroups so we can see variance within subgroups
subgroup_variance <- function( x, split ) {
  lower <- x[ x <= split ]
  upper <- x[ x > split ]
  sum(var(lower),var(upper), na.rm = TRUE)
}

variances <- purrr::map_dbl( splits, ~ subgroup_variance(x, .x))

min_split <- splits[which.min(variances)]

plot(seq_len(length(x)), x)
points( seq_len(length(x))[x <= min_split ], x[ x <= min_split ], col = "blue" )
points( seq_len(length(x))[x > min_split ], x[ x > min_split ], col = "red" )

# for categorcals it might make sense to propose e.g. k groupings... and select the
# most balanced one?

subsample_level_proposals <- function( x, k = 5 ) {
  unique_levels <- unique(x)
  replicate( n = k, {
    # randomly permute all levels
    permutation <- sample(unique_levels, length(unique_levels))
    # find 'position' of rule
    # sample a single level - ensure this is not the rightmost one
    position <- ceiling(stats::runif(1, 0, length(permutation)))
    left_group <- permutation[seq_len(length(permutation)) <= position]
    right_group <- permutation[seq_len(length(permutation)) > position]
    list( left_group, right_group )
  }, simplify = FALSE)
}

subgroup_level_distribution <- function( x, subgroup_left, subgroup_right ) {
  abs( sum(x %in% subgroup_left) - sum(x %in% subgroup_right))
}

find_rule_selfsup <- function(X, row_id, column, k = 5) {

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

    rule <- stats::runif(k, left_rule, right_rule)
    rule_selected <- which.min( sapply( X[row_id,column], subgroup_variance) )
    rule <- rule[rule_selected]

    left <- row_id[X[row_id, column] <= rule]
    right <- row_id[X[row_id, column] > rule]

    rule <- list(
      left = c(left_rule, rule),
      right = c(rule, right_rule)
    )
  } else {

    sampled_level_groups <- subsample_level_proposals( X[row_id, column], k)
    least_disparate_dist <- which.min(sapply( sampled_level_groups,
                      function( i ) {
                        subgroup_level_distribution(X[row_id, column], i[[1]], i[[2]]) }))
    left_rule <- sampled_level_groups[[least_disparate_dist]][[1]]
    right_rule <- sampled_level_groups[[least_disparate_dist]][[2]]
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


