traversal_oob_extension <- function(tree, X, id = seq_len(nrow(X)), predict_fun, scoring_fun, ...) {
  if (is.character(tree$rule)) {
    if (length(id) != 0) {
      pred <- predict_fun(X, id, tree)
      # tree$parameter_estimates <- c(tree$parameter_estimates, scores = list())
      if (is.null(tree$uncertainty_scores)) {
        tree <- c(tree, list(uncertainty_scores = scoring_fun(X, id, pred)))
      } else {
        tree[["uncertainty_scores"]] <- c(tree[["uncertainty_scores"]], scoring_fun(X, id, pred))
      }
    }
    return(tree)
  }
  # if the column is of type numeric
  if (is.numeric(tree$rule$rule)) {
    left_going <- id[X[id, tree$rule$column] <= tree$rule$rule]
    right_going <- id[X[id, tree$rule$column] > tree$rule$rule]
  }
  # if column is of a different type - ie character
  else {
    left_going <- id[X[id, tree$rule$column] %in% tree$rule$rule]
    right_going <- id[!(X[id, tree$rule$column] %in% tree$rule$rule)]
  }
  # the NAs follow the na na_dir of the fitted tree node - thus the rule is that
  # the values either fall in the set of the rule, or they are NA (and then they
  # get sent to a child node according to na_dir)
  # if NAs go left - add them, otherwise they should be picked automatically
  if (!as.logical(tree$rule$na_dir)) {
    left_going <- c(left_going, id[is.na(X[id, tree$rule$column])])
  }
  list(
    rule = tree$rule,
    left = traversal_oob_extension(tree$left, X, id = left_going, predict_fun, scoring_fun, ...),
    right = traversal_oob_extension(tree$right, X, id = right_going, predict_fun, scoring_fun, ...),
    node_id = tree$node_id
  )
}

extend_oob_score_tree <- function(tree, X, id = seq_len(nrow(X)), predict_fun, scoring_fun, ...) {
  structure(
    list(
      tree = traversal_oob_extension(tree, X, id = seq_len(nrow(X)), predict_fun, scoring_fun, ...)
    ),
    class = "random_tree"
  )
}

extend_oob_score <- function(forest, predict_fun, scoring_fun, ...) {
  structure(
    list(
      forest = furrr::future_map(
        forest[["forest"]],
        function(tree) {
          # compute oob scores for tree on oob samples
          extend_oob_score_tree(tree$tree,
            tree$oob_obs,
            id = seq_len(nrow(tree$oob_obs)),
            predict_fun,
            scoring_fun,
            ...
          )
        },
        .options = furrr::furrr_options(seed = TRUE)
      ),
      Phi = forest[["Phi"]]
    ),
    class = "encoder_forest"
  )
}

iterative_extend_oob_score <- function(forest, predict_fun, scoring_fun, iterative_forecast_fun, ...) {
  structure(
    list(
      forest = furrr::future_map(
        forest[["forest"]],
        function(tree) {
          iterative_forecast_fun(tree,
            tree$oob_obs,
            predict_fun = predict_fun,
            scoring_fun = scoring_fun,
            ...
          )
        },
        .options = furrr::furrr_options(seed = TRUE)
      ),
      Phi = forest[["Phi"]]
    ),
    class = "encoder_forest"
  )
}
