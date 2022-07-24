
parameter_sampler_mean <- function(X, row_id, target_name, ...) {
  purrr::safely(mean, otherwise = NA)(X[row_id, ][, target_name])$result
}

predictor_mean <- function(X, row_id, tree, ...) {
  tree$parameter_estimates
}

boostless_forest <- function(X,
                             target = "y",
                             max_depth = 12,
                             n_tree = 100,
                             epochs = 5,
                             subsample_size = 2^max_depth,
                             row_id = NULL,
                             resample = ifelse(subsample_size < (nrow(X) / n_tree),
                               TRUE,
                               FALSE
                             ),
                             ...) {

  # compute baseline loss before we start boosting
  baseline_loss <- mse(X[[target]], mean(X[[target]]))


  for (epoch in epochs) {
    boostless_trees <- encoder_forest(X,
      max_depth = max_depth,
      n_tree = n_tree,
      subsample_size = subsample_size,
      nosplit_columns = target,
      resample = resample,
      row_id = row_id,
      parameter_sampler = parameter_sampler_mean,
      target_name = target,
      ...
    )
    return(boostless_sampler(boostless_trees, target, baseline_loss = baseline_loss))
  }
}

mse <- function(x, y) {
  mean((x - y)^2)
}


boostless_sampler <- function(forest, target = "y", acceptance_rate = 0.3, shrink = 0.03, baseline_loss = 1) {

  # sample a tree index
  tree_id <- sample(seq_len(length(forest[["forest"]])), size = 1)
  # take tree
  tree <- forest[["forest"]][[tree_id]]
  # remove tree from forest
  forest[["forest"]][[tree_id]] <- NULL
  forest[["forest"]] <- purrr::compact(forest[["forest"]])
  # compute oob errors for tree
  preds <- predict(tree, tree[["oob_obs"]], predictor_mean)
  preds <- preds[order(preds$id), ]
  loss <- mse(tree[["oob_obs"]][[target]], preds$V1)

  return(list(baseline = baseline_loss, loss = loss))
  return(loss)
}
