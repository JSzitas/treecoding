# boosting
loss_evalutor_sampler_mean <- function(pred, X, learning_rate, target_cols) {
  dplyr::bind_cols(
    purrr::map2(
      X[, c(target_cols)],
      pred[, target_cols],
      ~ .y - learning_rate * .x
    )
  )
}

booster <- function(X,
                    max_depth = 4,
                    max_iterations = 500,
                    learning_rate = 0.03,
                    subsample_size = 2^max_depth,
                    target_cols = target_cols,
                    predict_fun = predictor_identity,
                    oob_gradient = TRUE,
                    loss_evaluator = loss_evalutor_sampler_mean,
                    ...) {
  iter <- 1
  boosts <- list()
  preds <- list()
  repeat{
    id <- sample(seq_len(nrow(X)),
      size = subsample_size
    )

    tree <- random_tree(X[id, ], max_depth = max_depth, ...)

    if (oob_gradient) {
      pred <- predict(tree, X[-id, ], predict_fun)
      res <- loss_evaluator(pred, X[-id, ], learning_rate, target_cols)
      # X[ nosplit_columns, ] <-
    } else {
      pred <- predict(tree, X, predict_fun)
      res <- loss_evaluator(pred, X, learning_rate)
    }
    preds[[iter]] <- pred
    boosts[[iter]] <- tree

    if (iter > max_iterations) break
    iter <- iter + 1
  }

  return(structure(list(
    predictions = preds,
    boosts = boosts
  ),
  class = "booster"
  ))
}
