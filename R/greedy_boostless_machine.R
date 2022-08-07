#' @importFrom rlang .data
greedy_boostless_machine <- function(X,
                                     target = "y",
                                     held_out = 0.2,
                                     stacking_iter = 100,
                                     max_depth = 12,
                                     n_tree = 100,
                                     subsample_size = 2^max_depth,
                                     row_id = NULL,
                                     resample = ifelse(subsample_size < (nrow(X) / n_tree),
                                       TRUE,
                                       FALSE
                                     ),
                                     ...) {

  train_id <- sample(nrow(X), nrow(X) * (1-held_out))
  train <- X[ train_id,]
  test <- X[ -train_id,]
  tictoc::tic("Fitting forest: ")
  forest <- encoder_forest(train,
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
  tictoc::toc()
  tictoc::tic("Predictions: ")
  preds <- predict(forest, test, predictor_mean)
  preds <- dplyr::mutate(preds, tree_id = as.numeric(.data$tree_id))
  tictoc::toc()
  tictoc::tic("Pivot: ")
  preds_wide <- tidyr::pivot_wider(preds,
    id_cols = "id",
    names_from = "tree_id",
    values_from = "V1",
    values_fn = list
  )
  preds_wide <- dplyr::mutate(
    preds_wide,
    dplyr::across(.fns = ~ as.numeric(unlist(.x)))
  )
  preds_wide <- dplyr::arrange(preds_wide, .data$id)
  tictoc::toc()

  tictoc::tic("Stacking:")
  stack_weights <- greedy_stacking(
    y = test[, target],
    Z = as.matrix(dplyr::select(
      preds_wide,
      -.data$id
    )),
    max_iter = stacking_iter
  )
  tictoc::toc()

  return( structure( list( forest = forest,
                           weights = stack_weights ),
                     class = "greedy_boosting_machine"
                     )
          )
}
#' Predict from terminal nodes of a tree in a greedy machine
#'
#' @param object nada
#' @param newdata yada
#' @param type = c('greedy','all')
#' @param ... prada
#' @return results
#' @importFrom stats predict
#' @export
#' @rdname prediction_greedy
predict.greedy_boosting_machine <- function( object,
                                             newdata,
                                             type = c('greedy', 'all'),
                                             ... ) {
  forest <- object$forest
  weights <- object$weights
  if( type[1] == 'greedy' ) {

    nonzero_weights <- which( weights > 0)
    weights <- data.frame( tree_id = seq_len( n_tree(forest) ),
                           weights = weights )

    preds <- predict(forest[nonzero_weights], newdata, predictor_mean)
    preds <- dplyr::mutate(preds, tree_id = as.numeric(.data$tree_id))
    preds <- dplyr::left_join(preds, weights, by = "tree_id")

    preds <- dplyr::group_by(preds, .data$id)
    preds <- dplyr::mutate(preds, pred = .data$weights * .data$V1)
    preds <- dplyr::summarise(preds, pred = sum(.data$pred))
    preds <- dplyr::ungroup(preds)
  }
  else {
    preds <- predict(forest, newdata, predictor_mean)
    preds <- dplyr::select(preds, -.data$tree_id)
    preds <- dplyr::group_by(preds, .data$id)
    preds <- dplyr::mutate(
      preds,
      dplyr::across(.fns = ~ as.numeric(unlist(.x)))
    )
    preds <- dplyr::summarise(preds, dplyr::across(.fns = mean))
    preds <- dplyr::ungroup(preds)
    preds <- dplyr::rename(preds, "pred" = "V1")
  }

  return(preds)
}




