traversal_sampling_vec <- function(tree, X, id = seq_len(nrow(X)), predict_fun) {
  if( length(na.omit(id)) == 0 || all(is.na(id))  ){ return() }
  if (is.character(tree$rule)) {
    pred <- predict_fun( X, id, tree)
    if( is.null(pred) || length(pred) == 0 ) pred <- rep(NA, length(id))
    return( suppressWarnings( cbind( id = id, as.data.frame(t(cbind( pred ))) )))
  }
  # find mia rows
  mia <- is.na( X[id, tree$rule$column] )
  id <- id[!mia]

  # if the column is of type numeric
  if (is.numeric(tree$rule$rule)) {
    left_going <- id[ X[,tree$rule$column][id] <= tree$rule$rule ]
    right_going <- id[ X[,tree$rule$column][id] > tree$rule$rule ]
  }
  # if column is of a different type - ie character
  else {
    left_going <- id[ X[,tree$rule$column][id] %in% tree$rule$rule ]
    right_going <- id[ !(X[,tree$rule$column][id] %in% tree$rule$rule) ]
  }
  # the NAs follow the na na_dir of the fitted tree node - thus the rule is that
  # the values either fall in the set of the rule, or they are NA (and then they
  # get sent to a child node according to na_dir)
  # if NAs go left - add them, otherwise they should be picked automatically
  if( !as.logical(tree$rule$na_dir) ) {
    left_going <- c( left_going, which(mia))#id[ is.na(X[id, tree$rule$column])])
  }
  else {
    right_going <- c(right_going, which(mia))
  }

  data.table::rbindlist( list( traversal_sampling_vec(tree$left, X, id = left_going, predict_fun ),
                               traversal_sampling_vec(tree$right, X, id = right_going, predict_fun )
  ),
  fill = TRUE
  )
}

predictor_nada <- function( X, row_id, samples, ... ) NA
predictor_identity <- function( X, row_id, samples, ... ) matrix( rep( samples, length(row_id)), nrow = length(row_id), byrow = TRUE)
predictor_identity_simple <- function( X, row_id, samples, ... ) samples
#' Predict from terminal nodes of a tree
#'
#' @param x nada
#' @param newdata yada
#' @param predict_fun lada
#' @param ... prada
#' @return results
#' @importFrom stats predict
#' @export
#' @rdname prediction
predict.random_tree <- function( x, newdata, predict_fun = predictor_identity, ... ) {
  traversal_sampling_vec( tree = x[["tree"]],
                          X = newdata,
                          id = seq_len(nrow(newdata)),
                          predict_fun = predict_fun)
}
#' @export
#' @rdname prediction
predict.encoder_forest <- function( x, newdata, predict_fun = predictor_identity, ... ) {
  smpls <- purrr::imap( x[["forest"]],
                        ~ cbind( tree_id = .y,
                                 predict( tree = .x,
                                          newdata = newdata,
                                          predict_fun = predict_fun,
                                          ...)
                        )
  )
  data.table::rbindlist( smpls, fill = TRUE )
}
