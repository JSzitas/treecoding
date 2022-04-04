#' Encoder forest
#' @description Fit an encoder forest to your data
#' @param X The data to use.
#' @param max_depth The maximal depth of the tree.
#' @param n_tree The number of trees used.
#' @param subsample_size The number of rows allocated to each tree.
#' @param resample Whether to resample rows when creating samples of data for trees.
#' @param ... Additional arguments (for future compatibility).
#' @return A fitted encoder forest
#' @export
encoder_forest <- function(X,
                           max_depth = 12,
                           n_tree = 1000,
                           subsample_size = 2^max_depth,
                           resample = ifelse(subsample_size < (nrow(X) / n_tree),
                                             TRUE,
                                             FALSE
                           ),
                           ...) {
  if (resample) {
    forest <- furrr::future_map(
      seq_len(n_tree),
      function(i) {
        # fit a tree on a random sample
        random_tree(X[sample(seq_len(nrow(X)),
                             size = subsample_size
        ), ],
        max_depth = max_depth,
        ...
        )
      },.options = furrr::furrr_options(seed = TRUE)
    )
  } else {
    if(is.null(subsample_size)) {
      tree_folds <- lapply( seq_len(n_tree),
                            function(sample){ seq_len(nrow(X))} )
    }
    else {
      tree_folds <- resample_folds(nrow(X), subsample_size)
    }
    forest <- furrr::future_map(tree_folds, function(fold) {
      # fit a tree on a predetermined fold
      random_tree(X[fold, ],
                  max_depth = max_depth,
                  ...
      )
    }, .options = furrr::furrr_options(seed = TRUE))
  }
  names(forest) <- seq_len(length(forest))
  structure( list( forest = forest,
                   Phi = 2^max_depth),
             class = "encoder_forest")
}
