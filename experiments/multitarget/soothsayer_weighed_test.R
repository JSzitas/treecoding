# if file does not exist, download it first
remove(list = ls())
pkgload::load_all(export_all = TRUE)

df <- qs::qread( "../soothsayer/local_data/oracle_weighed.qs") %>% #_top_5.qs" ) %>%
  as.data.frame()


# get top 5 for each series


samples_by_key <- function(df, key, train_size = 0.7, drop_key = TRUE) {
  unique_keys <- unique(df[[key]])
  train_keys <- sample(unique_keys, train_size * length(unique_keys))
  test_keys <- setdiff(unique_keys, train_keys)

  train <- df[df[[key]] %in% train_keys, ]
  test <- df[df[[key]] %in% test_keys, ]
  if (drop_key) {
    train <- train[, -which(colnames(train) %in% key)]
    test <- test[, -which(colnames(test) %in% key)]
  }

  return(list(
    train = train,
    test = test
  ))
}
res <- samples_by_key(df, "key")
train <- res$train
test <- res$test

target_cols <- c(
"target_ar", "target_ar1", "target_ar3", "target_arima", "target_arma11",
"target_bats", "target_croston", "target_ets", "target_nnetar",
"target_rw", "target_tbats", "target_theta")

# do the ica bit
# ica_res <- ica::icafast( train[, target_cols], 11, center = FALSE )

# ica_targets <- as.data.frame(ica_res[["S"]])
# colnames( ica_targets ) <- paste0( "ica_target_", seq_len(ncol(ica_targets)) )

mu <- colMeans(train[, target_cols])
Xpca <- prcomp(train[, target_cols])
pca_targets <- predict(Xpca, train)

train2 <- train %>%
  dplyr::select( -target_cols) %>%
  dplyr::bind_cols( pca_targets)#ica_targets )

target_cols <- paste0( "pca_targets",#"ica_target_",
                       seq_len(ncol(pca_targets)) )

parameter_sampler_mean <- function(X, row_id, ...) {
  setNames( colMeans(X[row_id, target_cols], na.rm = TRUE),
            target_cols)
}

rmse <- function(x, y) {
  sqrt(mean((x - y)^2, na.rm = TRUE))
}

mae <- function(x,y) {
  mean(abs(x-y), na.rm = TRUE)
}
random_benchmark <- function(train_y, size = 10) {
  rnorm(size, mean(train_y, na.rm = TRUE), sd(train_y, na.rm = TRUE))
}

predictor_identity_multi <- function(X, row_id, leaf_node, ...) {
  samples <- leaf_node$parameter_estimates
  list( predictions = matrix( samples, nrow = 1, ncol = length(samples) ))
}


tictoc::tic()
forest <- encoder_forest(train2, 8,
                         n_tree = 100,
                         resample = TRUE,
                         nosplit_columns = target_cols,
                         parameter_sampler = parameter_sampler_mean)
tictoc::toc()
predictions_on_test_forest <- predict(forest, test, predict_fun = predictor_identity_multi)
#
merge_predictions <- function( pred ) {

  preds <- predictions_on_test_forest %>%
    dplyr::select(predictions) %>%
    as.list() %>%
    .[["predictions"]] %>%
    purrr::map( ~ data.frame(matrix(unlist(.x), nrow = 1) )) %>%
    dplyr::bind_rows()
  res <- dplyr::bind_cols( dplyr::select(pred, tree_id, id), preds ) %>%
    dplyr::select(-tree_id) %>%
    dplyr::group_by(id) %>%
    dplyr::summarise( dplyr::across(.fns = mean,
                                    na.rm = TRUE),
                      .groups = "keep" ) %>%
    dplyr::ungroup()
  colnames(res) <- c("id", target_cols)
  return(res)
}

# res_rmse <- purrr::map2( test[, c(target_cols)], test_preds[, ..target_cols], ~ rmse(unlist(.x), unlist(.y)))
# # ntree = 50 >>
# # c(target_ar = 0.101756085589789, target_ar1 = 0.103312832542497,
# #   target_ar3 = 0.105020115033758, target_arima = 0.105190565620078,
# #   target_arma11 = 0.0962359264181502, target_bats = 0.102531820264145,
# #   target_croston = 0.0957052881433364, target_ets = 0.105586245161985,
# #   target_nnetar = 0.0985701330488181, target_rw = 0.096179902806649,
# #   target_tbats = 0.102531820264145, target_theta = 0.104553462210102
# # )
# res_mae <- purrr::map2( test[, c(target_cols)], test_preds[, ..target_cols], ~ mae(unlist(.x), unlist(.y)))
# # ntree = 50 >>
# # c(target_ar = 0.0943762252452988, target_ar1 = 0.0971471843356407,
# #   target_ar3 = 0.101522906937925, target_arima = 0.097530425318246,
# #   target_arma11 = 0.0894338363214792, target_bats = 0.096062375754972,
# #   target_croston = 0.0888531660984238, target_ets = 0.098872128947921,
# #   target_nnetar = 0.093897288021716, target_rw = 0.0885981072996365,
# #   target_tbats = 0.096062375754972, target_theta = 0.100099032506311
# # )
# random_rmse <- purrr::map( test[, c(target_cols)],~ rmse( runif( nrow(test), min(.x), max(.x) ), .) )



