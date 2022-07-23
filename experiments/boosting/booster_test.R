rm(list=ls())
pkgload::load_all()

df <- qs::qread( "../soothsayer/local_data/oracle_weighed_top_5.qs" ) %>%
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

parameter_sampler_mean <- function(X, row_id, ...) {
  setNames( colMeans(X[row_id, target_cols], na.rm = TRUE),
            target_cols)
}

predictor_identity_multi <- function(X, row_id, samples, ...) {
  res <- matrix(c(samples), nrow = length(row_id), ncol = length(samples), byrow = TRUE)
  colnames(res) <- target_cols
  return(res)
}

res <- booster( train,
                nosplit_columns = target_cols,
                subsample_size = 512,
                parameter_sampler = parameter_sampler_mean,
                predict_fun = predictor_identity_multi )




