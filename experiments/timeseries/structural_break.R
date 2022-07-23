remove(list=ls())
pkgload::load_all()
library(magrittr)
source("../average_tile.R")
# motivate structural breaks
set.seed(1071)

y <- 3000 + arima.sim( model = list(3,0,2), n = 1000 ) * 1000 + rnorm(1000, sd = 700)

y[ 370:630 ] <- rbinom( length(y[ 370:630 ]), size = 1, prob = 0.01 ) * y[ 370:630 ]

train_df <- data.frame(y = c(y)) %>%
  dplyr::mutate( lag_1 = dplyr::lag(y, 1),
                 lag_2 = dplyr::lag(y, 2),
                 lag_3 = dplyr::lag(y, 3),
                 lag_4 = dplyr::lag(y, 4),
                 lag_5 = dplyr::lag(y, 5),
                 lag_6 = dplyr::lag(y, 6),
                 lag_7 = dplyr::lag(y, 7),
                 time_index = seq_len(length(y)))
train <- train_df[1:900,]
test <- train_df[901:1000,]


parameter_sampler_ar <- function( X, row_id, ... ) {

  safe_lm <- purrr::safely( lm, otherwise = NULL )
  model <- safe_lm( y ~ ., data = X[row_id,])

  if(!is.null(model[["result"]])) {
    return(
      coef(model[["result"]], complete = TRUE)
    )
  }
  rep(NA, ncol(X)+1)
}

sampler_forest <- encoder_forest( train,
                                  max_depth = 3,
                                  n_tree = 50,
                                  subsample_size = 100,
                                  nosplit_columns = c("y", "lag_1",
                                                      "lag_2", "lag_3",
                                                      "lag_4", "lag_5",
                                                      "lag_6", "lag_7"),
                                  resample = TRUE,
                                  row_id = 1:800,#44890,
                                  min_nodesize = 15,
                                  parameter_sampler = parameter_sampler_ar )

predictor_ar <- function(X, row_id, leaf_node, ...) {
  samples <- leaf_node$parameter_estimates
  X <- subset( X, ( 1:nrow(X) == row_id), -y)
  X <- cbind(1, X)[,seq_len(length(samples))]
  # X <- cbind(1, X[row_id,][, c( "lag_1", "lag_2", "lag_3",
  #                               "lag_4", "lag_5", "time_index"
  # )])

  miss_cols <- which(is.na(samples))
  if (length(miss_cols) > 0) {
    return(list(predictions = matrix(as.matrix(X[, -miss_cols]) %*% samples[-miss_cols], ncol = 1)))
  }

  list(predictions = matrix(as.matrix(X) %*% samples, ncol = 1))
}

scoring_resid_ar <- function(X, id, pred) {
  y_hat <- as.numeric(unlist(pred))
  y <- as.numeric(unlist(X[id, "true_y", drop = TRUE]))
  c(y - y_hat)
}

uncertainty_to_conf_int <- function( y, uncertainty, delta = 0.1 ) {
  upper_bound <- quantile(as.numeric(unlist(uncertainty)), 1-(delta/2), na.rm = TRUE)
  lower_bound <- quantile(as.numeric(unlist(uncertainty)), delta/2, na.rm = TRUE )
  return( c( y+lower_bound, y, y+upper_bound ) )
}

rmse <- function( y, y_hat ) {
  sqrt( mean( ((y-y_hat)^2) ))
}

mape <- function( y, y_hat ) {
  mean( abs( (y-y_hat)/y), na.rm = TRUE)
}

forecast_empirical_coverage <- function( y, forecast_mat ) {
  na_forecasts <- which(!is.na(forecast_mat[,2]))
  sum(
    y[na_forecasts] <= forecast_mat[na_forecasts,3] &
      y[na_forecasts] >= forecast_mat[na_forecasts,1],
    na.rm = TRUE
  )/length(y[na_forecasts])
}

iterative_forecaster_ar <- function( tree, oob_obs, predict_fun, scoring_fun, ... ) {

  new_tree <- tree
  oob_obs[["true_y"]] <- oob_obs[["y"]]

  available_obs <- sample( seq_len(nrow(oob_obs)), min(c(nrow(oob_obs), 10)))
  for( observation in available_obs ) {
    curr_test <- oob_obs[observation,]
    curr_test[["true_y"]] <- oob_obs[observation,"y"]

    # current_forecasts <- observation:nrow(oob_obs)
    for( i in observation:min(observation+5, nrow(oob_obs)) ) {
      # compute all forecasts iteratively for given observation
      new_tree <- new_tree$tree
      new_tree <- extend_oob_score_tree( new_tree,
                                         curr_test,
                                         predict_fun = predict_fun,
                                         scoring_fun = scoring_fun,
                                         ...)
      forecast <- unname(unlist(predict( new_tree, curr_test, predict_fun )$predictions))
      # rssss <<- forecast
      curr_test <- data.frame( y = NA,
                               lag_1 = forecast,
                               lag_2 = curr_test[["lag_1"]],
                               lag_3 = curr_test[["lag_2"]],
                               lag_4 = curr_test[["lag_3"]],
                               lag_5 = curr_test[["lag_4"]],
                               lag_6 = curr_test[["lag_5"]],
                               lag_7 = curr_test[["lag_6"]],
                               time_index = curr_test[["time_index"]] + 1,
                               true_y = oob_obs[observation + i,"y"])
    }
  }
  return(new_tree)
}

tictoc::tic("Uncertainty estimated in:")
sampler_forest <- iterative_extend_oob_score(sampler_forest,
                                             predict_fun = predictor_ar,
                                             scoring_fun = scoring_resid_ar,
                                             iterative_forecast_fun = iterative_forecaster_ar)
tictoc::toc()

predictor_ar <- function( X, row_id, tree, ... ) {
  samples <- tree$parameter_estimates
  X <- subset( X, ( 1:nrow(X) == row_id), -y)
  X <- cbind(1, X)[,seq_len(length(samples))]
  miss_cols <- which(is.na(samples))
  uncertainty <- if(is.null(tree$uncertainty_scores)) {
    Inf
  } else {
    tree$uncertainty_scores
  }
  if(length(miss_cols) > 0) {
    return( list( predictions = matrix( as.matrix(X[,-miss_cols]) %*% samples[-miss_cols], ncol = 1),
                  uncertainty = uncertainty))
  }
  list( predictions = matrix( as.matrix(X) %*% samples, ncol = 1),
        uncertainty = uncertainty)
}

lm_benchmark <- function( X, target = "y") {
  safe_lm <- purrr::safely( lm, otherwise = NULL )
  frml <- as.formula( paste0(target,"~.") )
  model <- safe_lm( frml, data = X)
  return(model[["result"]])
}

mdl_lm <- lm_benchmark(train_df)





tictoc::tic("Validation loop done in:")
rmsse_y_scale <- mean(train$y^2, na.rm = TRUE)

plot_refresh_rate <- 5

n_validation_sets <- 40
validation_result <- list()
for( validation_set in seq_len(n_validation_sets) ) {
  print(paste0("Validation set:",validation_set))

  forecast_size = 30
  start_id = sample.int( nrow(test)-forecast_size-1, 1 )
  plot.ts(test$y[start_id:(start_id+forecast_size-1)], ylim = c(min(train$y)-0.5*sd(train$y),max(train$y)+0.5*sd(train$y)))

  forecast <- matrix( rep(NA, forecast_size*5), ncol = 5)
  curr_test <- test[start_id,]
  one_step_forecast <- matrix( rep(NA, forecast_size*3), ncol = 3)
  for( i in seq_len(forecast_size)){
    print(paste0("Step:",i))
    pred <- predict( sampler_forest, curr_test, predict_fun = predictor_ar)
    forecast[i,] <- c( uncertainty_to_conf_int( median( as.numeric(pred$predictions), na.rm = TRUE ), pred$uncertainty),
                       median( as.numeric(pred$predictions), na.rm = TRUE ) + median( as.numeric(unlist(pred$uncertainty)),
                                                                                      na.rm = TRUE ),
                       # predictions from a naive linear model with the same features
                       predict(mdl_lm, curr_test)
    )

    curr_test <- data.frame( y = NA,
                             lag_1 = forecast[i,2],
                             lag_2 = curr_test[["lag_1"]],
                             lag_3 = curr_test[["lag_2"]],
                             lag_4 = curr_test[["lag_3"]],
                             lag_5 = curr_test[["lag_4"]],
                             lag_6 = curr_test[["lag_5"]],
                             lag_7 = curr_test[["lag_6"]],
                             time_index = curr_test[["time_index"]] + 1
    )

    if( (i %% plot_refresh_rate) == 0 ) {
      lines(forecast[,2],col = "red")
      lines(forecast[,4],col = "green")
      lines(forecast[,1], col = "grey")
      lines(forecast[,3], col = "grey")
    }
  }
  validation_result[[as.character(validation_set)]] <- c( rmse = rmse( test$y[ (start_id+1): (start_id+i-1) ],
                                                                       forecast[1:(i-1),2]
                                                                       ),
                                                          adjusted_rmse = rmse( test$y[ (start_id+1): (start_id+i-1) ],
                                                                                 forecast[1:(i-1),4]
                                                                                 ),
                                                          mape = mape( test$y[ (start_id+1): (start_id+i-1) ],
                                                                       forecast[1:(i-1),2]),
                                                          coverage = forecast_empirical_coverage(
                                                            test$y[ (start_id+1):(i-1) ],
                                                            forecast[1:(i-1),] ),
                                                          naive_forecast_rmse = rmse(
                                                            test$y[ start_id:(i-1) ],
                                                            mean(train$y)),
                                                          # average_tile_rmsse = rmsse(
                                                          #   test$y[ start_id:(start_id +i-1) ],
                                                          #   average_tile(train$y) %>%
                                                          #     forecast.average_tile( h = 1900 ) %>%
                                                          #     .[["y"]],
                                                          #   rmsse_y_scale ),
                                                          lm_rmse = rmse( test$y[ (start_id+1): (start_id+i-1) ],
                                                                          forecast[1:(i-1),5]
                                                                          )
  )
}

dplyr::bind_rows(purrr::map( validation_result, ~ .x[ c(1,2,5,6) ])) %>% as.matrix %>% tsutils::nemenyi(plottype = "vmcb")

tictoc::toc()

