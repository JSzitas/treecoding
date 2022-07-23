source("../average_tile.R")
predictor_ar <- function(X, row_id, leaf_node, ...) {
  samples <- leaf_node$parameter_estimates
  X <- cbind(1, X[row_id,][, c( "Holiday", "demand_lag_1", "demand_lag_2", "demand_lag_3",
                            "demand_lag_4", "demand_lag_5", "time_index", "sin_48", "cos_48",
                            "sin_336", "cos_336"#, "sin_1344", "cos_1344"
                            )])

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

rmsse <- function( y, y_hat, y_scale ) {
  sqrt( mean( ((y-y_hat)^2)/y_scale ))
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
  oob_obs[["true_y"]] <- oob_obs[["Demand"]]

  available_obs <- sample( seq_len(nrow(oob_obs)), min(c(nrow(oob_obs), 10)))
  for( observation in available_obs ) {
    curr_test <- oob_obs[observation,]
    curr_test[["true_y"]] <- oob_obs[observation,"Demand"]

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
        curr_test <- data.frame( Demand = NA,
                                 # Temperature = oob_obs$Temperature[i],
                                 Holiday = oob_obs$Holiday[i],
                                 demand_lag_1 = forecast,
                                 demand_lag_2 = curr_test[["demand_lag_1"]],
                                 demand_lag_3 = curr_test[["demand_lag_2"]],
                                 demand_lag_4 = curr_test[["demand_lag_3"]],
                                 demand_lag_5 = curr_test[["demand_lag_4"]],
                                 time_index = curr_test[["time_index"]] + 1,
                                 sin_48 = oob_obs$sin_48[i],
                                 cos_48 = oob_obs$cos_48[i],
                                 sin_336 = oob_obs$sin_336[i],
                                 cos_336 = oob_obs$cos_336[i],
                                 # sin_1344 = oob_obs$sin_1344[i],
                                 # cos_1344 = oob_obs$cos_1344[i],
                                 true_y = oob_obs[observation + i,"Demand"])
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
  X <- subset( X, ( 1:nrow(X) == row_id), -Demand)
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

lm_benchmark <- function( X ) {
  safe_lm <- purrr::safely( lm, otherwise = NULL )
  model <- safe_lm( Demand ~ ., data = X)
  return(model[["result"]])
}

mdl_lm <- lm_benchmark(train)






tictoc::tic("Validation loop done in:")
rmsse_y_scale <- mean(train$Demand^2, na.rm = TRUE)

plot_refresh_rate <- 5

n_validation_sets <- 20
validation_result <- list()
for( validation_set in seq_len(n_validation_sets) ) {

  forecast_size = 30
  start_id = sample.int( nrow(test)-forecast_size-1, 1 )
  plot.ts(test$Demand[start_id:(start_id+forecast_size-1)], ylim = c(2000,10000))

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

    curr_test <- data.frame( Demand = NA,
                             # Temperature = test$Temperature[i],
                             Holiday = test$Holiday[i],
                             demand_lag_1 = forecast[i,2],
                             demand_lag_2 = curr_test[["demand_lag_1"]],
                             demand_lag_3 = curr_test[["demand_lag_2"]],
                             demand_lag_4 = curr_test[["demand_lag_3"]],
                             demand_lag_5 = curr_test[["demand_lag_4"]],
                             time_index = curr_test[["time_index"]] + 1,
                             sin_48 = test$sin_48[i],
                             cos_48 = test$cos_48[i],
                             sin_336 = test$sin_336[i],
                             cos_336 = test$cos_336[i]#,
                             # sin_1344 = test$sin_1344[i],
                             # cos_1344 = test$cos_1344[i]
                             )

    if( (i %% plot_refresh_rate) == 0 ) {
      lines(forecast[,2],col = "red")
      lines(forecast[,4],col = "green")
      lines(forecast[,1], col = "grey")
      lines(forecast[,3], col = "grey")
    }
  }
  validation_result[[as.character(validation_set)]] <- c( rmsse = rmsse( test$Demand[ (start_id+1): (start_id+i-1) ],
                                                                         forecast[1:(i-1),2],
                                                                         rmsse_y_scale ),
                                                          adjusted_rmsse = rmsse( test$Demand[ (start_id+1): (start_id+i-1) ],
                                                                                  forecast[1:(i-1),4],
                                                                                  rmsse_y_scale ),
                                                          mape = mape( test$Demand[ (start_id+1): (start_id+i-1) ],
                                                                        forecast[1:(i-1),2]),
                                                          coverage = forecast_empirical_coverage(
                                                            test$Demand[ (start_id+1):(i-1) ],
                                                            forecast[1:(i-1),] ),
                                                          naive_forecast_rmsse = rmsse(
                                                            test$Demand[ start_id:(i-1) ],
                                                            mean(train$Demand),
                                                            rmsse_y_scale ),
                                                          average_tile_rmsse = rmsse(
                                                            test$Demand[ start_id:(start_id +i-1) ],
                                                            average_tile(train$Demand) %>%
                                                              forecast.average_tile( h = 1900 ) %>%
                                                              .[["y"]],
                                                            rmsse_y_scale ),
                                                          rmsse_lm = rmsse( test$Demand[ (start_id+1): (start_id+i-1) ],
                                                                            forecast[1:(i-1),5],
                                                                            rmsse_y_scale )
                                                          )
}

dplyr::bind_rows(purrr::map( validation_result, ~ .x[ c(1,2,5,6,7) ])) %>% as.matrix %>% tsutils::nemenyi(plottype = "vmcb")

tictoc::toc()


