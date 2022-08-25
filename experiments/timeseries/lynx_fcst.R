remove(list=ls())
pkgload::load_all(compile = FALSE)
library(magrittr)
tictoc::tic(msg = paste0( "Fit done at:", Sys.time() ))

add_lags <- function( x, lags = 1:2 ) {
  dplyr::bind_cols( x, purrr::map( lags, ~ dplyr::lag(x, n = .x)) )
}

df <- lynx %>%
  c() %>%
  as.data.frame() %>%
  purrr::map( add_lags ) %>%
  as.data.frame() %>%
  setNames( nm = c( "y", paste0("y_lag_", 1:2))) %>%
  dplyr::bind_cols( data.frame(
    trigonometric_seasonal_dummy( length(lynx),
                                  find_seasonalities(lynx[1:80])
                                  )
    )
  ) %>%
  dplyr::mutate( index = seq_len(length(lynx)) )

train <- df[1:80,]
test <- df[81:114,]

parameter_sampler_ar <- function( X, row_id, ... ) {

  safe_lm <- purrr::safely( lm, otherwise = NULL )
  model <- safe_lm( y ~ y_lag_1 + y_lag_2 #+ y_lag_3
                     # + y_lag_4 + y_lag_5
                    , data = X[row_id,])

  if(!is.null(model[["result"]])) {
    return(
      coef(model[["result"]], complete = TRUE)
    )
  }
  rep(NA, ncol(X)+1)
}

sampler_forest <- encoder_forest( train,
                                  max_depth = 3,
                                  n_tree = 4000,
                                  min_nodesize = 0.20*nrow(train),
                                  subsample_size = nrow(train) * 0.75,#632,
                                  nosplit_columns = c( "y", paste0("y_lag_", 1:2)),
                                  resample = TRUE,
                                  row_id = NULL,
                                  parameter_sampler = parameter_sampler_ar )

tictoc::toc()

predictor_ar <- function( X, row_id, tree, ... ) {
  samples <- tree$parameter_estimates
  if( length(samples) == 0 ){
    return(NULL)
  }
  X <- subset( X, ( 1:nrow(X) == row_id), -y)
  X <- cbind(1, X)[,seq_len(length(samples))]
  miss_cols <- which(is.na(samples))
  if(length(miss_cols) > 0) {
    return(
      matrix( as.matrix(X[,-miss_cols]) %*% samples[-miss_cols], ncol = 1))
    }
  matrix( as.matrix(X) %*% samples, ncol = 1)
}

lm_benchmark <- function( X ) {
  safe_lm <- purrr::safely( lm, otherwise = NULL )
  model <- safe_lm( y ~ ., data = X)
  return(model[["result"]])
}

mdl_lm <- lm_benchmark(train)
source("../average_tile.R")
tile_fcst <- average_tile(train$y) %>%
  forecast.average_tile( h = nrow(test) ) %>%
  .[["y"]]

rmsse <- function( y, y_hat, y_scale ) {
  sqrt( mean( ((y-y_hat)^2)/y_scale ))
}
rmse <- function( y, y_hat ) {
  sqrt( mean( ((y-y_hat)^2) ))
}
rmsse_y_scale <- mean(train$y^2, na.rm = TRUE)
plot.ts(c(train$y,test$y), ylim = c(-3000,13000))
tictoc::tic("Forecast timng: ")
forecast <- matrix( rep(NA, 34*3), ncol = 3)
curr_test <- test[1,]
for( i in seq_len(nrow(test))){
  print(paste0("Step:",i))
  pred <- predict( sampler_forest, curr_test, predict_fun = predictor_ar)
  forecast[i,] <- c( median(pred$V1, na.rm = TRUE),
                     mean(pred$V1, na.rm = TRUE),
                     predict(mdl_lm, curr_test)
                     )

    curr_test <- data.frame( y = NA,
                             y_lag_1 = forecast[i,2],
                             y_lag_2 = curr_test[["y_lag_1"]],
                             # y_lag_3 = curr_test[["y_lag_2"]],
                             # y_lag_4 = curr_test[["y_lag_3"]],
                             # y_lag_5 = curr_test[["y_lag_4"]],
                             sin_10 = test$sin_10[i],
                             cos_10 = test$cos_10[i],
                             index = curr_test[["index"]] + 1
    )

    suppressWarnings(
      lines(c( rep(NA, length(train$y)), forecast[,1],col = "red"))
    )
    suppressWarnings(
      lines(c( rep(NA, length(train$y)),forecast[,2], col = "green"))
    )
    suppressWarnings(
      lines(c( rep(NA, length(train$y)),forecast[,3], col = "brown"))
    )
}

validation_result_rmsse <- c( rmsse = rmsse( test$y,
                                       forecast[,2],
                                       rmsse_y_scale ),
                        adj_rmsse = rmsse(test$y,
                                          forecast[,1],
                                          rmsse_y_scale),
                        rmsse_lm = rmsse( test$y,
                                          forecast[,3],
                                          rmsse_y_scale ),
                        naive_forecast_rmsse = rmsse( test$y,
                                                      mean(train$y),
                                                      rmsse_y_scale ),
                        average_tile_rmsse = rmsse( test$y,
                                                    tile_fcst,
                                                    rmsse_y_scale )
                        )
validation_result_rmse <- c( rmse = rmse( test$y,
                                       forecast[,2] ),
                        adj_rmse = rmse(test$y,
                                          forecast[,1]),
                        rmse_lm = rmse( test$y,
                                          forecast[,3] ),
                        naive_forecast_rmse = rmse( test$y,
                                                      mean(train$y) ),
                        average_tile_rmse = rmse( test$y,
                                                    tile_fcst )
)
tictoc::toc()


