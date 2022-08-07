# sampling tree time varying AR process

remove(list=ls())
pkgload::load_all(recompile = FALSE)
library(magrittr)
tictoc::tic(msg = paste0( "Fit done at:", Sys.time() ))


electricity <- tsibbledata::vic_elec %>%
  as.data.frame() %>%
  dplyr::select( -c("Time","Date", "Temperature")) %>%
  dplyr::mutate( Holiday = as.integer(Holiday)) %>%
  na.omit() %>%
  # input must not be a tibble currently
  as.data.frame() %>%
  dplyr::mutate( demand_lag_1 = dplyr::lag(Demand, 1),
                 demand_lag_2 = dplyr::lag(Demand, 2),
                 demand_lag_3 = dplyr::lag(Demand, 3),
                 demand_lag_4 = dplyr::lag(Demand, 4),
                 demand_lag_5 = dplyr::lag(Demand, 5),
                 time_index = seq_len(length(Demand)))

train_ids <- 1:35000
test_ids <- 35001:37000

train <- electricity[train_ids,]
seas_vec <- find_seasonalities(train$Demand)
seas_xreg <- trigonometric_seasonal_dummy( max(test_ids), seas_vec )
test <- electricity[test_ids,]
train <- cbind( train, seas_xreg[train_ids,])
test <- cbind( test, seas_xreg[test_ids,])


parameter_sampler_ar <- function( X, row_id, ... ) {

  safe_lm <- purrr::safely( lm, otherwise = NULL )
  model <- safe_lm( Demand ~ Holiday + demand_lag_1 + demand_lag_2 + demand_lag_3 +
                    + demand_lag_4 + demand_lag_5 + time_index, data = X[row_id,])

  if(!is.null(model[["result"]])) {
    return(
    coef(model[["result"]], complete = TRUE)
    )
  }
  rep(NA, ncol(X)+1)
}

sampler_forest <- encoder_forest( train,
                                  max_depth = 4,
                                  n_tree = 1000,
                                  subsample_size = 3000,
                                  nosplit_columns = c("Demand","demand_lag_1", "demand_lag_2", "demand_lag_3",
                                  "demand_lag_4", "demand_lag_5"),
                                  resample = TRUE,
                                  row_id = 1:34890,#44890,
                                  parameter_sampler = parameter_sampler_ar )

tictoc::toc()

