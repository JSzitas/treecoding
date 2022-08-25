# sampling tree time varying AR process
remove(list=ls())
pkgload::load_all()
library(magrittr)

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
test <- electricity[test_ids,]

rmse <- function(x, y) {
  sqrt(mean((x - y)^2, na.rm = TRUE))
}

tictoc::tic(msg = paste0( "Fit done at:", Sys.time() ))
greed <- greedy_boostless_machine( train,
                          target = "Demand",
                          held_out = 0.2,
                          stacking_iter = 100,
                          max_depth = 12,
                          n_tree = 100,
                          subsample_size = 2000,
                          resample = TRUE,
                          row_id = 1:34890)

tictoc::toc()

