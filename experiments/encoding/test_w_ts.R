remove(list=ls())
pkgload::load_all()
library(magrittr)

# add_lags <- function( x, lags = 1:5 ) {
#   dplyr::bind_cols( x, purrr::map( lags, ~ dplyr::lag(x, n = .x)) )
# }


electricity <- tsibbledata::vic_elec %>%
  as.data.frame() %>%
  dplyr::select( -c("Time","Date")) %>%
  dplyr::mutate( Holiday = as.integer(Holiday)) %>%
  # input must not be a tibble currently
  as.data.frame()


tree <- random_tree(electricity, max_depth = 16)
encoded_tree <- encode( tree, electricity )
decoded_tree <- decode( tree, encoded_tree )

# # future::plan("multisession")
# # future::plan("sequential")

tictoc::tic("Time to fit forest:")
forest <- encoder_forest( electricity, n_tree = 100, max_depth = 8, subsample_size = 2000 )
tictoc::toc()

tictoc::tic("Time to encode:")
encoded_forest <- encode( forest, electricity )
tictoc::toc()

tictoc::tic("Time to decode:")
decoded_forest <- decode( forest, encoded_forest )
tictoc::toc()




