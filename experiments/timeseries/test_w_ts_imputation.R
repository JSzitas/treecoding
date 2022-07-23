remove(list=ls())
pkgload::load_all()
library(magrittr)

add_lags <- function( x, lags = 1:5 ) {
  dplyr::bind_cols( x, purrr::map( lags, ~ dplyr::lag(x, n = .x)) )
}

replace_frac_w_na <- function(x, na_frac = 0.15) {
  x[ sample.int(length(x), size = length(x)*0.15) ] <- NA
  return(x)
}

miss_me <- function( df, missingness = 0.15 ) {
  purrr::map_dfc( df, ~ replace_frac_w_na(.x, missingness) )
}


electricity <- tsibbledata::vic_elec %>%
  as.data.frame() %>%
  miss_me() %>%
  dplyr::select( -c("Time","Date")) %>%
  dplyr::mutate( Holiday = as.integer(Holiday)) %>%
  purrr::map( add_lags ) %>%
  # dplyr::bind_cols() %>%
  # na.omit() %>%
  # input must not be a tibble currently
  as.data.frame()


tree <- random_tree(electricity, max_depth = 10)
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
