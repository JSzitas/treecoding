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
  # purrr::map( add_lags ) %>%
  # dplyr::bind_cols() %>%
  na.omit() %>%
  # input must not be a tibble currently
  as.data.frame()


# tree <- random_tree(electricity)
# encoded_tree <- encode( tree, electricity )
# decoded_tree <- decode( tree, encoded_tree )

# # future::plan("multisession")
# # future::plan("sequential")

forest <- encoder_forest( electricity, n_tree = 10, max_depth = 8, subsample_size = 2000 )
encoded_forest <- encode( forest, electricity )
decoded_forest <- decode( forest, encoded_forest )


# larger
# forest2 <- encoder_forest( electricity, n_tree = 50, max_depth = 8, subsample_size = 2000 )
# encoded_forest2 <- encode( forest2, electricity )
# decoded_forest2 <- decode( forest2, encoded_forest2 )



