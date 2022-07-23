

var <- function( y, max_lag = 5, const = TRUE ) {

  X <- dplyr::bind_cols(
    purrr::map( as.list(y), function(col) {
      dplyr::bind_cols(
        purrr::map( seq_len(max_lag),
                    ~ dplyr::lag(col, .x)
                    )
        )
  }))
  X <- as.matrix(na.omit(X))
  Y <- as.matrix(y)[(max_lag+1):NROW(y),]
  return( list(X, Y, t(X) %*% solve(X %*% t(X))) )
  # betas <- Y %*% t(X) %*% solve(X %*% t(X))
  return(betas)



  # for( i in seq_len(ncol(y)) ) {
  #   lm( y[,i] ~
  # }

  # purrr::map_dfc(y, function(col) {
  #
  # })
}
