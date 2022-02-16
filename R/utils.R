resample_folds <- function( range = 10, size = 2 ) {

  full_range <- seq_len(range)
  folds <- list()
  for( fold in seq_len(ceiling(range/size)) ) {
    current_sample <- sample( full_range, size = size )
    full_range <- full_range[ -c(current_sample) ]
    folds[[fold]] <- current_sample
  }
  return(folds)
}

first <- function(x) {
  if( length(x) == 0) {
    return(NA)
  }
  x[[1]]
}

last <- function(x) {
  if( length(x) == 0) {
    return(NA)
  }
  x[[length(x)]]
}

random_sample <- function(x) {
  if( length(x) == 0) {
    return(NA)
  }
  if( is.character(x) ) {
    return( sample( x, 1 ))
  }
  return( stats::runif(1, min(x), max(x)))
}

mean_sample <- function(x) {
  if( length(x) == 0) {
    return(NA)
  }
  if( is.character(x) ) {
      return( sample( x, 1 ))
  }
  return( mean(x) )
}

is_encoder_forest <- function( x ) {
  class(x) == "encoder_forest"
}

#' @export
`[.encoder_forest` <- function( x, j, drop = FALSE) {
  if( !is_encoder_forest(x) ) {
    return(NextMethod())
  }
  j_arg <- substitute(j)
  if (missing(j)) {
    j <- NULL
    j_arg <- NULL
  }
  else if (is.null(j)) {
    j <- integer()
  }
  if( drop ) {
    return(unclass(x)[j])
  }
  structure( unclass(x)[j], class = "encoder_forest" )
}
#' @export
`[[.encoder_forest` <- function (x, i, j, ..., exact = TRUE)
{
  NextMethod()
}
#' @export
`$.encoder_forest` <- function(x, i) {
  NextMethod()
}


