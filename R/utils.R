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
