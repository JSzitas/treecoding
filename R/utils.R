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
is_numeric <- function( x ) {
  suppressWarnings(
    !all( is.na( as.numeric( x ) ))
  )
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
median_sample <- function(x) {
  if( length(x) == 0) {
    return(NA)
  }
  if( is.character(x) ) {
    return( sample( x, 1 ))
  }
  return( median(x) )
}
