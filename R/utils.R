resample_folds <- function( ids = seq_len(10), size = 2 ) {

  # full_range <- seq_len(range)
  folds <- list()
  for( fold in seq_len(ceiling(length(ids)/size)) ) {
    current_sample <- sample( ids, size = size )
    ids <- ids[ -c(current_sample) ]
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


get_max_depth <- function( tree, depth = 0 ) {
  if( depth == 0 ) {
    tree <- tree$tree
  }
  if( is.character(tree$rule) ) {
    return(depth)
  }
  max( get_max_depth(tree$left, depth = depth + 1),
       get_max_depth(tree$right, depth = depth + 1)
       )
}
