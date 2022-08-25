# ad-hoc feature engineering

trigonometric_seasonal_dummy <- function( n, seas_length = c(12) ) {

  seas <- purrr::map( seas_length, function( season ) {

    sin_seas <- sin( 2*pi*seq_len(n)/season )
    cos_seas <- cos( 2*pi*seq_len(n)/season )

    X <- cbind( sin_seas, cos_seas )
    colnames(X) <- paste0( c("sin_", "cos_"), season)
    X
  })
  do.call(cbind, seas)
}

period <- function(x)
{
  n <- length(x)
  spec <- stats::spec.ar(c(x),plot=FALSE)
  if(max(spec$spec)>10) # Arbitrary threshold chosen by trial and error.
  {
    period <- round(1/spec$freq[which.max(spec$spec)])
    if(period==Inf) # Find next local maximum
    {
      j <- which(diff(spec$spec)>0)
      if(length(j)>0)
      {
        nextmax <- j[1] + which.max(spec$spec[j[1]:500])
        period <- round(1/spec$freq[nextmax])
      }
      else
        period <- 1
    }
  }
  else
    period <- 1
  return(period)
}

find_seasonalities <- function( y, max_iter = 5, aggregator = sum, upper_limit = 1500 ) {

  periods <- list()
  for( iter in seq_len(max_iter) ) {
    last_period <- tryCatch({
      period(y)
    }, error = function(e) 1 )

    if( last_period <= 1 || is.na(last_period) ){
      break;
    }
    periods[[iter]] <- last_period
    y <- stats::aggregate(
      stats::ts(y, freq = last_period), # where last_period is last infered periodicity
      nfrequency = 1, # nfrequency always set to 1
      FUN = aggregator # ie mean
    )
  }
  x <- cumprod( unlist(periods))
  x[ x < upper_limit ]
}

add_lags <- function( x, lags = 1:5 ) {
  sapply( lags, function(lag) {
    dplyr::lag(x,lag)
  })
}

add_time_var <- function( x, time_polynomial_order = 1 ) {
  base <- seq_len(length(x))
  sapply( seq_len(time_polynomial_order), function(power) {
    base^power
  })
}



