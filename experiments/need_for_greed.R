# sampling tree time varying AR process

remove(list=ls())
pkgload::load_all()
library(magrittr)
tictoc::tic(msg = paste0( "Fit done at:", Sys.time() ))

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
seas_vec <- find_seasonalities(train$Demand)
seas_xreg <- trigonometric_seasonal_dummy( max(test_ids), seas_vec )
test <- electricity[test_ids,]
train <- cbind( train, seas_xreg[train_ids,])
test <- cbind( test, seas_xreg[test_ids,])


parameter_sampler_ar <- function( X, row_id, ... ) {

  safe_lm <- purrr::safely( lm, otherwise = NULL )
  model <- safe_lm( Demand ~ ., data = X[row_id,])

  if(!is.null(model[["result"]])) {
    return(
      coef(model[["result"]], complete = TRUE)
    )
  }
  rep(NA, ncol(X)+1)
}

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











