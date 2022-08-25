remove(list=ls())
pkgload::load_all(recompile = FALSE)

set.seed(1071)
# estimating parameters of a 2 regime AR model - simple case where switching is
# dependent on a single time point

# generate stationary AR process
ar_1 <- 0.7
# a bit of mean reversion
ar_2 <- -0.3
# first 200 values of this process are one regime, the rest are the other regime
y <- c( rnorm(1), rep(NA, 399))
y[2] <- y[1] * 0.7
for( i in 3:200 ) {
  y[i]  <- ar_1 * y[i-1] + ar_2 * y[i-2] + rnorm(1)
}

plot(y, type = "l")

print(summary(lm(y ~ dplyr::lag(y) + dplyr::lag(y,2)-1 )))
# generate stationary AR process
ar_1 <- 0.2
# a bit of more mean reversion
ar_2 <- - 0.5
for( i in 201:400 ) {
  y[i] <- ar_1 * y[i-1] + ar_2 * y[i-2] + rnorm(1)
}

plot(y, type = "l")

print(summary(lm(y ~ dplyr::lag(y) + dplyr::lag(y,2)-1 )))
# the estimates look a bit like averages over the two regimes - not very happy

# note that adding a 'correct' interaction variable we can surely estimate correctly
index <- c(rep(0,200), rep(1, 200))

print(summary(lm(y ~ .-1, data.frame( lag_1 = dplyr::lag(y),
                                     lag_2 = dplyr::lag(y, 2),
                                     lag_1_after_index = dplyr::lag(y,1) * index,
                                     lag_2_after_index = dplyr::lag(y,2) * index)
                 )
              )
)
# caveat - the coefficients after index are 'differences' from baseline -
# so to get the real AR1 and AR2 coefficients, you need to take
# coef(lag_1) + coef(lag_1_after_index) >> which yields ~~ 0.21
# and ~~ -0.66 - which is not perfect, but is within the confidence intervals
# this is if you perfectly know the breakpoint

# note that in these simple cases we can identify this with breakpoints, more
# or less:
print(strucchange::breakpoints(y~dplyr::lag(y) + dplyr::lag(y,2)-1))
# even if we misspecify the constant
print(strucchange::breakpoints(y~dplyr::lag(y) + dplyr::lag(y,2)))
# not if we use just one lag
# print(strucchange::breakpoints(y~dplyr::lag(y)))
# but yes if we use more than necessary (probably :))
# print(strucchange::breakpoints(y~dplyr::lag(y) + dplyr::lag(y,2) + dplyr::lag(y,3) ))
# but this is still a bit tacky, and this is already a very simple example.
# can we do better? well, we can try

# create a design matrix
X <- data.frame( y = y,
                 y_lag_1 = dplyr::lag(y,1),
                 y_lag_2 = dplyr::lag(y,2),
                 index = seq_len(length(y)))

parameter_sampler_ar <- function( X, row_id, ... ) {

  safe_lm <- purrr::safely( lm, otherwise = NULL )
  model <- safe_lm( y ~ y_lag_1 + y_lag_2-1, data = X[row_id,])

  if(!is.null(model[["result"]])) {
    return(
      coef(model[["result"]], complete = TRUE)
    )
  }
  rep(NA, ncol(X)+1)
}
tictoc::tic("Estimation:")
forest <- encoder_forest(X,
                         max_depth = 3,
                         # generally more trees is always better >> unless intolerably slow,
                         # use more
                         n_tree = 500,
                         subsample_size = 50,
                         # we insist that splits should not be done on these variables
                         nosplit_columns = c( "y", "y_lag_1", "y_lag_2"),
                         resample = TRUE,
                         min_nodesize = 10,
                         parameter_sampler = parameter_sampler_ar)
predictor_ar_par <- function(X, row_id, leaf_node, ...) {
  samples <- leaf_node$parameter_estimates
  # by convention I wrap results in a named list
  list(parameters = samples)
}
# predict coefficients in sample
pred <- predict( forest, X, predict_fun = predictor_ar_par)
tictoc::toc()
tictoc::tic("Pivot:")
# predicted coefficients are for all trees - we need to summarize them
est_coefs <- pred %>%
  tidyr::unnest_wider(col = parameters) %>%
  dplyr::group_by(id) %>%
  # generally median estimates tend to be more stable than mean ones for
  # this type of tree
  dplyr::summarise( est_lag_1 = median(y_lag_1, na.rm = TRUE),
                    est_lag_2 = median(y_lag_2, na.rm = TRUE)) %>%
  dplyr::ungroup()

plot.ts(est_coefs[,2], ylim = c( min(unlist(est_coefs[,2:3]))-0.1,
                                 max(unlist(est_coefs[,2:3]))+0.1 )
        )
lines(est_coefs[,3], col = "red")
tictoc::toc()
# the result is sort of 'smoothed' and 'time-varying' yet for the purpose of
# prediction this is (nearly) perfect, and in cases with smooth regime switches,
# this also seems better.

# note that I did not need to **know** when this change occurs, nor did
# I need to rely on a **test** and I still got reasonable estimates of
# a time varying coefficient
# but what if I am stupid with this?

tictoc::tic("Estimation:")
forest <- encoder_forest(X,
                         max_depth = 3,
                         # generally more trees is always better >> unless intolerably slow,
                         # use more
                         n_tree = 500,
                         subsample_size = 50,
                         # here comes my stupidity - who says there has to be 1
                         # regime switching variable? :)
                         nosplit_columns = "y",
                         resample = TRUE,
                         min_nodesize = 10,
                         parameter_sampler = parameter_sampler_ar)
predictor_ar_par <- function(X, row_id, leaf_node, ...) {
  samples <- leaf_node$parameter_estimates
  # by convention I wrap results in a named list
  list(parameters = samples)
}
# predict coefficients in sample
pred <- predict( forest, X, predict_fun = predictor_ar_par)
tictoc::toc()
tictoc::tic("Pivot:")
# predicted coefficients are for all trees - we need to summarize them
est_coefs <- pred %>%
  tidyr::unnest_wider(col = parameters) %>%
  dplyr::group_by(id) %>%
  # generally median estimates tend to be more stable than mean ones for
  # this type of tree
  dplyr::summarise( est_lag_1 = median(y_lag_1, na.rm = TRUE),
                    est_lag_2 = median(y_lag_2, na.rm = TRUE)) %>%
  dplyr::ungroup()

plot.ts(est_coefs[,2], ylim = c( min(unlist(est_coefs[,2:3]))-0.1,
                                 max(unlist(est_coefs[,2:3]))+0.1 )
)
lines(est_coefs[,3], col = "red")
tictoc::toc()

# that looks messy - but it is broadly not wrong, just not as nice and smooth
# as the other one



