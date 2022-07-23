remove(list=ls())
pkgload::load_all()

# supervised vs unsupervised performance benchmark

# tree_speed_benchmark <- bench::press(
#   nrow = c(100,200,250,500,750,1000,2000,5000),
#   max_depth = c(1:12),
#   noisevar = c( 1,5,10,20,25 )
#   # corrval = c(0,0.1,0.3,0.5,0.7,0.9),
#   {
#     dat <- caret::LPH07_2( nrow, noiseVars = noisevar )
#     bench::mark(
#       min_time = .05,
#       iterations = 50,
#       extra_tree = random_tree2(dat, max_depth = max_depth,
#                                 split_finder = supervised_find_rule),
#       totally_random = random_tree2( dat, max_depth = max_depth ),
#       check = FALSE
#     )
#   }
# )

rmse <- function( y, y_hat ) {
  sqrt(mean( (y - y_hat)^2, na.rm = TRUE ))
}


# performance comparison
sim_grid <- expand.grid( nrow = c(100,250,500,1000,5000),
                         max_depth = c(3,5,8,12),
                         corrvar = c(5,10,20),
                         noisevar = c( 5, 10, 20 ) )

parameter_sampler_mean <- function(X, row_id, target_col, ...) {
  mean(X[row_id, target_col], na.rm = TRUE)
}

predictor_identity_multi <- function(X, row_id, leaf_node, ...) {
  samples <- leaf_node$parameter_estimates
  list( predictions = samples)
}



res <- list()
for( sim in seq_len(nrow(sim_grid)) ) {

  temp <- replicate( n = 30, expr = {
    dat <- caret::LPH07_2( sim_grid[sim,]$nrow,
                           noiseVars = sim_grid[sim,]$noisevar,
                           corrVars = sim_grid[sim,]$corrvar )
    train <- rsample::initial_split(data = dat)
    test <- rsample::testing(train)
    train <- rsample::training(train)

    extra_tree <- random_tree2(train, max_depth = sim[sim,]$max_depth,
                               split_finder = supervised_find_rule,
                               nosplit_columns = "y",
                               parameter_sampler = parameter_sampler_mean,
                               target_col = "y")

    extra_preds <- unlist( predict(extra_tree, test, predict_fun = predictor_identity_multi)$predictions)

    rand_tree <- random_tree2(train, max_depth = sim[sim,]$max_depth,
                              nosplit_columns = "y",
                              parameter_sampler = parameter_sampler_mean,
                              target_col = "y")
    rand_preds <- unlist( predict(rand_tree, test, predict_fun = predictor_identity_multi)$predictions)

    lm_preds <- predict( lm(y~., train), test)

    data.frame( sim_grid[sim,],
                extra = rmse( test$y, extra_preds ),
                rand = rmse( test$y, rand_preds ),
                lm = rmse( test$y, lm_preds))
  }, simplify = FALSE)

  res[[sim]] <- do.call(rbind, temp)
}

res <- res %>%
  dplyr::bind_rows() %>%
  tidyr::pivot_longer(cols = c("lm","rand","extra"), names_to = "model")

res %>%
  dplyr::filter( nrow == 5000 ) %>%
  dplyr::select( -nrow ) %>%
  tidyr::unite(col = "group", sep = "/", -tidyselect::all_of(c("model","value"))) %>%
  ggplot2::ggplot(ggplot2::aes(x = value, color = model)) +
    ggplot2::geom_density() +
  ggplot2::facet_wrap(~group)

