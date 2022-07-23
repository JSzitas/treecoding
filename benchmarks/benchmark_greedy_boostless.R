pkgload::load_all()

rmse <- function( y, y_hat ) {
  sqrt(mean( (y - y_hat)^2, na.rm = TRUE ))
}

res <- greedy_boostless_machine(train, target = "Demand", subsample_size = 2000,
                                held_out = 0.2,
                                stacking_iter = 75, n_tree = 100, resample = TRUE)

stacked_preds <- stacked_prediction( forest = res$forest, test, res$weights ) %>%
  dplyr::rename( "stacked_pred" = "pred" )
unstacked_preds <- unstacked_prediction( forest = res$forest, test ) %>%
  dplyr::rename( "unstacked_pred" = "pred" )

preds <- dplyr::left_join(stacked_preds, unstacked_preds, by = c("id"))

print(
  data.frame( unstacked_rmse = rmse( test$Demand, preds$unstacked_pred ),
            stacked_rmse = rmse( test$Demand, preds$stacked_pred ) )
)
