# variable importances

# blackbox
blackbox_varimp <- function(model, data, test_cols = "test", score_fun = rmse,
                            predictor = purrr::partial(predict, predict_fun = predictor_identity_multi),
                            predict_adapter = function(x) {
                              x$prediction
                            }) {
  test_col_results <- list()
  for (test_col in test_cols) {
    # return(predict_adapter(predictor(model, data), test_col))
    baseline <- score_fun(data[[test_col]], predict_adapter(predictor(model, data), test_col))
    # return(baseline)
    score_cols <- setdiff(colnames(data), test_col)
    varimps <- c()
    for (col in score_cols) {
      new_data <- data
      # permute
      new_data[[col]] <- sample(data[[col]], nrow(data))
      permute_score <- score_fun(data[[test_col]], predict_adapter(predictor(model, new_data), test_col))
      varimps[[col]] <- baseline - permute_score
    }
    test_col_results[[test_col]] <- unlist(varimps)
  }
  return(test_col_results)
}

merge_predictions <- function(pred, test_col = test_col) {
  pred %>%
    as.data.frame() %>%
    dplyr::group_by(id) %>%
    dplyr::transmute(dplyr::across(test_col, .fns = mean)) %>%
    dplyr::arrange(id) %>%
    dplyr::distinct() %>%
    dplyr::ungroup() %>%
    dplyr::select(test_col) %>%
    unlist()
}

blackbox_varimp(forest, test, target_cols, predict_adapter = merge_predictions) -> varimps
varimps %>%
  purrr::map(~ .x %>%
    sort(decreasing = TRUE) %>%
    .[1:10] %>%
    names()) %>%
  dplyr::bind_rows() %>%
  View()
