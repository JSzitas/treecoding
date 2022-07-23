
test_objects <- readRDS("testdata/decoding_data.rda")
forest <- test_objects$forest

test_that("subsetting a forest works", {
  expect_true(is_encoder_forest(forest))
  expect_true(is_encoder_forest(forest[1]))
  expect_true(is_encoder_forest(forest[1:5]))
  expect_true(is_encoder_forest(forest[]))
  expect_true(is_random_tree(forest[10, drop = TRUE]))
})
