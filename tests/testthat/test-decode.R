
test_objects <- readRDS("testdata/decoding_data.rda")

test_that("decoding a tree works", {
  testthat::skip("Broken for now, revisit when fixing decoding")
  set.seed(1071)
  decoded_tree <- decode(test_objects$tree, test_objects$encoded_tree)

  expected_result <- structure(list(`1` = c("b", "b", NA, "b", NA, NA), `2` = c(
    "1.8706630098618",
    "1.8706630098618", "-1.39795502396663", "1.8706630098618", "-1.39795502396663",
    "-1.39795502396663"
  ), `4` = c(
    NA, NA, "-2.07220051281721", NA,
    "1.20458724034056", "1.20458724034056"
  )), row.names = c(NA, -6L), class = c("data.table", "data.frame"))

  expect_equal(head(decoded_tree), expected_result)
  expect_equal(dim(decoded_tree), c(1000, 3))
})

test_that("decoding a forest works", {
  testthat::skip("Broken for now, revisit when fixing decoding")

  decoded_forest <- decode(test_objects$forest, test_objects$encoded_forest)

  expected_result <- structure(list(`1` = c("c", "c", "a", "c", "b", "a"), `2` = c(
    0.626031547179863,
    0.626031547179863, -0.366569733456803, 0.626031547179863, -0.366569733456803,
    -0.366569733456803
  ), `3` = c(
    0.776572376804498, 0.2551885635062,
    0.2551885635062, 0.776572376804498, 0.776572376804498, 0.470562187173014
  ), `4` = c(
    -0.271323354920122, -0.271323354920122, -0.271323354920122,
    -0.271323354920122, -0.271323354920122, 1.02823162600197
  )), row.names = c(
    NA,
    -6L
  ), class = c("data.table", "data.frame"))

  expect_equal(head(decoded_forest), expected_result)
  expect_equal(dim(decoded_forest), c(1000, 4))
})
