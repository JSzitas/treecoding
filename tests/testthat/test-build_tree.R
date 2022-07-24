test_that("Fitting a random tree works", {
  set.seed(1071)

  n <- 100000
  p <- 6

  df <- data.frame(
    fact = sample(letters[1:6], n, replace = TRUE),
    matrix(rnorm(n * (p - 1)), ncol = p - 1)
  )
  tree <- random_tree(df, max_depth = 8)

  expect_s3_class(tree, "random_tree")
  expect_equal(tree[["tree"]][["rule"]][["column"]], "X3")
  expect_equal(tree[["tree"]][["rule"]][["rule"]], 0.6484567, tolerance = 0.05)
})
