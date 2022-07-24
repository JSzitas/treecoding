test_that("utils works", {
  set.seed(1071)

  expect_equal(mean_sample(logical(0)), NA)
  expect_equal(mean_sample(1:10), 5.5)
  expect_equal(mean_sample(c("a", "a")), "a")

  expect_equal(median_sample(logical(0)), NA)
  expect_equal(median_sample(1:10), 5.5)
  expect_equal(median_sample(c("a", "a")), "a")
})
