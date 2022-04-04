test_that("utils works", {

  set.seed(1071)

  expect_true( is_numeric( c("1","2","3",NA) ))
  expect_true( !is.numeric( c("1","2","b",NA)))

  expect_equal( length(resample_folds(10, 5)), 2 )
  expect_equal( Reduce( sum, resample_folds(100, 20) ), 4949)

  expect_equal( mean_sample( logical(0) ), NA )
  expect_equal( mean_sample( 1:10 ), 5.5 )
  expect_equal( mean_sample(c("a","a")), "a" )

  expect_equal( median_sample( logical(0) ), NA )
  expect_equal( median_sample( 1:10 ), 5.5 )
  expect_equal( median_sample(c("a","a")), "a" )
})
