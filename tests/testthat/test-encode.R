
test_objects <- readRDS( "testdata/decode_data.rda" )

test_that("encoding works for individual trees", {

  encoded <- encode(test_objects$tree, test_objects$df)

  expect_equal( length(encoded), 1000 )
  expect_equal( encoded[1], 6 )
  expect_equal( encoded[1000], 4 )
  expect_equal( sum(encoded), 5090)
})

test_that("encoding works for entire forests", {

  encoded <- encode(test_objects$forest, test_objects$df)

  expect_equal( length(encoded), 10 )
  expect_equal( class(encoded), "list" )
  expect_equal( names(encoded), as.character(1:10) )
  expect_equal( encoded[[1]][1], 3 )
  expect_equal( encoded[[10]][100], 2 )
  expect_equal( Reduce(sum, encoded), 32209)
})
