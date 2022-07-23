# mondrian speed benchmarks (by lambda)
pkgload::load_all()

mondrian_tree_benchmark <- bench::press(
  lambda = c(1e2, 1e3, 1e6, 1e9, 1e12),
  nrow = c(500,5000),
  {
    dat <- caret::LPH07_2( nrow)
    bench::mark(
      min_time = .05,
      iterations = 50,
      mondrian_tree(dat, lambda = lambda),
      check = FALSE
    )
  }
)
