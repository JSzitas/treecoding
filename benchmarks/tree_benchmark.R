# mondrian vs non-mondrian speed benchmarks
pkgload::load_all()

tree_speed_benchmark <- bench::press(
  nrow = c(100,200,250,500,750,1000),
  max_depth = c(1:12),
  # corrval = c(0,0.1,0.3,0.5,0.7,0.9),
  {
    dat <- caret::LPH07_2( nrow )
    bench::mark(
      min_time = .05,
      iterations = 50,
      random_tree(dat, max_depth = max_depth),
      random_tree2( dat, max_depth = max_depth ),
      check = FALSE
    )
  }
)

# select
tree_bench_subset <- tree_speed_benchmark %>%
  tidyr::unnest( cols = c(time,gc) ) %>%
  dplyr::select( expression, nrow, max_depth, time  ) %>%
  tidyr::pivot_wider( names_from = "expression",
                      values_from = "time",
                      values_fn = list )

find_crit_nemenyi <- function( bench_result ) {
  res <- list()
  for( i in seq_len(nrow(bench_result)) ) {
    temp <- tree_bench_subset[i,] %>%
      tidyr::unnest( cols = c(`random_tree(dat, max_depth = max_depth)`,
                              `random_tree2(dat, max_depth = max_depth)`)
      ) %>%
      as.matrix %>%
      .[,c(3,4)] %>%
      tsutils::nemenyi() %>%
      .[["fpval"]]
    res[[i]] <- data.frame( tree_bench_subset[i,1:2], pval = temp )

  }
  do.call(rbind, res)
}









