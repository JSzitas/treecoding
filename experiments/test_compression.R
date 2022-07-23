# mondrian vs non-mondrian speed benchmarks
pkgload::load_all()

set.seed(1071)

df <- caret::LPH07_2( 1000 )
tree <- random_tree2( df, max_depth = 12 )

tree[["tree"]][["rule"]] %>%
  object.size %>%
  print()
