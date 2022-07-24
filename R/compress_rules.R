
compress_rule <- function(rule, col_names) {
  c(
    which(col_names == rule$column),
    unname(unlist(rule$ranges))
  )
}

compress_tree <- function(tree, col_names) {
  tree$rule

  # compress_tree( tree$left ),
  # compress_tree(tree$right)
}

compress_rules <- function(tree, df) {
  col_names <- colnames(df)

  compress_tree(tree$tree, col_names)
}
