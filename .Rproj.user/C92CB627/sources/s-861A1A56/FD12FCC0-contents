


find_terminal_values <- function(tree, terminals = list(), ...) {

  if (is.character(tree$rule)) {
    return( cbind( id = tree$node_id, tree$terminal_rules ))
  }
  rbind( find_terminal_values(tree$left ),
         find_terminal_values( tree$right)
    )
}



decode_tree <- function(  ) {

}
