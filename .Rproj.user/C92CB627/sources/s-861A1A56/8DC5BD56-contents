

traversal_id <- function( tree, row ) {
  rule <- tree$rule
  if( is.character(rule) ) {
    return(tree$node_id)
  }
  if( row[rule$column] <= rule$rule ) {
    traversal_id( tree$left, row )
  }
  else{
    traversal_id( tree$right, row )
  }
}

encode_tree <- function( tree, X ) {
  apply( X, 1, function(i) {
    traversal_id(tree, i)
  })
}
