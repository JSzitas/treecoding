
find_rule <- function( X, column ) {
  # sample a random number - consider all rules to be smaller equal for now
  rule <- runif( 1, min(X[,column]), max(X[,column]))

  left <- X[ X[,column] <= rule,,drop = FALSE]
  right <- X[ X[,column] > rule,,drop = FALSE]

  list( column = column,
        rule =  list( left = min(X[,column]),
                      mid = rule,
                      right = max(X[,column])),
        left = left,
        right = right)
}

resolve_terminal_rules <- function( rule_df ) {
  rule_df %>%
    dplyr::group_by(column) %>%
    dplyr::slice_tail() %>%
    dplyr::ungroup()
}


split <- function( X, max_length = 5, current_depth = 1, node_id = 1, terminal_rules = NULL, ... ) {

  if( current_depth == max_length || nrow(X) < 2 ) {
    return(list( rule = "terminal_node",
                 terminal_rules = resolve_terminal_rules(terminal_rules),
                 node_id = node_id) )
  }

  col <- sample( seq_len(ncol(X)),1 )
  rule <- find_rule( X, col )
  list( rule = list( column = rule$column, rule = rule$rule$mid ),
        left = split( rule$left, max_length, current_depth+1, 2*node_id,
                      terminal_rules = rbind( terminal_rules,
                                              data.frame( column = rule$column,
                                                          left = rule$rule$left,
                                                          right = rule$rule$mid) )
                      ),
        right = split( rule$right, max_length, current_depth+1, 2*node_id + 1,
                       terminal_rules = rbind( terminal_rules,
                                               data.frame( column = rule$column,
                                                           left = rule$rule$mid,
                                                           right = rule$rule$right) )),
        node_id = node_id)
}

build_random_tree <- function( X, max_length = 10 ) {
  structure(
      split( X, max_length = max_length),
    class = "random_tree"
  )
}
