


dump_rule <- function(node, dir = "<=") {
  paste0("column:", node$rule$column, "|value", dir, node$rule$rule)
}

# repair_ruleset <- function(ruleset) {
#   rules <- list()
#   for( i in seq_len(length(ruleset)/2)) {
#     rules[[i]] <- ruleset[ (2*i-1):(2*i) ]
#   }
#   return(rules)
# }

find_rulesets <- function(tree, ruleset = list(), ...) {
  if (is.character(tree$rule)) {
    return(data.frame(
      ruleset = paste0(unlist(ruleset), collapse = "||"),
      terminal_íd = tree$node_id
    ))
  }
  rbind(
    find_rulesets(tree$left,
      ruleset = c(
        ruleset,
        list(dump_rule(tree,
          dir = "<="
        ))
      )
    ),
    find_rulesets(tree$right,
      ruleset = c(
        ruleset,
        list(dump_rule(tree,
          dir = ">"
        ))
      )
    )
  )
}

dump_range <- function(node) {
  data.frame(
    column = node$rule$column,
    range = node$rule$range
  )
}

resolve_ranges <- function(ranges) {
  do.call(rbind, ranges)
}

last_range <- function(range, k = 2) {
  len <- NROW(range)
  range[(len - k + 1):len, ]
}

repair_ranges <- function(ranges_df) {
  ranges_df %>%
    dplyr::group_by(id, column) %>%
    dplyr::group_split() %>%
    purrr::map(~ last_range(.x)) %>%
    dplyr::bind_rows()
}

find_ranges <- function(tree, ranges = list(), ...) {
  if (is.character(tree$rule)) {
    return(cbind(resolve_ranges(ranges), id = tree$node_id))
  }
  repair_ranges(
    rbind(
      find_ranges(tree$left,
        ranges = c(
          ranges,
          list(dump_range(tree))
        )
      ),
      find_ranges(tree$right,
        ranges = c(
          ranges,
          list(dump_range(tree))
        )
      )
    )
  )
}

runif_1 <- function( min, max ) {
  runif(1, min, max)
}


sample_from_range <- function( range, type = list(min, runif_1, max, mean), ... ) {
  # for nor only uniform ranges
  type[[1]]( min(range), max(range) )
}

draw_from_ranges <- function( ranges, id = ranges$id[1], ... ) {
  ranges %>%
    dplyr::filter( id == id ) %>%
    dplyr::group_by( column ) %>%
    dplyr::mutate( sample = sample_from_range(range, ...) ) %>%
    dplyr::ungroup() %>%
    dplyr::select( column, sample ) %>%
    dplyr::distinct() %>%
    dplyr::arrange(dplyr::desc(column)) %>%
    dplyr::select(sample) %>%
    c %>%
    as.matrix(nrow = 1) %>%
    data.frame()
}

generate <- function(tree, id = NULL, ...) {
  # get tree ranges
  ranges <- find_ranges(tree)
  # if id unsupplied then propose a random id
  if(is.null(id)) {
    id <- sample( unique(ranges$id), 1)
  }
  draw_from_ranges( ranges, id, ... )
}

reconstruct <- function(tree, encoded_values, ...) {

  # for a given tree find all ranges
  ranges <- find_ranges(tree)
  # if id unsupplied then propose a random id
  purrr::map( encoded_values, function(id){
    draw_from_ranges( ranges, id, ... )
  }) %>%
    dplyr::bind_rows()
}


