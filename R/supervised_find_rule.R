
rss <- function(X) {
  sum((colMeans(as.matrix(X)) - as.matrix(X))^2)
}

supervised_find_rule <- function(X, row_id, available_columns, ranges, col_try = 5, k = 5, ...) {

  # set the ranger mtry to 5
  try_columns <- sample(available_columns, col_try)
  targets <- X[, setdiff(colnames(X), available_columns), drop = FALSE]

  res <- list()
  iter <- 1
  for (col in try_columns) {
    for (split_k in seq_len(k)) {
      # find missing values in column
      missing_rows <- is.na(X[row_id, col])
      row_id <- row_id[!missing_rows]
      missing_rows <- which(missing_rows)
      if (length(row_id) == 0) {
        next()
      }
      range <- ranges[col]

      if (is.numeric(unlist(range))) {
        range <- unlist(range)
        position <- runif(1, range[1], range[2])

        left <- row_id[X[row_id, col] <= position]
        right <- row_id[X[row_id, col] > position]

        left_range <- unname(c(range[1], position))
        right_range <- unname(c(position, range[2]))
      } else {
        # randomly permute all levels
        permutation <- sample(unlist(range), length(range))
        # find 'position' of rule
        # sample a single level
        position <- ceiling(runif(1, min = 0, max = length(range)))
        left_range <- permutation[seq_len(length(permutation)) <= position]
        right_range <- permutation[seq_len(length(permutation)) > position]

        left <- row_id[X[row_id, col] %in% left_range]
        right <- row_id[X[row_id, col] %in% right_range]
      }
      if (length(left) == 0 ||
        length(right) == 0) {
        next()
      }

      left_rss <- rss(targets[left, ])
      right_rss <- rss(targets[right, ])
      mia_rss_left <- rss(targets[c(left, missing_rows), ])
      mia_rss_right <- rss(targets[c(right, missing_rows), ])
      # determine mia direction by looking at RSS with missing going right
      rss_res <- c( # versus missing going left
        mia_rss_left + right_rss,
        left_rss + mia_rss_right
      )
      mia_dir <- which.min(rss_res) - 1
      if (mia_dir == 0) {
        left <- c(left, missing_rows)
      } else {
        right <- c(right, missing_rows)
      }

      res[[iter]] <- list(
        column = col,
        rss = rss_res[which.min(rss_res)],
        left_range = left_range,
        right_range = right_range,
        left_id = left,
        right_id = right,
        mia = mia_dir
      )
      iter <- iter + 1
    }
  }
  if (length(res) == 0) {
    return(
      list(
        column = col,
        rule = NULL,
        left = NULL,
        right = NULL,
        mia_dir = NULL
      )
    )
  }
  # return(res)

  best <- which.min(sapply(res, function(x) x[["rss"]]))
  rule <- res[[best]]

  # update ranges and return
  left_ranges <- ranges
  left_ranges[[rule$column]] <- rule$left_range # c( ranges[[rule$column]][1], rule$rule)
  right_ranges <- ranges
  right_ranges[[rule$column]] <- rule$right_range # c( rule$rule, ranges[[rule$column]][2] )

  return(
    list(
      column = rule$column,
      left_ranges = left_ranges,
      right_ranges = right_ranges,
      left_id = rule$left_id,
      right_id = rule$right_id,
      mia_dir = rule$mia
    )
  )
}
