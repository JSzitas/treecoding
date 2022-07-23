# if file does not exist, download it first
remove(list = ls())
pkgload::load_all()


file_path <- "./data/energy.xlsx"
if (!file.exists(file_path)) {
  dir.create("./data")
  url <- "http://archive.ics.uci.edu/ml/machine-learning-databases/00242/ENB2012_data.xlsx"
  download.file(
    url = url,
    destfile = file_path,
    mode = "wb"
  )
}
df <- readxl::read_xlsx(path = file_path) %>%
  as.data.frame()

train <- sample.int(nrow(df), 0.7 * nrow(df))
test <- seq_len(nrow(df))[-train]

train <- df[train, ]
test <- df[test, ]

parameter_sampler_mean <- function(X, row_id, ...) {
  colMeans(X[row_id, c("Y1", "Y2")], na.rm = TRUE)
}

subgroup_variance <- function(x, split) {
  lower <- x[x <= split]
  upper <- x[x > split]
  sum(var(lower), var(upper), na.rm = TRUE)
}
subsample_level_proposals <- function(x, k = 5) {
  unique_levels <- unique(x)
  replicate(
    n = k,
    {
      # randomly permute all levels
      permutation <- sample(unique_levels, length(unique_levels))
      # find 'position' of rule
      # sample a single level - ensure this is not the rightmost one
      position <- ceiling(stats::runif(1, 0, length(permutation)))
      left_group <- permutation[seq_len(length(permutation)) <= position]
      right_group <- permutation[seq_len(length(permutation)) > position]
      list(left_group, right_group)
    },
    simplify = FALSE
  )
}

subgroup_level_distribution <- function(x, subgroup_left, subgroup_right) {
  abs(sum(x %in% subgroup_left) - sum(x %in% subgroup_right))
}

find_rule_selfsup <- function(X, row_id, column, k = 5) {
  mia <- is.na(X[row_id, column])
  row_id <- row_id[!mia]
  if (length(row_id) == 0) {
    return(
      list(
        column = column,
        rule = NULL,
        left = NULL,
        right = NULL,
        mia_dir = NULL
      )
    )
  }

  # numeric rules
  if (is.numeric(X[, column])) {
    left_rule <- min(X[row_id, column])
    right_rule <- max(X[row_id, column])

    rule <- stats::runif(k, left_rule, right_rule)
    rule_selected <- which.min(sapply(
      rule,
      function(split) {
        subgroup_variance(X[row_id, column], split)
      }
    ))
    rule <- rule[rule_selected]

    left <- row_id[X[row_id, column] <= rule]
    right <- row_id[X[row_id, column] > rule]

    rule <- list(
      left = c(left_rule, rule),
      right = c(rule, right_rule)
    )
  } else {
    sampled_level_groups <- subsample_level_proposals(X[row_id, column], k)
    least_disparate_dist <- which.min(sapply(
      sampled_level_groups,
      function(i) {
        subgroup_level_distribution(X[row_id, column], i[[1]], i[[2]])
      }
    ))
    left_rule <- sampled_level_groups[[least_disparate_dist]][[1]]
    right_rule <- sampled_level_groups[[least_disparate_dist]][[2]]
    left <- row_id[X[row_id, column] %in% left_rule]
    right <- row_id[X[row_id, column] %in% right_rule]
    rule <- list(
      left = left_rule,
      right = right_rule
    )
  }
  mia_dir <- rbinom(1, 1, 0.5)
  # either send mia observations left or right, with equal probability
  if (mia_dir == TRUE) {
    # 1 is for right
    right <- c(right, which(mia))
  } else {
    # 0 is for left
    left <- c(left, which(mia))
  }

  list(
    column = column,
    rule = rule,
    left = left,
    right = right,
    mia_dir = mia_dir
  )
}




rmse <- function(x, y) {
  sqrt(mean((x - y)^2, na.rm = TRUE))
}
random_benchmark <- function(train_y, size = 10) {
  rnorm(size, mean(train_y, na.rm = TRUE), sd(train_y, na.rm = TRUE))
}

predictor_identity_multi <- function(X, row_id, samples, ...) {
  matrix(c(samples), nrow = length(row_id), ncol = length(samples), byrow = TRUE)
}

tictoc::tic()
forest <- encoder_forest(train, 8,
  n_tree = 250, resample = TRUE,
  nosplit_columns = c("Y1", "Y2"), parameter_sampler = parameter_sampler_mean, split_finder = find_rule
)
tictoc::toc()
tictoc::tic()
predictions_on_test_forest <- predict(forest, test, predict_fun = predictor_identity_multi)
#
merge_predictions <- function( pred ) {
  res <- pred[, lapply(.SD, mean, na.rm = TRUE), by = id, .SDcols = c("X1", "X2")]
  unlist(res[order(id), ]$X1)
}

