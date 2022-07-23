
set.seed(1073)
ratio <- function(x) {
  ifelse( x[1] > x[2], x[2]/x[1], x[1]/x[2])
}


n_categ = 20
n_rare = 100
n_superrare = 1000

rare_strs <- Reduce( paste0, expand.grid(letters,letters, letters)) %>% unique
superrare_strs <- Reduce( paste0, expand.grid(letters,letters, letters, letters)) %>% unique


n_samples = 10000

res <- vector( "list", length = n_samples)

for( i in 1:n_samples ) {

  x <- sample(letters, n_categ)
  x <- c( x, sample(rare_strs, n_rare))
  x <- c( x, sample(superrare_strs, n_superrare) )
  x <- sample(x, size = 30000, replace = TRUE,
              prob = c(runif(n_categ), runif(n_rare, 0, 0.05),
                       runif(n_superrare, 0, 0.005)))

  row_id <- seq_len(length(x))

  # count aware categorical sampling
  unique_levels <- table(x)
  # randomly permute all levels
  permutation <- sample(unique_levels, length(unique_levels))
  # find 'position' of rule
  # sample a single level - ensure this is not the rightmost one
  position <- stats::runif(1, 0, 1)
  position <- cumsum(permutation/length(x)) < position

  left_rule <- names(which( position ))
  right_rule <- names(which( !position ))

  left <- row_id[x %in% left_rule]
  right <- row_id[x %in% right_rule]
  rule <- list(
    left = left_rule,
    right = right_rule
  )

  aware_classes_left = length(left_rule)/(n_categ + n_rare + n_superrare)

  # balance
  aware_ratio <- length(left)/length(x)#ratio(purrr::map_dbl( rule, ~sum(table(x)[.x])))

  # unaware
  unique_levels <- unique(x)
  # randomly permute all levels
  permutation <- sample(unique_levels, length(unique_levels))
  # find 'position' of rule
  # sample a single level - ensure this is not the rightmost one
  position <- ceiling(stats::runif(1, 0, length(permutation)))
  left_rule <- permutation[seq_len(length(permutation)) <= position]
  right_rule <- permutation[seq_len(length(permutation)) > position]

  left <- row_id[x %in% left_rule]
  right <- row_id[x %in% right_rule]
  rule <- list(
    left = left_rule,
    right = right_rule
  )
  # balance
  unaware_ratio <- length(left)/length(x)# ratio(purrr::map_dbl( rule, ~sum(table(x)[.x])))

  unaware_classes_left = length(left_rule)/(n_categ + n_rare + n_superrare)

  res[[i]] <- c( aware = unname(aware_ratio),
                 unaware = unname(unaware_ratio),
                 aware_left = aware_classes_left,
                 unaware_left = unaware_classes_left)
}

