# isolation benchmark
remove(list=ls())
pkgload::load_all()
library(magrittr)

set.seed(1071)

anomaly_generator <- function( n = 5000, p = 10, anomalies = 100  ) {
  X <- matrix( rnorm( n*p, 0, 1), nrow = n, ncol = p)
  anom <- purrr::map( seq_len(p), ~ ceiling(runif(1)*anomalies))
  anom <- purrr::map( anom, ~ sample( seq_len(nrow(X)), .x) )

  X <- purrr::imap( anom, function(anomalies, index) {
    X[anomalies,index] <- rnorm(anomalies,
                                mean = rnorm(1, sd = 250),
                                sd = abs(rnorm(1, 0, sd = 250)))
    return(X[,index])
  })

  anomaly <- rep(0, n)
  anomaly[ Reduce(union, anom) ] <- 1

  return(setNames( data.frame(X, anomaly), c(seq_len(p), "anomaly")))
}

df <- anomaly_generator()

# fit <- random_tree( df, max_depth = 8 )

fit <- encoder_forest( df,
                       max_depth = 8,
                       n_tree = 200 )

scores <- isolation( fit, df  )

data.frame( anom = df$anomaly, score = scores ) %>%
  ggplot2::ggplot(ggplot2::aes(x = score, colour = as.factor(anom))) +
    ggplot2::geom_density()









