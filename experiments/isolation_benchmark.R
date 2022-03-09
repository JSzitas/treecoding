# isolation benchmark
remove(list=ls())
pkgload::load_all()

set.seed(1071)


anomaly_generator <- function( n= 5000, p = 10, anomalies = 100  ) {
  X <- matrix( rnorm( n*p, 0, 1), nrow = n, ncol = p)
  anom <- purrr::map( seq_len(p), ~ ceiling(runif(1)*anomalies))
  anom <- purrr::map( anom, ~ sample( seq_len(nrow(X)), .x) )

  X <- purrr::imap( anom, function(anomalies, index) {
    X[anomalies,index] <- rnorm(anomalies,
                                mean = rnorm(1, sd = 100),
                                sd = abs(rnorm(1, 0, sd = 100)))
    return(X[,index])
  })

  anomaly <- rep(0, n)
  anomaly[ Reduce(union, anom) ] <- 1

  return(setNames( data.frame(X, anomaly), c(seq_len(p), "anomaly")))
}

df <- anomaly_generator()



fit <- encoder_forest( df[,1:10],
                       max_depth = 8,
                       n_tree = 100 )

scores <- isolation( fit, df[,1:10]  )











