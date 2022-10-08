## usethis namespace: start
#' @importFrom Rcpp sourceCpp
## usethis namespace: end
NULL

## usethis namespace: start
#' @useDynLib treecoding, .registration = TRUE
## usethis namespace: end
NULL

Rcpp::loadModule("RandomTreeQRN", TRUE)
