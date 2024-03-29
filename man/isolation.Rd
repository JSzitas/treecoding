% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/isolation.R
\name{isolation}
\alias{isolation}
\alias{isolation.random_tree}
\alias{isolation.encoder_forest}
\title{Isolation forest score}
\usage{
isolation(object, ...)

\method{isolation}{random_tree}(object, X, ...)

\method{isolation}{encoder_forest}(object, X, ...)
}
\arguments{
\item{object}{The object to use}

\item{...}{Additional arguments passed to individual methods.}

\item{X}{The data to compute for.}
}
\value{
An isolation score object.
}
\description{
Calculate an isolation score from an object (generic method)
}
