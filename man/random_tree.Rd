% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/random_tree.R
\name{random_tree}
\alias{random_tree}
\title{Build a random tree}
\usage{
random_tree(
  X,
  max_depth = 10,
  split_finder = find_rule,
  nosplit_columns = NULL,
  row_id = NULL,
  chronological_oob = TRUE,
  ...
)
}
\arguments{
\item{X}{The data to use - currently only supports a matrix.}

\item{max_depth}{The maximal depth of the tree (though the tree might be shorter - this is an upper bound).}

\item{split_finder}{A function to find splits.}

\item{nosplit_columns}{Columns to ignore when splitting - but which are nonetheless propagated to
the terminal node.}

\item{row_id}{A subset of rows to use for growing the tree - if **NULL**, use all rows.}

\item{chronological_oob}{Use only observations which came **after** the training set for errors?}

\item{...}{Additional arguments.}
}
\value{
A fitted tree
}
\description{
Fit a random tree to your data.
}
