% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/means.R, R/sums.R
\name{means}
\alias{means}
\alias{sums}
\title{Compute means and sums}
\usage{
means(
  ...,
  data = NULL,
  requiredValidValues = 0,
  returnIfInvalid = NA,
  silent = FALSE
)

sums(
  ...,
  data = NULL,
  requiredValidValues = 0,
  returnIfInvalid = NA,
  silent = FALSE
)
}
\arguments{
\item{...}{The dataframe or vectors for which to compute the means or sums.
When passing a dataframe as unnamed argument (i.e. in the "dots", \code{...}),
the means or sums for all columns in the dataframe will be computed. If you
want to select one or more columns, make sure to pass the dataframe as \code{data}.}

\item{data}{If a dataframe is passed as \code{data}, the values passed in
the "dots" (\code{...}) will be taken as column names or indices in that
dataframe. This allows easy indexing.}

\item{requiredValidValues}{The number (if larger than 1) or proportion (if
between 0 and 1) of values that have to be valid (i.e. nonmissing) before the
mean or sum is returned.}

\item{returnIfInvalid}{Which value to return for rows not meeting the
criterion specified in \code{requiredValidValues}.}

\item{silent}{Whether to suppress messages.}
}
\value{
The means or sums.
}
\description{
These functions allow easily computing means and sums. Note that if you
attach \code{rosetta} to the search path,
}
\examples{
rosetta::means(mtcars$mpg, mtcars$disp, mtcars$wt);
rosetta::means(data=mtcars, 'mpg', 'disp', 'wt');
rosetta::sums(mtcars$mpg, mtcars$disp, mtcars$wt);
rosetta::sums(data=mtcars, 'mpg', 'disp', 'wt');
}
