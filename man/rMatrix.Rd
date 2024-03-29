% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rMatrix.R
\name{rMatrix}
\alias{rMatrix}
\alias{print.rMatrix}
\title{Correlation matrix}
\usage{
rMatrix(
  dat,
  x,
  y = NULL,
  conf.level = 0.95,
  correction = "fdr",
  digits = 2,
  pValueDigits = 3,
  colspace = 2,
  rowspace = 0,
  colNames = "numbers"
)

\method{print}{rMatrix}(
  x,
  digits = x$digits,
  pValueDigits = x$pValueDigits,
  colNames = x$colNames,
  ...
)
}
\arguments{
\item{dat}{A dataframe containing the relevant variables.}

\item{x}{Vector of 1+ variable names.}

\item{y}{Vector of 1+ variable names; if this is left empty, a symmetric
matrix is created; if this is filled, the matrix will have the x variables
defining the rows and the y variables defining the columns.}

\item{conf.level}{The confidence of the confidence intervals.}

\item{correction}{Correction for multiple testing: an element out of the
vector c("holm", "hochberg", "hommel", "bonferroni", "BH", "BY", "fdr",
"none"). NOTE: the p-values are corrected for multiple testing; The
confidence intervals are not (yet :-)).}

\item{digits}{With what precision do you want the results to print.}

\item{pValueDigits}{Determines the number of digits to use when displaying p
values. P-values that are too small will be shown as p<.001 or p<.00001 etc.}

\item{colspace}{Number of spaces between columns}

\item{rowspace}{Number of rows between table rows (note: one table row is 2
rows).}

\item{colNames}{colNames can be "numbers" or "names". "Names" cause
variables names to be printed in the heading; "numbers" causes the rows to
become numbered and the numbers to be printed in the heading.}

\item{...}{Additional arguments are ignored.}
}
\value{
An \code{rMatrix} object that when printed shows the correlation matrix

An object with the input and several output variables. Most notably a number
of matrices: \item{r}{Pearson r values.} \item{parameter}{Degrees of
freedom.} \item{ci.lo}{Lower bound of Pearson r confidence interval.}
\item{ci.hi}{Upper bound of Pearson r confidence interval.}
\item{p.raw}{Original p-values.} \item{p.adj}{p-values adjusted for multiple
testing.}
}
\description{
rMatrix provides a correlation matrix with confidence intervals and a
p-value adjusted for multiple testing.
}
\details{
rMatrix provides a symmetric or asymmetric matrix of correlations, their
confidence intervals, and p-values. The p-values can be corrected for
multiple testing.
}
\examples{


rMatrix(mtcars, x=c('disp', 'hp', 'drat'))


}
\author{
Gjalt-Jorn Peters

Maintainer: Gjalt-Jorn Peters \href{mailto:gjalt-jorn@userfriendlyscience.com}{gjalt-jorn@userfriendlyscience.com}
}
\keyword{utilities}
