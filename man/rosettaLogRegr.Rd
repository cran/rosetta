% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/logRegr.R
\name{logRegr}
\alias{logRegr}
\alias{rosettaLogRegr_partial}
\alias{knit_print.rosettaLogRegr}
\alias{print.rosettaLogRegr}
\title{Userfriendly wrapper to do logistic regression in R}
\usage{
logRegr(
  formula,
  data = NULL,
  conf.level = 0.95,
  digits = 2,
  predictGroupValue = NULL,
  comparisonGroupValue = NULL,
  pvalueDigits = 3,
  crossTabs = TRUE,
  oddsRatios = TRUE,
  plot = FALSE,
  collinearity = FALSE,
  env = parent.frame(),
  predictionColor = rosetta::opts$get("viridis3")[3],
  predictionAlpha = 0.5,
  predictionSize = 2,
  dataColor = rosetta::opts$get("viridis3")[1],
  dataAlpha = 0.33,
  dataSize = 2,
  observedMeansColor = rosetta::opts$get("viridis3")[2],
  binObservedMeans = 7,
  observedMeansSize = 2,
  observedMeansWidth = NULL,
  observedMeansAlpha = 0.5,
  theme = ggplot2::theme_bw(),
  headingLevel = 3
)

rosettaLogRegr_partial(
  x,
  digits = x$input$digits,
  pvalueDigits = x$input$pvalueDigits,
  headingLevel = x$input$headingLevel,
  echoPartial = FALSE,
  partialFile = NULL,
  quiet = TRUE,
  ...
)

\method{knit_print}{rosettaLogRegr}(
  x,
  digits = x$input$digits,
  headingLevel = x$input$headingLevel,
  pvalueDigits = x$input$pvalueDigits,
  echoPartial = FALSE,
  partialFile = NULL,
  quiet = TRUE,
  ...
)

\method{print}{rosettaLogRegr}(
  x,
  digits = x$input$digits,
  pvalueDigits = x$input$pvalueDigits,
  headingLevel = x$input$headingLevel,
  forceKnitrOutput = FALSE,
  ...
)
}
\arguments{
\item{formula}{The formula, specified in the same way as for
\code{\link[stats:glm]{stats::glm()}} (which is used for the actual analysis).}

\item{data}{Optionally, a dataset containing the variables in the formula
(if not specified, the variables must exist in the environment specified in
\code{env}.}

\item{conf.level}{The confidence level for the confidence intervals.}

\item{digits}{The number of digits used when printing the results.}

\item{predictGroupValue, comparisonGroupValue}{Can optionally be used
to set the value to predict and the value to compare with.}

\item{pvalueDigits}{The number of digits used when printing the p-values.}

\item{crossTabs}{Whether to show cross tabulations of the correct
predictions for the null model and the tested model, as well as the
percentage of correct predictions.}

\item{oddsRatios}{Whether to also present the regression coefficients
as odds ratios (i.e. simply after a call to \code{\link[base:Log]{base::exp()}}).}

\item{plot}{Whether to display the plot.}

\item{collinearity}{Whether to show collinearity diagnostics.}

\item{env}{If no dataframe is specified in \code{data}, use this argument to
specify the environment holding the variables in the formula.}

\item{predictionColor, dataColor, observedMeansColor}{The color of,
respectively, the line and confidence interval showing the prediction; the
points representing the observed data points; and the means based on the
observed data.}

\item{predictionAlpha, dataAlpha, observedMeansAlpha}{The alpha of,
respectively, the confidence interval of the prediction; the points
representing the observed data points; and the means based on the observed
data (set to 0 to hide an element).}

\item{predictionSize, dataSize, observedMeansSize}{The size of, respectively,
the line of the prediction; the points representing the observed data
points; and the means based on the observed data (set to 0 to hide an
element).}

\item{binObservedMeans}{Whether to bin the observed means; either FALSE or a
single numeric value specifying the number of bins.}

\item{observedMeansWidth}{The width of the lines of the observed means. If
not specified (i.e. \code{NULL}), this is computed automatically and set to
the length of the shortest interval between two successive points in the
predictor data series (found using \code{\link[ufs:findShortestInterval]{ufs::findShortestInterval()}}.}

\item{theme}{The theme used to display the plot.}

\item{headingLevel}{The number of hashes to print in front of the headings}

\item{x}{The object to print (i.e. as produced by \code{rosetta::logRegr}).}

\item{echoPartial}{Whether to show the executed code in the R Markdown
partial (\code{TRUE}) or not (\code{FALSE}).}

\item{partialFile}{This can be used to specify a custom partial file. The
file will have object \code{x} available.}

\item{quiet}{Passed on to \code{\link[knitr:knit]{knitr::knit()}} whether it should b
chatty (\code{FALSE}) or quiet (\code{TRUE}).}

\item{\dots}{Any additional arguments are passed to the default print method
by the print method, and to \code{\link[rmdpartials:partial]{rmdpartials::partial()}} when knitting an
RMarkdown partial.}

\item{forceKnitrOutput}{Force knitr output.}
}
\value{
Mainly, this function prints its results, but it also returns them
in an object containing three lists: \item{input}{The arguments specified
when calling the function} \item{intermediate}{Intermediat objects and
values} \item{output}{The results, such as the plot, the cross tables, and
the coefficients.}
}
\description{
This function is meant as a userfriendly wrapper to approximate the way
logistic regression is done in SPSS.
}
\examples{

### Simplest way to call logRegr
rosetta::logRegr(data=mtcars, formula = vs ~ mpg);

### Also ordering a plot
rosetta::logRegr(
  data=mtcars,
  formula = vs ~ mpg,
  plot=TRUE
);

### Only use five bins
rosetta::logRegr(
  data=mtcars,
  formula = vs ~ mpg,
  plot=TRUE,
  binObservedMeans=5
);

\dontrun{
### Mimic output that would be obtained
### when calling from an R Markdown file
rosetta::rosettaLogRegr_partial(
  rosetta::logRegr(
    data=mtcars,
    formula = vs ~ mpg,
    plot=TRUE
  )
);
}

}
\seealso{
\code{\link{regr}} and \code{\link{fanova}} for similar functions
for linear regression and analysis of variance and \code{\link[stats:glm]{stats::glm()}} for the
regular interface for logistic regression.
}
\author{
Ron Pat-El & Gjalt-Jorn Peters (both while at the Open University of
the Netherlands)

Maintainer: Gjalt-Jorn Peters \href{mailto:gjalt-jorn@userfriendlyscience.com}{gjalt-jorn@userfriendlyscience.com}
}
\keyword{hplot}
\keyword{htest}
\keyword{models}
\keyword{nonlinear}
\keyword{regression}
