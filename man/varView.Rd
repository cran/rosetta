% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/varView.R
\name{varView}
\alias{varView}
\alias{print.rosettaVarView}
\title{Variable View}
\usage{
varView(
  data,
  columns = names(data),
  varViewCols = rosetta::opts$get(varViewCols),
  varViewRownames = TRUE,
  maxLevels = 10,
  truncLevelsAt = 50,
  showLabellerWarning = rosetta::opts$get(showLabellerWarning),
  output = rosetta::opts$get("tableOutput")
)

\method{print}{rosettaVarView}(x, output = attr(x, "output"), ...)
}
\arguments{
\item{data}{The dataframe containing the variables to view.}

\item{columns}{The columns to include.}

\item{varViewCols}{The columns of the variable view.}

\item{varViewRownames}{Whether to set the variable names as
row names of the variable view dataframe that is returned.}

\item{maxLevels}{For factors, the maximum number of levels to
show.}

\item{truncLevelsAt}{For factors levels, the number of characters
at which to truncate.}

\item{showLabellerWarning}{Whether to show a warning if labeller
labels are encountered.}

\item{output}{A character vector containing one or more of
"\code{console}", "\code{viewer}", and one or more filenames in existing
directories. If \code{output} contains \code{viewer} and RStudio is used,
the variable view is shown in the RStudio viewer.}

\item{x}{The varView data frame to print.}

\item{...}{Any additional arguments are passed along to
the \code{\link[=print.data.frame]{print.data.frame()}} function.}
}
\value{
A dataframe with the variable view.
}
\description{
This function provides an overview of the variables in a
dataframe, allowing efficient inspection of the factor levels,
ranges for numeric variables, and numbers of missing values.
}
\examples{
### The default variable view
rosetta::varView(iris);

### Only for a few variables in the dataset
rosetta::varView(iris, columns=c("Sepal.Length", "Species"));

### Set some variable and value labels using the `labelled`
### standard, which is also used by `haven`
dat <- iris;
attr(dat$Sepal.Length, "label") <- "Sepal length";
attr(dat$Sepal.Length, "labels") <-
  c('one' = 1,
    'two' = 2,
    'three' = 3);

### varView automatically recognizes and shows these, adding
### a 'label' column
rosetta::varView(dat);

### You can also specify that you only want to see some columns
### in the variable view
rosetta::varView(dat,
                 varViewCols = c('label', 'values', 'level'));


}
\author{
Gjalt-Jorn Peters & Melissa Gordon Wolf
}
