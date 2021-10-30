#' Flexible anova
#'
#' This function is meant as a userfriendly wrapper to approximate the way
#' analysis of variance is done in SPSS.
#'
#' This wrapper uses \code{\link{oneway}} and \code{\link{lm}} and
#' \code{\link{lmer}} in combination with \code{car}'s \code{\link{Anova}}
#' function to conduct the analysis of variance.
#'
#' @param data The dataset containing the variables to analyse.
#' @param y The dependent variable. For oneway anova, factorial anova, or
#' ancova, this is the name of a variable in dataframe \code{data}. For
#' repeated measures anova, this is a vector with the names of all variable
#' names in dataframe \code{data}, e.g. \code{c('t0_value', 't1_value',
#' 't2_value')}.
#' @param between A vector with the variables name(s) of the between subjects
#' factor(s).
#' @param covar A vector with the variables name(s) of the covariate(s).
#' @param plot Whether to produce a plot. Note that a plot is only produced for
#' oneway and twoway anova and oneway repeated measures designs: if covariates
#' or more than two between-subjects factors are specified, not plot is
#' produced. For twoway anova designs, the second predictor is plotted as
#' moderator (and the first predictor is plotted on the x axis).
#' @param levene Whether to show Levene's test for equality of variances (using
#' \code{car}'s \code{\link{leveneTest}} function but specifying
#' \code{\link{mean}} as function to compute the center of each group).
#' @param digits Number of digits (actually: decimals) to use when printing
#' results. The p-value is printed with one extra digit.
#' @param contrast This functionality has not been implemented yet.
#' @param x The object to print (i.e. as produced by `regr`).
#' @param \dots Any additional arguments are ignored.
#' @return Mainly, this function prints its results, but it also returns them
#' in an object containing three lists: \item{input}{The arguments specified
#' when calling the function} \item{intermediate}{Intermediat objects and
#' values} \item{output}{The results such as the plot.}
#' @rdname fanova
#' @author Gjalt-Jorn Peters
#'
#' Maintainer: Gjalt-Jorn Peters <gjalt-jorn@@userfriendlyscience.com>
#' @seealso \code{\link{regr}} and \code{\link{logRegr}} for similar functions
#' for linear and logistic regression and \code{\link{oneway}},
#' \code{\link{lm}}, \code{\link{lmer}} and \code{\link{Anova}} for the
#' functions used behind the scenes.
#' @keywords htest hplot
#' @examples
#'
#' ### Oneway anova with a plot
#' fanova(dat=mtcars, y='mpg', between='cyl', plot=TRUE);
#'
#' ### Factorial anova
#' fanova(dat=mtcars, y='mpg', between=c('vs', 'am'), plot=TRUE);
#'
#' ### Ancova
#' fanova(dat=mtcars, y='mpg', between=c('vs', 'am'), covar='hp');
#'
#' ### Don't run these examples to not take too much time during testing
#' ### for CRAN
#' \dontrun{
#' ### Repeated measures anova; first generate datafile
#' dat <- mtcars[, c('am', 'drat', 'wt')];
#' names(dat) <- c('factor', 't0_dependentVar' ,'t1_dependentVar');
#' dat$factor <- factor(dat$factor);
#'
#' ### Then do the repeated measures anova
#' fanova(dat, y=c('t0_dependentVar' ,'t1_dependentVar'),
#'        between='factor', plot=TRUE);
#' }
#'
#' @export fanova
fanova <- function(data,
                   y,
                   between = NULL,
                   covar = NULL,
                   plot = FALSE,
                   levene = FALSE,
                   digits = 2,
                   contrast = NULL) {

  res <- list(input = as.list(environment()),
              intermediate = list(),
              output = list());

  res$intermediate$dataName <- as.character(deparse(substitute(data)));

  if (length(y) == 1) {
    res$intermediate$yVarName <- y;
  } else {
    res$intermediate$yVarName <- ufs::sharedSubString(y);
    if (is.na(res$intermediate$yVarName))
      res$intermediate$yVarName <- vecTxt(y);
  }

  res$output$msg <- paste0("Flexible Analysis of Variance was called with:\n\n",
                           "  Dependent variable: ", res$intermediate$yVarName, "\n",
                           ifelse(is.null(between), "", paste0("  Factors: ", vecTxt(between), "\n")),
                           ifelse(is.null(covar), "", paste0("  Covariates: ", vecTxt(covar), "\n")),
                           "\n");

  ### Set contrast function; first set default contrast for repeated
  ### measures anova
  if (is.null(contrast)) {
    #contrast <- 'poly'
    contrastFunction <- NULL
  }
  if (!is.null(contrast)) {
    contrastFunction <- paste0('contr.', contrast);
    if (!exists(contrastFunction)) {
      stop("Function doesn't exist");
    }
  }

  ### Convert 'between' variables (factors) to factors
  for (currentVar in seq_along(between)) {
    if (!is.factor(data[, between[currentVar]])) {
      cat0("Between-subjects factor ", between[currentVar],
           " does not have class 'factor' in dataframe '",
           res$intermediate$dataName, "'. Converting it now.\n");
      data[, between[currentVar]] <- factor(data[, between[currentVar]]);
    }
  }

  if (length(y) == 1) {

    ### oneway, factorial anova or ancova

      ### Generate formula (interactions between factors only)
      factorPart <- paste(y, "~", paste(c(between), collapse="*"))
      res$intermediate$formula <-
        stats::formula(paste(c(factorPart, covar), collapse = "+"));

      ### Run linear model
      res$intermediate$primaryObject <-
        stats::lm(formula=res$intermediate$formula,
           data = data,
           contrasts = contrastFunction);

      ### Get Anova results using car's Anova
      res$intermediate$secondaryObject <-
        car::Anova(res$intermediate$primaryObject, type=3);

      ### Compute omega effect sizes
      linmodel <- res$intermediate$primaryObject
      tvals <- summary(linmodel)$coefficients[,3]
      dfs <- summary(linmodel)$df
      fstat <- summary(linmodel)$fstatistic

      if (requireNamespace("effectsize", quietly = TRUE)) {
        omegasq1 <- effectsize::F_to_omega2(f=fstat[1], df=fstat[2], df_error=fstat[3]);
        omegasq2 <- effectsize::t_to_omega2(t=tvals, df_error=dfs[2]);
        omegasq2[1,] <- omegasq1;
        ome <- as.data.frame(round(omegasq2[,-2],3));
        rownames(ome)[1] <- "model";
        res$intermediate$omegas <- ome;
      } else {
        omegasq1 <- ufs::convert.f.to.omegasq(f=fstat[1], df1=fstat[2], df2=fstat[3]);
        omegasq2 <- NA;
        res$intermediate$omegas <- omegasq1;
      }

      ### Make a plot if we want one
      if (plot) {
        if (is.null(covar) && (length(between) < 3)) {
          if (length(between) == 1) moderator <- NULL
          if (length(between) == 2) moderator <- between[2]
          res$output$plot <- dlvPlot(data,
                                     y=y,
                                     x=between[1],
                                     z=moderator)$plot
        } else {
          warning("Sorry, I can only generate a plot for oneway or ",
                  "two-way anovas (not for ancovas or anovas with ",
                  "more than two factors).");
        }
      }

      ### Optional Levene's test
      if (levene) {
        if (length(between) == 1) {
          leveneGroups <- as.factor(data[,between])
        } else {
          leveneGroups <-
            as.factor(apply(data[, between], 1, function(x) return(paste0(x, collapse="-"))))
        }
        res$intermediate$leveneTest <-
          car::leveneTest(y = data[,y], group = leveneGroups)
      }


  } else {
    ### We need to do a repeated measures anova, so first convert the
    ### data to a long format.
    longDat <- data.frame(subject = factor(rep(row.names(data), length(y))),
                          time = factor(rep(seq_along(y), each=nrow(data))),
                          y = unlist(data[, y]));
    for (currentVar in between) {
      longDat[, currentVar] <- factor(rep(data[, currentVar],
                                          length(y)));
    }
    for (currentVar in covar) {
      longDat[, currentVar] <- as.numeric(rep(data[, currentVar],
                                              length(y)));
    }

    res$intermediate$longDat <- longDat;

    ### Then build the lmer formula: y is predicted by all
    ### interactions and main effects for all factors ('between')
    ### and covariates ('covar'), all interactions between all
    ### factors and the time variable (called 'time'), and
    ### the random slope for time (the final term).

    covarPart <- NULL
    covarPart <- if (!is.null(covar)) paste(c(covar), collapse=" + ")
    factorPart <- paste(c(between, "time"), collapse=" * ")
    res$intermediate$formula <-
      stats::formula(paste(paste("y ~",
                                 paste(c(covarPart, factorPart), collapse=" + "),
                                 "+ (1|time)")))


    ### Run the mixed model
    res$intermediate$primaryObject <-
      lme4::lmer(formula=res$intermediate$formula,
           data = longDat,
           contrasts = contrastFunction);

    ### Run the analysis of variance
    suppressMessages(res$intermediate$secondaryObject <-
      car::Anova(res$intermediate$primaryObject,
                 type=3, test.statistic="F"));

    ### Approach using lm (and the idata and idesign objects for Anova)
    # idata <- longDat; #[, 'time', drop=FALSE];
    # print(names(longDat))
    # res$intermediate$formula <- formula(paste0("cbind(", paste(y, collapse=", "), ") ~",
    #                                            paste(c(between, covar), collapse=" * ")));
    # res$intermediate$primaryObject <- lm(res$intermediate$formula,
    #                                      data = data);
    # print(res$intermediate$primaryObject);
    # print(idata);
    # res$intermediate$secondaryObject <- Anova(res$intermediate$primaryObject,
    #                                           idata=idata,
    #                                           idesign=~1*time,
    #                                           type=3,
    #                                           test.statistic='F');

    if (plot) {
      if (length(between) > 1) {
        warning("Sorry, I can only generate a plot for ",
                "oneway repeated measures anovas.");
      } else {
        res$output$plot <-
          dlvPlot(longDat,
                  x='time',
                  y='y',
                  z=between)$plot +
          ggplot2::labs(x='Time', y=res$intermediate$yVarName);
      }
    }

  }


  class(res) <- 'fanova';
  return(res);

}

#' @method print fanova
#' @rdname fanova
#' @export
print.fanova <- function(x, digits=x$input$digits, ...) {
  cat(x$output$msg);
  cat("\n");
  if (!is.null(x$output$plot)) {
    grid::grid.newpage();
    grid::grid.draw(x$output$plot);
  }
  print(x$intermediate$secondaryObject);

  if (length(x$input$y) == 1) {
    cat0("\n\nPartial omega squared effect sizes \n")
    print(x$intermediate$omegas)
    cat0("\n\nParameter estimates, omnibus F-test and R-squared values \n")
  }

  if (length(x$input$y) > 1) cat0("\n\nMultilevel results \n\n")

  print(summary(x$intermediate$primaryObject), correlation = FALSE)

  if(!is.null(x$intermediate$leveneTest)) {
    cat0("\n### Levene's test for homogeneity of variances:\n\n",
         "F[", x$intermediate$leveneTest[1, 1],
         ", ", x$intermediate$leveneTest[2, 1],
         "] = ", round(x$intermediate$leveneTest[1, 2], digits),
         ", ", formatPvalue(x$intermediate$leveneTest[1, 3], digits=digits+1),
         ".\n");
  }

}
