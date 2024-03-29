### This function is based on the function by GitHub user Rentrop (https://github.com/rentrop),
### taken from the gist at https://gist.github.com/rentrop/d39a8406ad8af2a1066c,
### which itself uses code from 'car:::qqPlot'.



#' Easy ggplot Q-Q plot
#'
#' This function creates a qq-plot with a confidence interval.
#'
#' This is strongly based on the answer by user Floo0 to a Stack Overflow
#' question at Stack Exchange (see
#' \url{https://stackoverflow.com/questions/4357031/qqnorm-and-qqline-in-ggplot2/27191036#27191036}),
#' also posted at GitHub (see
#' \url{https://gist.github.com/rentrop/d39a8406ad8af2a1066c}). That code is in
#' turn based on the `qqPlot()` function from the `car` package.
#'
#' @param x A vector containing the values to plot.
#' @param distribution The distribution to (a 'd' and 'q' are prepended, and
#' the resulting functions are used, e.g. \code{\link{dnorm}} and
#' \code{\link{qnorm}} for the normal curve).
#' @param \dots Any additional arguments are passed to the quantile function
#' (e.g. \code{\link{qnorm}}). Because of these dots, any following arguments
#' must be named explicitly.
#' @param ci Whether to show the confidence interval.
#' @param line.estimate Whether to show the line showing the match with the
#' specified distribution (e.g. the normal distribution).
#' @param conf.level THe confidence of the confidence leven arround the
#' estimate for the specified distribtion.
#' @param sampleSizeOverride It can be desirable to get the confidence
#' intervals for a different sample size (when the sample size is very large,
#' for example, such as when this plot is generated by the function
#' [ufs::normalityAssessment()]. That different sample size can be
#' specified here.
#' @param observedOnX Whether to plot the observed values (if \code{TRUE}) or
#' the theoretically expected values (if \code{FALSE}) on the X axis. The other
#' is plotted on the Y axis.
#' @param scaleExpected Whether the scale the expected values to match the
#' scale of the variable. This option is provided to be able to mimic SPSS' Q-Q
#' plots.
#' @param theoryLab The label for the theoretically expected values (on the Y
#' axis by default).
#' @param observeLab The label for the observed values (on the Y axis by
#' default).
#' @param theme The theme to use.
#' @return A \code{\link{ggplot}} plot is returned.
#' @author John Fox and Floo0; implemented in this package (and tweaked a bit)
#' by Gjalt-Jorn Peters.
#'
#' Maintainer: Gjalt-Jorn Peters <gjalt-jorn@@userfriendlyscience.com>
#' @keywords hplot
#' @examples
#'
#' ggqq(mtcars$mpg);
#'
#' @export ggqq
ggqq <- function(x, distribution = "norm", ...,
                 ci = TRUE,
                 line.estimate = NULL,
                 conf.level = 0.95,
                 sampleSizeOverride = NULL,
                 observedOnX = TRUE,
                 scaleExpected = TRUE,
                 theoryLab = "Theoretical quantiles",
                 observeLab = "Observed quantiles",
                 theme = ggplot2::theme_bw()){

  q.function <- eval(parse(text = paste0("stats::q", distribution)));
  d.function <- eval(parse(text = paste0("stats::d", distribution)));
  x <- stats::na.omit(x);
  ord <- order(x);
  n <- ifelse(is.null(sampleSizeOverride), length(x), sampleSizeOverride);
  P <- stats::ppoints(length(x));
  df <- data.frame(ord.x = x[ord], z = q.function(P, ...));

  if (scaleExpected) {
    df$expected <- (df$z * stats::sd(x, na.rm=TRUE)) + mean(x, na.rm=TRUE);
  }

  if(is.null(line.estimate)){
    Q.x <- stats::quantile(df$ord.x, c(0.25, 0.75));
    Q.z <- q.function(c(0.25, 0.75), ...);

    if (scaleExpected) {
      Q.z <- (Q.z * stats::sd(x, na.rm=TRUE)) + mean(x, na.rm=TRUE);
    }

    b <- diff(Q.x)/diff(Q.z);
    coef <- c(Q.x[1] - b * Q.z[1], b);
  } else {
    coef <- stats::coef(line.estimate(ord.x ~ z));
  }

  zz <- stats::qnorm(1 - (1 - conf.level)/2);

  if (scaleExpected) {
    SE <- (coef[2]/d.function(df$z)) * sqrt(P * (1 - P)/n) * stats::sd(x, na.rm=TRUE);
    fit.value <- coef[1] + coef[2] * df$expected;
    df$z <- df$expected;
    df$upper <- fit.value + zz * SE;
    df$lower <- fit.value - zz * SE;
  } else{
    SE <- (coef[2]/d.function(df$z)) * sqrt(P * (1 - P)/n);
    fit.value <- coef[1] + coef[2] * df$z;
    df$upper <- fit.value + zz * SE;
    df$lower <- fit.value - zz * SE;
  }

  p <- ggplot2::ggplot(df, ggplot2::aes_string(x='z', y='ord.x')) +
    ggplot2::geom_point() +
    ggplot2::geom_abline(intercept = coef[1], slope = coef[2]) +
    ggplot2::xlab(theoryLab) +
    ggplot2::ylab(observeLab) + theme;

  if (ci) {
    p <- p +
      ggplot2::geom_ribbon(ggplot2::aes_string(ymin = 'lower', ymax = 'upper'), alpha=0.2);
  }

  if (observedOnX) {
    p <- p + ggplot2::coord_flip();
  }

  return(p);
}
