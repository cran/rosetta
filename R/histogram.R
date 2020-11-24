#' Simple function to create a histogram
#'
#' @param vector A variable or vector.
#' @param bins The number of bins; when 0, either the number of unique
#' values in `vector` or `20`, whichever is lower.
#' @param theme The ggplot2 theme to use.
#'
#' @return A ggplot2 plot.
#' @export
#'
#' @examples rosetta::histogram(mtcars$mpg);
histogram <- function(vector,
                      bins = NULL,
                      theme = ggplot2::theme_bw()) {
  xName <- deparse(substitute(vector));
  tmpDf <- data.frame(x = vector);
  names(tmpDf) <- xName;
  if (is.null(bins)) {
    bins <-
      min(length(unique(stats::na.omit(vector))), 20);
  }
  return(
    ggplot2::ggplot(data = tmpDf,
                    mapping = ggplot2::aes_string(x = xName)) +
      ggplot2::geom_histogram(bins = bins) +
      ggplot2::labs(x = xName,
                    y = 'Count') +
      theme
  );
}
