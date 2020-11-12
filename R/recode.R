#' Recode a Variable (`car` version)
#'
#' This function is from the \pkg{car} package. Please see that
#' help page for details: [car::recode()].
#'
#' @param var numeric vector, character vector, or factor.
#' @param recodes character string of recode specifications: see below.
#' @param as.factor return a factor; default is `TRUE` if var is a
#' factor, `FALSE` otherwise.
#' @param as.numeric if `TRUE` (the default), and `as.factor` is `FALSE`,
#' then the result will be coerced to numeric if all values in the result
#' are numerals—i.e., represent numbers.
#' @param levels an optional argument specifying the order of the levels
#' in the returned factor; the default is to use the sort order of the
#' level names.
#'
#' @export
#' @author John Fox \email{jfox@@mcmaster.ca}
#' @references Fox, J. and Weisberg, S. (2019) *An R Companion to
#' Applied Regression*, Third Edition, Sage.
#'
#' @examples x<-rep(1:3,3)
#' x
#' rosetta::recode(
#'   x,
#'   "c(1,2)='A'; else='B'"
#' );
#' rosetta::recode(
#'   x,
#'   "1:2='A'; 3='B'"
#' );
recode <- car::recode;
