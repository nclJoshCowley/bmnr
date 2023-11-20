utils::globalVariables(".")
utils::globalVariables(":=")

#' Imports
#'
#' Imports from other packages used within this package
#'
#' @name utils-imports
#'
#' @import methods
#' @import Rcpp
#'
#' @importFrom rlang .data
#' @importFrom rlang %||%
#'
#' @keywords internal
#'
#' @section Links:
#'   - [`rlang::dot-data()`], data pronoun.
#'   - [`rlang::op-null-default()`], default value for NULL operator.
#'   - [`rlang::dyn-dots`], walrus operator (`:=`).
NULL


#' @importFrom ggplot2 autoplot
#' @export
ggplot2::autoplot
