#' Parse [`bmnr`] Formula
#'
#' Description of how to specify models with coordinate metadta.
#'
#' @param object formula. User supplied model description.
#'
#' @section Extended Formula:
#' For more information on extending `formula`, see the `Formula` package but
#' be warned about <https://github.com/rstudio/rstudio/issues/12409>.
#'
#' Specific a standard LHS as one would with [`stats::lm()`] using `cbind`.
#' * `cbind(y01, y02) ~ x01 + x02`
#' * `cbind(y01, y02) ~ .`
#'
#' The RHS is split into predictors and coordinate information split into two
#' parts using `"|"`
#'
#' For example,
#' * `cbind(y01, y02) ~ x01 + x02 + x03 | coord_x + coord_y`
#'
#' @name bmnr-formula
NULL


#' @rdname bmnr-formula
#' @export
parse_bmnr_formula <- function(object) {
  stopifnot(
    inherits(object, "formula"),
    "Formula length mismatch, is LHS specified?" = (length(object) == 3)
  )

  lhs <- object[[2]]
  rhs <- object[[3]]

  if (length(rhs) == 1 || rhs[[1]] != "|") {
    return(list(model = object, coord = NULL))
  }

  return(list(
    model = rlang::new_formula(lhs, rhs[[2]]),
    coord = rlang::new_formula(NULL, rlang::expr(!!rhs[[3]] - 1))
  ))
}
