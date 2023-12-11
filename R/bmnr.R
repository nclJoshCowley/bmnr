#' Bayesian Matrix Normal Regression
#'
#' @param object formula. Specifies model description; see "Extended Formula".
#' @param data data.frame. Training data that contains all `formula` terms.
#' @param ... Extra arguments passed to [rstan::sampling].
#' @param prior list. Required prior information; see [`bmnr_prior()`].
#' @param mvnorm logical. When `TRUE`, model assumes independence among-rows
#'
#' @note The formula-data interface can be ignored in favour of a [`bmnr-sim`].
#'
#' @inheritSection bmnr-formula Extended Formula
#'
#' @export
bmnr <- function(object, ..., prior) {
  UseMethod("bmnr")
}


#' @rdname bmnr
#' @export
bmnr.bmnr_sim <- function(object, ..., prior, mvnorm = FALSE) {
  y_nms <- grep("^y[0-9]+$", colnames(object$data), value = TRUE)
  x_nms <- grep("^x[0-9]+$", colnames(object$data), value = TRUE)
  coord_nms <- grep("^coord_", colnames(object$data), value = TRUE)

  as_sum <- function(.l) {
    purrr::reduce(rlang::syms(.l), function(.x, .y) rlang::expr(!!.x + !!.y))
  }

  # Independence among-rows
  if (mvnorm) {
    formula <-
      rlang::new_formula(
        lhs = rlang::call2("cbind", !!!rlang::syms(y_nms)),
        rhs = rlang::expr(!!as_sum(x_nms))
      )

    out <- bmnr.formula(formula, object$data, ..., prior = prior)
    return(bmnrfit_mvrnorm_simstudy_class(out, params = object$params))
  }

  # Spatial dependence among-rows
  formula <-
    rlang::new_formula(
      lhs = rlang::call2("cbind", !!!rlang::syms(y_nms)),
      rhs = rlang::expr(!!as_sum(x_nms) | !!as_sum(coord_nms))
    )

  out <- bmnr.formula(formula, object$data, ..., prior = prior)
  return(bmnrfit_simstudy_class(out, params = object$params))
}


#' @rdname bmnr
#' @export
bmnr.formula <- function(object, data, ..., prior) {
  formula <- parse_bmnr_formula(object)

  y <- stats::model.response(stats::model.frame(formula$model, data = data))
  x <- stats::model.matrix(formula$model, data = data)

  # Independence among-rows
  if (is.null(formula$coord)) {
    out <- bmnr_mvnorm_yx(y, x, ..., prior = prior)
    return(
      bmnrfit_mvrnorm_class(out, data = data, formula = formula, prior = prior)
    )
  }

  # Spatial dependence among-rows
  coords <- stats::model.matrix(formula$coord, data = data)
  out <- bmnr_yx(y, x, coords, ..., prior = prior)

  return(
    bmnrfit_class(out, data = data, formula = formula, prior = prior)
  )
}
