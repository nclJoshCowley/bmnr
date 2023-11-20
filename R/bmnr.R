#' Bayesian Matrix Normal Regression
#'
#' @param object Typically a formula (with `data`) but can be [`bmnr_sim`].
#' @param coord_formula RHS only formula. Specifies coordinate data.\
#'   **Note**. Future versions will pull this argument into `formula`.
#' @param data data.frame. Training data that contains all `formula` terms.
#' @param ... Extra arguments passed to [rstan::sampling].
#' @param prior list. Required prior information; see [`bmnr_prior()`].
#'
#' @export
bmnr <- function(object, ..., prior) {
  UseMethod("bmnr")
}


#' @rdname bmnr
#' @export
bmnr.bmnr_sim <- function(object, ..., prior) {
  y_nms <- grep("^y[0-9]+$", colnames(object$data), value = TRUE)
  x_nms <- grep("^x[0-9]+$", colnames(object$data), value = TRUE)
  coord_nms <- grep("^coord_", colnames(object$data), value = TRUE)

  formula <-
    stats::reformulate(
      response = rlang::call2("cbind", !!!rlang::syms(y_nms)),
      termlabels = x_nms
    )

  coord_formula <- stats::reformulate(coord_nms, intercept = FALSE)

  out <- bmnr.formula(formula, coord_formula, object$data, ..., prior = prior)

  return(bmnrfit_simstudy_class(out, params = object$params))
}


#' @rdname bmnr
#' @export
bmnr.formula <- function(object, coord_formula, data, ..., prior) {
  mf <- stats::model.frame(object, data = data)
  y <- stats::model.response(mf)
  x <- stats::model.matrix(mf, data = data)

  coords <-
    coord_formula |>
    stats::update.formula(new = ~ . - 1) |>
    stats::model.frame(data = data) |>
    stats::model.matrix(data = data)

  out <- bmnr_yx(y, x, coords, ..., prior = prior)

  return(
    bmnrfit_class(
      out,
      data = data,
      formula = object,
      coord_formula = coord_formula,
      prior = prior
    )
  )
}
