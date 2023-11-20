#' S4 Class Objects for `bmnr`
#'
#' S4 object, inheriting from [`stanfit`][rstan::stanfit-class], that is
#' returned by [`bmnr`].
#'
#' @import rstan
#' @importFrom methods new
#'
#' @slot data data frame(s). Originally passed data.
#' @slot formula formula. Distinguishes \eqn{Y} and \eqn{X} data.
#' @slot coord_formula formula. Distinguishes coordinate data.
#' @slot prior list. Originally passed prior hyperparameters.
#' @slot params list. Underlying parameter values, used in plotting.
#'
#' @aliases bmnrfit bmnrfit_simstudy
#' @name bmnrfit-class
NULL


#' Model Fit S4 Object
#'
#' @describeIn bmnrfit General purpose S4 class (parent class is `stanfit`).
#'
#' @export
bmnrfit_class <-
  setClass(
    Class = "bmnrfit",
    contains = "stanfit",
    slots = list(
      data = "data.frame",
      formula = "formula",
      coord_formula = "formula",
      prior = "list"
    )
  )


#' Simulation Study S4 Object
#'
#' @describeIn bmnrfit Simulation study subclass (parent class is `bmnrfit`).
#'
#' @aliases bmnrfit_simstudy
#' @export
bmnrfit_simstudy_class <-
  setClass(
    Class = "bmnrfit_simstudy",
    contains = "bmnrfit",
    slots = list(params = "list")
  )
