#' S4 Class Objects for `bmnr`
#'
#' S4 object, inheriting from [`stanfit`][rstan::stanfit-class], that is
#' returned by [`bmnr`].
#'
#' @import rstan
#' @importFrom methods new
#'
#' @slot data data frame(s). Originally passed data.
#' @slot formula list.
#'     * `$model` describes \eqn{Y} and \eqn{X} variables.
#'     * `$coord` describes \eqn{s} coordinate variables.
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
      formula = "list",
      prior = "list"
    )
  )


#' Model Fit S4 Object
#'
#' @describeIn bmnrfit Special case of `bmnrfit`, rows assumed independent.
#'
#' @export
bmnrfit_mvrnorm_class <-
  setClass(
    Class = "bmnrfit_mvrnorm",
    contains = "bmnrfit"
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


#' Simulation Study S4 Object
#'
#' @describeIn bmnrfit Simulation study subclass
#'   (parent class is `bmnrfit_mvrnorm`).
#'
#' @export
bmnrfit_mvrnorm_simstudy_class <-
  setClass(
    Class = "bmnrfit_mvrnorm_simstudy",
    contains = "bmnrfit_mvrnorm",
    slots = list(params = "list")
  )
