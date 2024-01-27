#' Bayesian Matrix Normal Regression Package
#'
#' Fits a Matrix normal distribution where the mean is a linear predictor,
#'   covariance across rows is assumed Gaussian process (GP) and
#'   covariance across columns is estimated.
#'
#' @param object Object that inherits from [`bmnrfit`].
#' @param new_data data frame. Data used to create a design matrices.
#'   Setting `new_data = NULL` utilises the pre-trained data (special case).
#'
#' @docType package
#' @name bmnr-package
#' @useDynLib bmnr, .registration = TRUE
#' @import methods
#' @import Rcpp
#' @importFrom rstan sampling
#'
#'
#' @section Model description:
#' Suppose we observe two matrices,
#'   - \eqn{Y}: \eqn{n_s} observations for \eqn{n_y} dependent variables;
#'   - \eqn{X}: \eqn{n_s} observations for \eqn{n_x} independent variables.
#'
#' Our model assumes
#'   \deqn{
#'     Y \sim MN_{n_s, n_y}(XB, \Sigma_s, \Sigma_y),
#'   }
#' where
#'   - \eqn{B}, \eqn{n_x + 1} by \eqn{n_y} regression coefficient matrix;
#'   - \eqn{\Sigma_s}, (across row) covariance matrix, determined by a Gaussian
#'     process (function of \eqn{n_s} coordinates and hyperparameters);
#'   - \eqn{\Sigma_y}, (across column) covariance matrix.
#'
#'
#' @section Prior Information:
#' It is assumed, *a priori*, that
#'   - \eqn{B_{ij} \sim N(0, \tau_B)} where \eqn{\tau_B =} `regr_prec`.
#'   - **TODO**. Complete prior section.
#'
#'
#' @references
#' Stan Development Team (2023). RStan: the R interface to Stan.
#' R package version 2.32.3. <https://mc-stan.org>
#'
NULL
