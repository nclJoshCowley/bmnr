#' Simulate **Bayesian Matrix Normal Regression**
#'
#' Simulates from a BMNR model.
#'
#' @inheritSection bmnr-package Model description
#'
#' @param n_y integer. Number of dependent variables.
#' @param n_x integer. Number of independent variables.
#' @param regr matrix (\eqn{n_x + 1} by \eqn{n_y}). Regression coefficients.
#' @param covar_y \eqn{\Sigma_y} matrix. Covariance across columns.
#' @param coords,gp_func,...
#'   Determines the covariance across rows, specifically
#'   \eqn{\Sigma_s = } `gp_func(coords, ...)`.
#'
#' @note Setting `gp_func = NULL` implies independence across rows.
#'
#' @return List with class (`bmnr_sim`) containing `$data` and `$params`.
#'
#' @aliases bmnr-sim
#' @export
simulate_bmnr <- function(n_y, n_x, regr, covar_y, coords, gp_func, ...) {
  if (is.null(gp_func)) gp_func <- function(.x) diag(nrow = nrow(.x))

  n_s <- nrow(coords)

  stopifnot("Bad dimensions for 'regr'" = dim(regr) == c(n_x + 1, n_y))

  x_matrix <-
    structure(
      scale(matrix(stats::rnorm(n_s * n_x), nrow = n_s, ncol = n_x)),
      "scaled:center" = NULL,
      "scaled:scale" = NULL,
      dimnames = list(NULL, sprintf("x%02i", seq_len(n_x)))
    )

  y_mean <- cbind(1, x_matrix) %*% regr
  covar_s <- gp_func(coords, ...)

  y_matrix <-
    matrixNormal::rmatnorm(M = y_mean, U = covar_s, V = covar_y) |>
    `colnames<-`(sprintf("y%02i", seq_len(n_y)))

  data <-
    lapply(list(coords, y_matrix, x_matrix), tibble::as_tibble) |>
    dplyr::bind_cols()

  out <-
    list(
      data = data,
      params = rlang::list2(
        regr = regr, covar_y = covar_y, gp_func = gp_func, ...
      )
    )

  return(structure(out, class = "bmnr_sim"))
}



#' @describeIn simulate_bmnr Default arguments for quick simulation.
#' @export
example_simulate_bmnr <- function(...) {
  n_y <- 2
  n_x <- 5

  args <- utils::modifyList(val = rlang::list2(...), list(
    n_y = n_y, n_x = n_x,

    coords = sim_unif_spatiotemporal_coords(n_locations = 20, n_years = 2.25),

    regr = matrix(sample.int(n_y * (n_x + 1)), nrow = n_x + 1, ncol = n_y),

    covar_y = 0.8 + diag(0.2, nrow = n_y),

    gp_func = function(coords, ...) gp_matern32_cov_ard(coords, ...),
    gp_scale = 1,
    gp_length = c(0.1, 0.2, 0.4)
  ))

  # Special case where `gp_func = NULL`
  if ("gp_func" %in% names(rlang::list2(...))) {
    args <- append(args, rlang::list2(...)["gp_func"])
  }

  return(do.call(simulate_bmnr, args))
}



#' Simulate Spatiotemporal Coordinates
#'
#' Get sampling coordinates for `n_locations` with a consistent quarterly
#'   schedule over `n_years` years.
#'
#' @param n_locations integer. Number of locations from \eqn{U(0, 1)^2}.
#' @param n_years integer. Number of years to simulate
#'
#' @returns matrix.
#'   Spatial coordinates are \eqn{[0,1]} and temporal units are years.
#'
#' @keywords internal
sim_unif_spatiotemporal_coords <- function(n_locations, n_years) {
  implied_structure <-
    expand.grid(
      coord_t = seq(from = 0, to = 12 * n_years, by = 3) / 12,
      index_loc = seq_len(n_locations),
      KEEP.OUT.ATTRS = FALSE
    )

  out <-
    dplyr::mutate(
      implied_structure,
      coord_x = stats::runif(n_locations)[.data$index_loc],
      coord_y = stats::runif(n_locations)[.data$index_loc],
      .keep = "unused", .before = 0
    )

  return(as.matrix(out))
}
