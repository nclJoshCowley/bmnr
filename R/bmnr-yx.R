#' Implementation of BMNR Model
#'
#' @param y matrix Dependent variables.
#' @param x matrix. Explanatory variables.
#' @param coords matrix. Gaussian process inputs.
#' @inheritParams bmnr
#'
#' @keywords internal
bmnr_yx <- function(y, x, coords, ..., prior) {
  mcmc_data <-
    list(
      n_s = nrow(y), n_y = ncol(y), n_x = ncol(x), n_gp_dims = ncol(coords),
      y = y, x = x, coords = coords
    )

  message("Exectuing MCMC algorithm ...")

  out <-
    rstan::sampling(
      object = stanmodels$bmnr,
      data = c(mcmc_data, prior),
      pars = c("regr", "covar_y", "gp_length"),
      ...
    )

  return(out)
}
