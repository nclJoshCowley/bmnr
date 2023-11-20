#' Matern 3/2 Kernel with Characteristic Length Scales
#'
#'
#' @param coords matrix. Each row is a vector input to the kernel.
#' @param gp_scale real. Multiplicative amplitude of the kernel function.
#' @param gp_length vector. Multiple length scales, one for each dimension.
#'
#' @note Exposed stan function.
#'
#' @returns Covariance matrix with 'nrow(x)' rows and columns
#'
#' @export
gp_matern32_cov_ard <- function(coords, gp_scale, gp_length) {
  stan_data <- list(
    coords = coords, gp_scale = gp_scale, gp_length = gp_length,
    n_s = nrow(coords), n_inputs = ncol(coords)
  )

  if (is.null(stanmodels$interface_gp_matern32_cov_ard)) {
    stop("Stan file 'interface_gp_matern32_cov_ard' not compiled.")
  }

  out_stanfit <-
    rstan::sampling(
      stanmodels$interface_gp_matern32_cov_ard, data = stan_data,
      chains = 1, iter = 1, warmup = 0, algorithm = "Fixed_param", refresh = 0
    )

  out <- rstan::extract(out_stanfit, pars = "out", permuted = FALSE)
  return(array(out, dim = out_stanfit@par_dims$out))
}
