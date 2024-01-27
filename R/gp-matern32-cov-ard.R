#' Matern 3/2 Kernel with Characteristic Length Scales
#'
#' @param x_r,x_c matrix. Each row is a vector input to the kernel.
#' @param gp_scale real. Multiplicative amplitude of the kernel function.
#' @param gp_length vector. Multiple length scales, one for each dimension.
#'
#' @note Exposed stan function.
#'
#' @returns Covariance matrix with 'nrow(x)' rows and columns
#'
#' @export
gp_matern32_cov_ard <- function(x_r, gp_scale, gp_length, x_c = NULL) {
  if (is.null(x_c)) x_c <- matrix(nrow = 0, ncol = ncol(x_r))

  stan_data <- list(
    n_inputs = ncol(x_r), n_r = nrow(x_r), n_c = nrow(x_c),
    x_r = x_r, x_c = x_c,
    gp_scale = gp_scale, gp_length = gp_length
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
