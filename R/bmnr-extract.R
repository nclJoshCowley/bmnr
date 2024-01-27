#' Extract Model Parameters
#'
#' Extracts or calculates key model parameters and derived quantities
#'
#' @inheritParams bmnr-package
#'
#' @name extract-pars
NULL


#' @rdname extract-pars
#' @export
extract_regr <- function(object) {
  n_iters <- object@sim$iter - object@sim$warmup
  n_chains <- object@sim$chains

  array(
    rstan::extract(object, "regr", permuted = FALSE),
    dim = c(n_iters, n_chains, object@par_dims$regr)
  )
}


#' @rdname extract-pars
#' @export
extract_covar_y <- function(object) {
  n_iters <- object@sim$iter - object@sim$warmup
  n_chains <- object@sim$chains

  array(
    rstan::extract(object, "covar_y", permuted = FALSE),
    dim = c(n_iters, n_chains, object@par_dims$covar_y)
  )
}


#' @rdname extract-pars
#' @export
extract_gp_length <- function(object) {
  n_iters <- object@sim$iter - object@sim$warmup
  n_chains <- object@sim$chains

  array(
    rstan::extract(object, "covar_y", permuted = FALSE),
    dim = c(n_iters, n_chains, object@par_dims$gp_length)
  )
}


#' @rdname extract-pars
#' @export
extract_linpred <- function(object, new_data) {
  if (is.null(new_data)) new_data <- object@data

  n_s <- nrow(new_data)
  n_y <- object@par_dims$covar_y[1]
  n_iters <- object@sim$iter - object@sim$warmup
  n_chains <- object@sim$chains

  x <-
    stats::model.matrix(
      stats::delete.response(stats::terms(object@formula$model)),
      data = new_data
    )

  mcmc_regr <- extract_regr(object)

  out <- array(NA_real_, dim = c(n_iters, n_chains, n_s, n_y))

  for (ii in seq_len(n_iters)) {
    for (ic in seq_len(n_chains)) {
      out[ii, ic, , ] <- x %*% mcmc_regr[ii, ic, , ]
    }
  }

  return(out)
}


#' @describeIn extract-pars
#'   Alternative output when [`cov2cor`] is applied to each value of `covar_y`.
#' @export
extract_corr_y <- function(object) {
  n_iters <- object@sim$iter - object@sim$warmup
  n_chains <- object@sim$chains

  covar_y <- bmnr::extract_covar_y(object)
  out <- array(NA, dim = dim(covar_y))

  for (ii in seq_len(n_iters)) {
    for (ic in seq_len(n_chains)) {
      out[ii, ic, , ] <- stats::cov2cor(covar_y[ii, ic, , ])
    }
  }

  return(out)
}


#' @rdname extract-pars
#' @param .nugget numeric. Added to main diagonal for computation stability.
#' @export
extract_covar_s <- function(object, new_data, .nugget = 1e-8) {
  if (object@model_name == "bmnr_mvnorm") {
    stop("Parameter 'covar_s' not defined for 'bmnr_mvnorm' model")
  }

  if (is.null(new_data)) new_data <- object@data

  coords <-
    stats::model.matrix(
      stats::delete.response(stats::terms(object@formula$coord)),
      data = new_data
    )

  n_s <- nrow(new_data)
  n_iters <- object@sim$iter - object@sim$warmup
  n_chains <- object@sim$chains

  message("Simulating from many GPs, this may take a while")
  .pb <- knitrProgressBar::progress_estimated(n_iters * n_chains)

  mcmc_gp_length <- extract_gp_length(object)

  out <- array(NA_real_, dim = c(n_iters, n_chains, n_s, n_s))

  for (ii in seq_len(n_iters)) {
    for (ic in seq_len(n_chains)) {
      knitrProgressBar::update_progress(.pb)

      out[ii, ic, , ] <-
        gp_matern32_cov_ard(
          x_r = coords,
          gp_scale = 1,
          gp_length = mcmc_gp_length[ii, ic, ]
        )

      if (!is.null(.nugget)) {
        out[ii, ic, , ] <- out[ii, ic, , ] + diag(.nugget, nrow = n_s)
      }
    }
  }

  return(out)
}


#' @rdname extract-pars
#' @export
extract_pointwise_log_lik <- function(object, new_data) {
  UseMethod("extract_pointwise_log_lik")
}


#' @rdname extract-pars
#' @export
extract_pointwise_log_lik.bmnrfit_mvrnorm <- function(object, new_data) {
  if (is.null(new_data)) new_data <- object@data

  n_s <- nrow(new_data)
  n_y <- object@par_dims$covar_y[1]
  n_iters <- object@sim$iter - object@sim$warmup
  n_chains <- object@sim$chains

  y <-
    stats::model.response(
      stats::model.frame(object@formula$model, data = new_data)
    )

  mcmc_linpred <- bmnr::extract_linpred(object, new_data)
  mcmc_covar_y <- bmnr::extract_covar_y(object)

  out <- array(NA_real_, dim = c(n_iters, n_chains, n_s))

  message("Evaluating multivariate normal densities, this may take a while")
  .pb <- knitrProgressBar::progress_estimated(n_iters * n_chains)

  for (ii in seq_len(n_iters)) {
    for (ic in seq_len(n_chains)) {
      knitrProgressBar::update_progress(.pb)

      for (i in seq_len(n_s)) {
        out[ii, ic, i] <-
          mvtnorm::dmvnorm(
            x = y[i, ],
            mean = mcmc_linpred[ii, ic, i, ],
            sigma = mcmc_covar_y[ii, ic, , ],
            log = TRUE
          )
      }
    }
  }

  return(out)
}


#' @rdname extract-pars
#' @export
extract_pointwise_log_lik.bmnrfit <- function(object, new_data) {
  if (is.null(new_data)) new_data <- object@data

  n_s <- nrow(new_data)
  n_y <- object@par_dims$covar_y[1]
  n_iters <- object@sim$iter - object@sim$warmup
  n_chains <- object@sim$chains

  y <-
    stats::model.response(
      stats::model.frame(object@formula$model, data = new_data)
    )

  mcmc_linpred <- bmnr::extract_linpred(object, new_data)
  mcmc_covar_y <- bmnr::extract_covar_y(object)
  mcmc_covar_s <- bmnr::extract_covar_s(object, new_data)

  out <- array(NA_real_, dim = c(n_iters, n_chains, 1))

  message("Evaluating matrix-variate normal densities, this may take a while")
  .pb <- knitrProgressBar::progress_estimated(n_iters * n_chains)

  for (ii in seq_len(n_iters)) {
    for (ic in seq_len(n_chains)) {
      knitrProgressBar::update_progress(.pb)

      out[ii, ic, ] <-
        matrixNormal::dmatnorm(
          A = y,
          M = mcmc_linpred[ii, ic, , ],
          U = mcmc_covar_s[ii, ic, , ],
          V = mcmc_covar_y[ii, ic, , ],
          log = TRUE
        )
    }
  }

  return(out)
}
