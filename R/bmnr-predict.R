#' Prediction of `bmnr` Models
#'
#' @inheritParams bmnr-package
#' @param ... Extra arguments passed to [`mvtnorm::rmvnorm()`].
#' @param type choice. Return type, see **Value** section for more information
#'
#' @section Posterior Predictive Distribution:
#' TODO. Convert from handwritten notes to Markdown
#'
#' @returns
#' The helper `extract_posterior_predictive_pars()` returns a list of the
#'   distribution parameters in the form of a `$flattened_mean` and `$covar`.
#' Output of the `predict` method is dictated by the `type` argument:
#'
#' * `summary`
#'
#'     Tibble representing posterior predictive summaries;
#'     one row per supplied observation from `new_data`.
#'
#' * `numeric`
#'
#'     Tibble with raw MCMC objects stored in a single named list column;
#'     one row per supplied observation from `new_data`.
#'
#' * `raw`
#'
#'     MCMC array of dimension `c(n_iter, n_chains, nrow(new_data), n_y)`.
#'
#' @name bmnr-predict
NULL


#' @rdname bmnr-predict
#' @export
predict.bmnrfit <- function(object, new_data, ..., type = NULL) {
  type <- match.arg(type, choices = c("summary", "numeric", "raw"))
  if (is.null(new_data)) new_data <- object@data

  n_iters <- object@sim$iter - object@sim$warmup
  n_chains <- object@sim$chains
  n_y <- object@par_dims$covar_y[1]

  pars <- extract_posterior_predictive_pars_bmnrfit(object, new_data)

  out <- array(NA, dim = c(n_iters, n_chains, nrow(new_data), n_y))

  message("Drawing MCMC samples from posterior predictive distribution")
  .pb <- knitrProgressBar::progress_estimated(n_iters * n_chains)

  for (ii in seq_len(n_iters)) {
    for (ic in seq_len(n_chains)) {
      knitrProgressBar::update_progress(.pb)

      out[ii, ic, , ] <-
        mvtnorm::rmvnorm(
          n = 1,
          mean = pars$flattened_mean[ii, ic, ],
          sigma = pars$covar[ii, ic, , ],
          ...
        )
    }
  }

  y_nms <- get_names_from_bmnrfit(object)$y
  out <- convert_raw_predictions_to_output(out, type, .names = y_nms)

  return(out)
}


#' @rdname bmnr-predict
#' @export
predict.bmnrfit_mvrnorm <- function(object, new_data, ..., type = NULL) {
  type <- match.arg(type, choices = c("summary", "numeric", "raw"))
  if (is.null(new_data)) new_data <- object@data

  n_iters <- object@sim$iter - object@sim$warmup
  n_chains <- object@sim$chains
  n_y <- object@par_dims$covar_y[1]

  lp_obs <- extract_linpred(object, new_data)
  covar_y <- extract_covar_y(object)

  out <- array(NA, dim = c(n_iters, n_chains, nrow(new_data), n_y))

  message("Drawing MCMC samples from posterior predictive distribution")
  .pb <- knitrProgressBar::progress_estimated(n_iters * n_chains)

  for (ii in seq_len(n_iters)) {
    for (ic in seq_len(n_chains)) {
      knitrProgressBar::update_progress(.pb)

      for (i in seq_len(nrow(new_data))) {
        out[ii, ic, i, ] <-
          mvtnorm::rmvnorm(
            n = 1,
            mean = lp_obs[ii, ic, i, ],
            sigma = covar_y[ii, ic, , ],
            ...
          )
      }
    }
  }

  y_nms <- get_names_from_bmnrfit(object)$y
  out <- convert_raw_predictions_to_output(out, type, .names = y_nms)

  return(out)
}


#' @rdname bmnr-predict
#' @keywords internal
extract_posterior_predictive_pars_bmnrfit <- function(object, new_data) {
  if (is.null(new_data)) new_data <- object@data
  obs_data <- object@data

  n_iters <- object@sim$iter - object@sim$warmup
  n_chains <- object@sim$chains

  n_y <- object@par_dims$covar_y[1]
  n_new <- n_y * nrow(new_data)

  y_obs <-
    stats::model.frame(object@formula$model, data = obs_data) |>
    stats::model.response()

  joint_coords <-
    rbind(
      stats::model.matrix(
        stats::delete.response(stats::terms(object@formula$coord)),
        data = obs_data
      ),
      stats::model.matrix(
        stats::delete.response(stats::terms(object@formula$coord)),
        data = new_data
      )
    )

  # Used to select correct partitions of `joint_covar_s`
  i_obs <- seq_len(nrow(obs_data))
  i_new <- nrow(obs_data) + seq_len(nrow(new_data))
  n_all <- nrow(joint_coords)

  lp_obs <- extract_linpred(object = object, new_data = obs_data)
  lp_new <- extract_linpred(object = object, new_data = new_data)
  covar_y <- extract_covar_y(object)
  gp_length <- extract_gp_length(object)

  out <-
    list(
      flattened_mean = array(NA, dim = c(n_iters, n_chains, n_new)),
      covar = array(NA, dim = c(n_iters, n_chains, n_new, n_new))
    )

  message("Deriving posterior predictive parameters, this may take a while")
  .pb <- knitrProgressBar::progress_estimated(n_iters * n_chains)

  for (ii in seq_len(n_iters)) {
    for (ic in seq_len(n_chains)) {
      knitrProgressBar::update_progress(.pb)

      joint_covar_s <-
        gp_matern32_cov_ard(
          joint_coords, gp_scale = 1, gp_length = gp_length[ii, ic, ]
        ) +
        diag(1e-8, nrow = n_all)

      # Interim calculations use block conditioning of multivariate normal
      joint_covar_11 <-
        covar_y[ii, ic, , ] %x% joint_covar_s[i_obs, i_obs]

      joint_covar_21 <-
        covar_y[ii, ic, , ] %x% joint_covar_s[i_new, i_obs]

      joint_covar_22 <-
        covar_y[ii, ic, , ] %x% joint_covar_s[i_new, i_new]

      shared_term <-
        joint_covar_21 %*% chol2inv(chol(joint_covar_11))

      out$flattened_mean[ii, ic, ] <-
        c(lp_new[ii, ic, , ]) +
        (shared_term %*% (c(y_obs) - c(lp_obs[ii, ic, , ])))

      out$covar[ii, ic, , ] <-
        joint_covar_22 - (shared_term %*% t(joint_covar_21))
    }
  }

  return(out)
}


#' @rdname bmnr-predict
#'
#' @param x array. Raw MCMC output as described in **Value** section.
#' @param .names character. Names each dependent variable to be predicted.
#'
#' @keywords internal
convert_raw_predictions_to_output <- function(x, type, .names = NULL) {
  type <- match.arg(type, choices = c("summary", "numeric", "raw"))

  if (type == "raw") return(x)

  if (is.null(.names)) .names <- sprintf("y_pred%02i", seq_len(dim(x)[4]))

  draws_list <-
    asplit(x, 4) |>
    rlang::set_names(nm = .names) |>
    purrr::imap(function(.y, .nm) {
      tibble::as_tibble(expand.grid(
        .iter = seq_len(dim(.y)[1]),
        .chain = seq_len(dim(.y)[2]),
        .index = seq_len(dim(.y)[3])
      )) |>
        dplyr::mutate(.value = c(.y)) |>
        tidyr::nest(".pred_{.nm}" := -".index")
    })

  draws <-
    draws_list |>
    purrr::reduce(function(.x, .y) dplyr::left_join(.x, .y, by = ".index"))

  if (type == "numeric") return(draws)

  # type == "summary"
  return(
    draws |>
      dplyr::mutate(
        dplyr::across(
          .cols = dplyr::starts_with(".pred"),
          .fns = list(
            mean = function(.draws_list) {
              purrr::map_dbl(
                .draws_list, \(.x) mean(.x$.value)
              )
            },
            lower = function(.draws_list) {
              purrr::map_dbl(
                .draws_list, \(.x) stats::quantile(.x$.value, probs = 0.025)
              )
            },
            upper = function(.draws_list) {
              purrr::map_dbl(
                .draws_list, \(.x) stats::quantile(.x$.value, probs = 0.975)
              )
            }
          )
        ),
        .keep = "unused"
      ) |>
      dplyr::rename_with(function(.nm) gsub("_mean$", "", .nm))
  )
}
