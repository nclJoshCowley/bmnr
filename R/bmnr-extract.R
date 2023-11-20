#' Extract Samples to Data Frame
#'
#' Wrapper for [`rstan::extract()`]; converts array of samples to tidy format.
#'
#' @param object Object that inherits from [`stanfit`][rstan::stanfit-class].
#' @param pars character. Parameter names, with optional subsetting.
#' @inheritParams rlang::args_dots_empty
#'
#' @note Applying this method to an object of class [`bmnrfit_simstudy`] adds
#'   an extra `.truth` column as appropriate.
#'
#' @name extract_to_data_frame
NULL


#' @rdname extract_to_data_frame
#' @export
setGeneric("extract_to_data_frame", function(object, pars, ...) {
  standardGeneric("extract_to_data_frame")
})


#' @rdname extract_to_data_frame
#' @export
setMethod("extract_to_data_frame", "stanfit", function(object, pars, ...) {
  rlang::check_dots_empty()

  mcmcarray_to_data_frame(rstan::extract(object, pars, permuted = FALSE))
})


#' @rdname extract_to_data_frame
#' @export
setMethod("extract_to_data_frame", "bmnrfit_simstudy", function(object, pars, ...) {
  rlang::check_dots_empty()

  out_draws <- methods::callNextMethod()
  out_truth <- extract_simstudy_true_values(object, pars)

  dplyr::left_join(out_draws, out_truth, by = ".term")
})


#' Get True Value from a Simulation Study
#'
#' @param object Object that inherits from [`bmnrfit_simstudy`].
#' @inheritParams extract_to_data_frame
#'
#' @keywords internal
extract_simstudy_true_values <- function(object, pars) {
  stopifnot(inherits(object, "bmnrfit_simstudy"))

  pars_list <- validate_and_split_pars(object, pars)

  out_list <-
    Map(x = pars_list, nm = names(pars_list), function(x, nm) {
      all_true_values <-
        tibble::tibble(
          .term = validate_and_split_pars(object, nm)[[nm]],
          .truth = c(object@params[[nm]]) %||% NA_real_
        )

      return(dplyr::filter(all_true_values, .data$.term %in% x))
    })

  problematic_out <-
    names(Filter(Negate(function(.x) ncol(.x) == 2 && nrow(.x) > 0), out_list))

  if (length(problematic_out)) {
    stop("Invalid truth recovery for: ", toString(problematic_out))
  }

  return(dplyr::bind_rows(out_list))
}


#' Make `pars` Argument Explicit
#'
#' Extends and splits desired parameters based on bare parameter names.
#'
#' @inheritParams extract_to_data_frame
#'
#' @keywords internal
validate_and_split_pars <- function(object, pars) {
  extended_pars <-
    dimnames(rstan::extract(object, pars, permuted = FALSE))[["parameters"]]

  split(extended_pars, gsub("\\[[0-9,]+\\]$", "", extended_pars))
}


#' Convert Stan's MCMC output to Data Frame
#'
#' @param mcmcarray array. Dimensions should be iterations, chains and label.
#'
#' @keywords internal
mcmcarray_to_data_frame <- function(mcmcarray) {
  par_nms <- dimnames(mcmcarray)[[3]] %||% seq_len(dim(mcmcarray)[3])

  out <-
    tibble::as_tibble(
      expand.grid(
        .iter = seq_len(nrow(mcmcarray)),
        .chain = seq_len(ncol(mcmcarray)),
        .term = factor(par_nms, levels = par_nms),
        KEEP.OUT.ATTRS = FALSE,
        stringsAsFactors = FALSE
      )
    )

  out$.value <- c(mcmcarray)

  return(out)
}
