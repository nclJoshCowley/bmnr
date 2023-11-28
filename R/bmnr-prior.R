#' Prior Information
#'
#' Helper function to assist in defining prior information for BMNR models.
#'
#' @param n_y integer. Number of dependent variables.
#' @param ... Overwrite default values for each prior.
#' @param with_gp logical. Setting `TRUE` supplied parameters for `gp_length`.
#'
#' @inheritSection bmnr-package Prior Information
#'
#' @export
bmnr_prior <- function(n_y, ..., with_gp = TRUE) {
  linreg_prior <-
    list(regr_prec = 0.1, covar_y_df = n_y, covar_y_scale = diag(nrow = n_y))

  gp_prior <-
    list(gp_length_shape = 2, gp_length_rate = 10)

  default_prior <- c(linreg_prior, if (with_gp) gp_prior)

  out <- utils::modifyList(default_prior, rlang::list2(...))

  return(out)
}
