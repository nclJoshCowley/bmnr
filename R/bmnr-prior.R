#' Prior Information
#'
#' Helper function to assist in defining prior information for BMNR models.
#'
#' @param ... Overwrite default values for each prior.
#'
#' @inheritSection bmnr-package Prior Information
#'
#' @export
bmnr_prior <- function(...) {
  list(
    regr_prec = 0.1
  )
}
