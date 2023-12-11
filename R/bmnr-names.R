#' Get Names from [`bmnr`]
#'
#' @inheritParams bmnr-package
#'
#' @returns List with character vector elements representing original names.
#' @export
get_names_from_bmnrfit <- function(object) {
  lhs <- object@formula$model[[2]]
  if (lhs[[1]] == quote(cbind)) lhs <- lhs[-1]

  y_nms <- vapply(lhs, deparse, character(1))

  x_nms <-
    colnames(
      stats::model.matrix(object@formula$model, data = object@data[1, ])
    )

  coord_nms <-
    if (inherits(object, "bmnrfit_mvrnorm")) NULL else {
      colnames(
        stats::model.matrix(object@formula$coord, data = object@data[1, ])
      )
    }

  return(list(y = y_nms, x = x_nms, coord = coord_nms))
}
