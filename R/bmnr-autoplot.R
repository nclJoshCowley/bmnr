#' Plot Functionality for `bmnr`
#'
#' Methods for the [`ggplot2::autoplot`] generic.
#'
#' @inheritParams bmnr-package
#' @param pars character. Parameter names, with optional subsetting.
#' @param type choice. Either `density`, `trace` or `acf`.
#' @inheritParams rlang::args_dots_empty
#'
#' @name bmnr-plot
NULL


#' @rdname bmnr-plot
#' @export
autoplot.bmnrfit <- function(object, pars, type, ...) {
  if (is.character(type) && length(type) > 1) {
    names(type) <- gsub("^Acf$", "ACF", tools::toTitleCase(type))
    return(lapply(type, \(.x) autoplot(object, pars, .x)))
  }

  layer_to_add <-
    bmnr_layer(type, .truth = inherits(object, "bmnrfit_simstudy"))

  base_plot_objects <-
    plot_bmnr_empty(object, pars) |>
    purrr::imap(function(.x, .nm) {
      .x + ggplot2::labs(subtitle = .nm) # get_interpretable_names(object, .nm))
    })

  return(lapply(base_plot_objects, `+`, layer_to_add))
}


#' @keywords internal
#' @inheritParams bmnr-plot
plot_bmnr_empty <- function(object, pars) {
  plot_data <- extract_to_data_frame(object, pars)

  plot_data_per_term <-
    split(plot_data, factor(plot_data$.term, levels = unique(plot_data$.term)))

  return(lapply(plot_data_per_term, ggplot2::ggplot))
}



#' Get `ggplot2` Layer(s) for Plotting MCMC Output.
#'
#' @param type choice. Name of the supported plot type including
#' * `"none"`, plots are to be empty until the user adds layers.
#' * `"density"`, useful distribution visualisation.
#' * `"trace"`, useful for verifying chain convergence.
#' * `"acf"`, autocorrelation of the MCMC samples at discrete lags.
#' Alternatively one can pass a **list** of custom layers to be added.
#'
#' @name bmnr_layer
NULL


#' @rdname bmnr_layer
#' @export
bmnr_layer <- function(type, .truth = FALSE) {
  if (is.list(type) || inherits(type, "Layer")) return(type)

  switch(
    match.arg(type, c("none", "density", "trace", "acf")),
    none = list(),
    density = layer_density(.truth),
    trace = layer_trace(.truth),
    acf = layer_acf()
  )
}


#' @rdname bmnr_layer
#' @keywords internal
layer_density <- function(.truth = FALSE) {
  cur_labs <- ggplot2::labs(y = NULL, x = NULL, fill = "Chain")

  cur_geom_density <-
    ggplot2::geom_density(
      ggplot2::aes(
        x = .data$.value,
        fill = factor(.data$.chain)
      ),
      adjust = 1.5,
      alpha = 0.1,
    )

  cur_geom_vline <-
    ggplot2::geom_vline(
      ggplot2::aes(xintercept = .data$.truth),
      linetype = "dashed",
      alpha = 0.55,
      na.rm = TRUE
    )

  out <- list(cur_labs, cur_geom_density)
  return(if (.truth) append(out, cur_geom_vline) else out)
}


#' @rdname bmnr_layer
#' @keywords internal
layer_trace <- function(.truth = FALSE) {
  cur_scale <- scale_x_iterations()

  cur_labs <- ggplot2::labs(x = "Iteration", y = "Value", colour = "Chain")

  cur_geom_line <-
    ggplot2::geom_line(
      ggplot2::aes(
        y = .data$.value,
        x = .data$.iter,
        colour = factor(.data$.chain)
      ),
      alpha = 0.55
    )

  cur_geom_hline <-
    ggplot2::geom_hline(
      ggplot2::aes(yintercept = .data$.truth),
      linetype = "dashed",
      alpha = 0.55,
      na.rm = TRUE
    )

  out <- list(cur_scale, cur_labs, cur_geom_line)
  return(if (.truth) append(out, cur_geom_hline) else out)
}


#' @rdname bmnr_layer
#' @keywords internal
layer_acf <- function() {
  cur_labs <- ggplot2::labs(y = "ACF", x = NULL, fill = "Chain")

  cur_geom_bar <-
    ggplot2::geom_bar(
      ggplot2::aes(y = .data$acf, x = .data$lag, fill = factor(.data$.chain)),
      data = prepare_acf_data,
      stat = "identity",
      position = "dodge",
      width = 0.2,
      na.rm = TRUE
    )

  out <- list(cur_labs, cur_geom_bar)
  return(out)
}


#' @keywords internal
#' @param data data frame. MCMC output, requires `.chain` and `.term` columns.
#' @noRd
prepare_acf_data <- function(data) {
  data |>
    dplyr::group_by(.data$.term, .data$.chain) |>
    dplyr::reframe({
      .acf <- stats::acf(.data$.value, lag.max = NULL, plot = FALSE)
      as.data.frame(unclass(.acf)[c("acf", "lag")])
    })
}


#' Position Scale for MCMC Iterations
#'
#' Wrapper around [`scale_x_continuous`][ggplot2::scale_x_continuous()] with
#'   updated defaults for thousands of MCMC iterations.
#'
#' @param ... passed to `scale_x_continuous`
#' @param n integer. Number of desirable breaks.
#'
#' @keywords internal
scale_x_iterations <- function(..., n = 5) {
  defaults <-
    list(
      name = "Iterations",
      breaks = function(x) {
        br <- pretty(x, n = n)
        br[1] <- 0
        all_br_gt_1e3 <- all(log10(subset(br, br != 0)) >= 3)

        # if (all_br_gt_1e3) br[length(br)] <- 1e3 * floor(x[2] / 1e3)
        # if (!all_br_gt_1e3) br[length(br)] <- 1e2 * floor(x[2] / 1e2)

        return(unique(sort(br)))
      },
      labels = function(br) {
        all_br_gt_1e3 <- all(log10(subset(br, br != 0)) >= 3)
        sprintf(
          if (all_br_gt_1e3) "%.0fk" else "%.1fk",
          br / 1e3
        )
      }
    )

  args <- utils::modifyList(defaults, rlang::list2(...))

  return(do.call(ggplot2::scale_x_continuous, args))
}
