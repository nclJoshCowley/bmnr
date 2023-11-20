#' Render Analysis Report for [`bmnr`]
#'
#' Render Quarto document and produce output files `.Rds` and `.html`.
#'
#' @inheritParams bmnr-package
#' @param outfile character. Name of output file(s); file extensions dropped.
#'
#' @return Silently return the input `object` for use with pipes.
#'
#' @export
render_bmnr <- function(object, outfile) {
  requireNamespace("quarto", quietly = TRUE)

  out_nm <- gsub("\\.(rds|html)$", "", basename(outfile), ignore.case = TRUE)

  # All rendering to be done in temporary directory to avoid clashes
  cur_wd <- getwd()
  on.exit(setwd(cur_wd))

  out_dir <- file.path(cur_wd, dirname(outfile))
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

  temp_dir <- tempfile("bmnr\\", tempdir(), fileext = "")
  dir.create(temp_dir, recursive = TRUE)
  setwd(temp_dir)

  # Require analysis QMD and object saved as RDS
  bmnr_analysis_qmd <-
    system.file(
      "reports", "bmnr-analysis.qmd", mustWork = TRUE, package = "bmnr"
    )

  file.copy(bmnr_analysis_qmd, basename(bmnr_analysis_qmd))
  saveRDS(object, sprintf("%s.rds", out_nm))

  quarto::quarto_render(
    input = basename(bmnr_analysis_qmd),
    output_file = sprintf("%s.html", out_nm),
    execute_params = list(
      object = structure(sprintf('readRDS("%s.rds")', out_nm), tag = "!expr")
    )
  )

  unlink(basename(bmnr_analysis_qmd))

  output_files <- sprintf("%s%s", out_nm, c(".rds", ".html"))
  file.rename(output_files, file.path(out_dir, output_files))

  invisible(object)
}


#' Functions used in Analysis Report
#'
#' Functions not to be used anywhere other than `bmnr-analysis.qmd`.
#'
#' @inheritParams render_bmnr
#' @param is_child logical. Some options are only set when this is `FALSE`.
#' @param fig.asp numeric. Default aspect ratio per **panel**, not per plot.
#'
#' @name bmnr-render-internal
#' @export
.set_bmnr_render_options <- function(object, is_child, fig.asp) {
  requireNamespace("ggplot2", quietly = TRUE)
  requireNamespace("patchwork", quietly = TRUE)
  requireNamespace("knitr", quietly = TRUE)

  stopifnot(inherits(object, "bmnrfit"))

  if (isFALSE(is_child)) {
    ggplot2::theme_set(ggplot2::theme_minimal(base_size = 11))

    knitr::opts_chunk$set(
      echo = FALSE, message = FALSE, fig.asp = fig.asp, out.width = "100%"
    )
  }

  # Child documents to inherit `fig.asp`
  fig.asp <- knitr::opts_chunk$get("fig.asp")

  ggplot2::theme_update(
    legend.position = "bottom",
    axis.text = ggplot2::element_text(size = ggplot2::rel(0.65)),
    plot.subtitle = ggplot2::element_text(size = ggplot2::rel(0.75))
  )

  nr_regr <- object@par_dims$regr[1]
  nr_covar_y <- object@par_dims$covar_y[1]

  get_fig_asp_possibly <- function(n_rows) {
    if (n_rows == 0) fig.asp else fig.asp * n_rows
  }

  knitr::opts_template$set(
    regr = list(fig.asp = get_fig_asp_possibly(nr_regr)),
    covar_y = list(fig.asp = get_fig_asp_possibly(nr_covar_y))
  )

  s3_register("knitr::knit_print", "list", bmnr::printer_tabset)

  invisible(NULL)
}


#' Print Tabsets (via [`knitr::knit_print`])
#'
#' Converts a list to a Quarto / RMD tabset using the names as tab headings.
#'
#' @param x list. Each element is passed to `knit_print` within tabs.
#' @param options,... unused arguments required for `knit_print`.
#'
#' @references
#'   <https://github.com/nclJoshCowley/jcutils/blob/master/R/knitr-printers.R>
#'
#' @export
printer_tabset <- function(x, options, ...) {
  if (is.null(names(x))) names(x) <- seq_along(x)

  header <- ":::: {.panel-tabset}"
  footer <- "::::"

  res <- lapply(seq_along(x), function(i) {
    knitr::knit_child(
      text = c(
        "##### `r names(x)[i]`",
        "",
        "```{r}",
        "#| echo: false",
        "x[[i]]",
        "```"
      ),
      options = options["fig.asp"],
      envir = environment(),
      quiet = TRUE
    )
  })

  knitr::asis_output(paste(c(header, res, footer), collapse = "\n\n"))
}


#' Extract Algorithm Controls from `stanfit`
#'
#' @param object Object that inherits from `stanfit`.
#'
#' @export
get_stan_controls <- function(object) {
  stopifnot(inherits(object, "stanfit"))

  seed <- toString(rstan::get_seed(object) %||% rstan::get_seeds(object))

  return(
    c(object@sim[c("warmup", "iter", "thin", "chains")], list(seed = seed))
  )
}
