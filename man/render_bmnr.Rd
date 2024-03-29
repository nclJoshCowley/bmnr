% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/render.R
\name{render_bmnr}
\alias{render_bmnr}
\title{Render Analysis Report for \code{\link{bmnr}}}
\usage{
render_bmnr(object, outfile)
}
\arguments{
\item{object}{Object that inherits from \code{\link{bmnrfit}}.}

\item{outfile}{character. Name of output file(s); file extensions dropped.}
}
\value{
Silently return the input \code{object} for use with pipes.
}
\description{
Render Quarto document and produce output files \code{.Rds} and \code{.html}.
}
