% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bmnr-extract-data-frame.R
\name{validate_and_split_pars}
\alias{validate_and_split_pars}
\title{Make \code{pars} Argument Explicit}
\usage{
validate_and_split_pars(object, pars)
}
\arguments{
\item{object}{Object that inherits from \code{\link[rstan:stanfit-class]{stanfit}}.}

\item{pars}{character. Parameter names, with optional subsetting.}
}
\description{
Extends and splits desired parameters based on bare parameter names.
}
\keyword{internal}
