% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bmnr-prior.R
\name{bmnr_prior}
\alias{bmnr_prior}
\title{Prior Information}
\usage{
bmnr_prior(n_y, ..., with_gp = TRUE)
}
\arguments{
\item{n_y}{integer. Number of dependent variables.}

\item{...}{Overwrite default values for each prior.}

\item{with_gp}{logical. Setting \code{TRUE} supplied parameters for \code{gp_length}.}
}
\description{
Helper function to assist in defining prior information for BMNR models.
}
\section{Prior Information}{

It is assumed, \emph{a priori}, that
\itemize{
\item \eqn{B_{ij} \sim N(0, \tau_B)} where \eqn{\tau_B =} \code{regr_prec}.
\item \strong{TODO}. Complete prior section.
}
}

