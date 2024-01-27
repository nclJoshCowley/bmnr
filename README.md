# Bayesian Matrix Normal Regression (`bmnr`)

The original scope of this R package was to fit a matrix-variate normal
    regression in Stan.
We found the model to be computationally infeasible for our time-frame when
    applied to 1500 observations but still used this package to fit a
    multivariate normal regression (with semi-conjugate prior information).

The code is therefore stuck in development and can be used at ones own risk.

If you have any questions about these models or you are interested in the
    future development of this package feel free to contact me at
    <josh.cowley@hotmail.com>.


## Installation

You can install the development version of `bmnr` with:

``` r
# install.packages("devtools")
devtools::install_github("nclJoshCowley/bmnr")
```
