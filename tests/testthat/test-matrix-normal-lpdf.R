test_that("Matrix normal (log) density consistent", {
  requireNamespace("rstan", quietly = TRUE)
  requireNamespace("matrixNormal", quietly = TRUE)

  # Make Stan function(s) available
  sm <- utils::getFromNamespace("stanmodels", "bmnr")
  rstan::expose_stan_functions(sm$bmnr)

  A = matrix(rnorm(20), 10, 2)
  M = matrix(0, 10, 2)
  U = diag(0.1, nrow = 10)
  V = diag(0.8, nrow = 2)

  out_stan <- matrix_normal_lpdf(A, M, U, V)
  out_r <- matrixNormal::dmatnorm(A, M, U, V)

  testthat::expect_equal(out_stan, out_r)
})
