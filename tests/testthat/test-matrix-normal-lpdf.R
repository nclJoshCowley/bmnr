test_that("Matrix normal (log) density consistent", {
  requireNamespace("rstan", quietly = TRUE)
  requireNamespace("matrixNormal", quietly = TRUE)

  # Make Stan function(s) available
  sm <- utils::getFromNamespace("stanmodels", "bmnr")
  rstan::expose_stan_functions(sm$bmnr)

  A = matrix(rnorm(20), 10, 2)
  M = matrix(0, 10, 2)
  U = 0.2 + diag(0.5, nrow = 10)
  L_U = t(chol(U))
  V = 0.8 + diag(0.2, nrow = 2)

  out_stan <- matrix_normal_halfcholesky_lpdf(A, M, L_U, V)
  out_r <- matrixNormal::dmatnorm(A, M, U, V, log = TRUE)

  testthat::expect_equal(out_stan, out_r)
})
