data {
  // Dimensions
  int<lower=1> n_s;
  int<lower=1> n_y;
  int<lower=1> n_x;

  // Data
  matrix[n_s, n_y] y;
  matrix[n_s, n_x] x;

  // Prior Hyperparameters
  real<lower=0> regr_prec;
  real<lower=0> covar_y_df;
  cov_matrix[n_y] covar_y_scale;
}
parameters {
  matrix[n_x, n_y] regr;
  cov_matrix[n_y] covar_y;
}
model {
  matrix[n_y, n_y] L_covar_y = cholesky_decompose(covar_y);
  matrix[n_s, n_y] mean_y = x * regr;

  // Prior
  for (yi in 1:n_y) regr[, yi] ~ normal(0, 1 / sqrt(regr_prec));
  covar_y ~ inv_wishart(covar_y_df, covar_y_scale);

  // Likelihood
  for (ii in 1:n_s) y[ii, ] ~ multi_normal_cholesky(mean_y[ii, ], L_covar_y);
}
