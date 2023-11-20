functions {
  #include /functions/matrix_normal_halfcholesky.stan
  #include /functions/gp_matern32_cov_ard.stan
}
data {
  // Dimensions
  int<lower=1> n_s;
  int<lower=1> n_y;
  int<lower=1> n_x;
  int<lower=1> n_gp_dims;

  // Data
  matrix[n_s, n_y] y;
  matrix[n_s, n_x] x;
  vector[n_gp_dims] coords[n_s];

  // Prior Hyperparameters
  /*
  real<lower=0> regr_prec;
  cov_matrix[n_y] covar_y_scale;
  real<lower=0> covar_y_df;

  // GP Prior Hyperparameters
  real<lower=0> gp_length_shape;
  real<lower=0> gp_length_rate;
  */
}
transformed data {
  // TODO. To be supplied by user in future
  real regr_prec = 0.1;
  real covar_y_df = n_y;
  cov_matrix[n_y] covar_y_scale = diag_matrix(rep_vector(1, n_y));

  // TODO. To be supplied by user in future
  real<lower=0> gp_length_shape = 2;
  real<lower=0> gp_length_rate = 10;

  // GP Parameters (Assumed known)
  real<lower=0> gp_scale_prec = 1;
  real<lower=0> gp_nugget = 1e-8;
}
parameters {
  matrix[n_x, n_y] regr;
  cov_matrix[n_y] covar_y;
  real<lower=0> gp_scale;
  vector[n_gp_dims] gp_length;
}
model {
  matrix[n_s, n_s] covar_s;
  matrix[n_s, n_s] L_covar_s;

  // GP
  covar_s =
    gp_matern32_cov_ard(coords, gp_scale, gp_length) +
    diag_matrix(rep_vector(gp_nugget, n_s));

  L_covar_s = cholesky_decompose(covar_s);

  // Prior
  for (yi in 1:n_y) regr[, yi] ~ normal(0, regr_prec);
  covar_y ~ inv_wishart(covar_y_df, covar_y_scale);
  gp_length ~ gamma(gp_length_shape, gp_length_rate);
  gp_scale ~ normal(0, 1 / sqrt(gp_scale_prec));

  // Likelihood
  y ~ matrix_normal_halfcholesky(x * regr, L_covar_s, covar_y);
}
