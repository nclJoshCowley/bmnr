functions {
  #include /functions/gp_matern32_cov_ard.stan
}
data {
  int<lower=1> n_s;
  int<lower=1> n_inputs;

  vector[n_inputs] coords[n_s];
  real gp_scale;
  vector[n_inputs] gp_length;
}
generated quantities {
  matrix[n_s, n_s] out = gp_matern32_cov_ard(coords, gp_scale, gp_length);
}
