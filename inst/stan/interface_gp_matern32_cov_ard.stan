functions {
  #include /functions/gp_matern32_cov_ard.stan
}
data {
  int<lower=1> n_inputs;
  int<lower=1> n_r;
  int<lower=0> n_c;

  vector[n_inputs] x_r[n_r];
  vector[n_inputs] x_c[n_c];

  real gp_scale;
  vector[n_inputs] gp_length;
}
generated quantities {
  // NCOL = n_c (when non-zero) and n_r otherwise
  matrix[n_r, n_c ? n_c : n_r] out;

  if (n_c == 0) {
    out = gp_matern32_cov_ard(x_r, gp_scale, gp_length);
  } else {
    out = gp_matern32_cov_ard_general(x_r, x_c, gp_scale, gp_length);
  }
}
