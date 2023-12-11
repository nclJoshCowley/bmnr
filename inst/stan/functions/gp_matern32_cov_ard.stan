/*
 * Matern 3/2 Kernel with Characteristic Length Scales
 *
 * @param x array of vector. Distance between these two vectors are needed.
 * @param gp_scale real. Multiplicative amplitude of the kernel function.
 * @param gp_length vector. Multiple length scales, one for each dimension.
 *
 * @return Covariance matrix with 'size(x)' rows and columns
*/
matrix gp_matern32_cov_ard_general(vector[] x_r, vector[] x_c, real gp_scale, vector gp_length) {
    int n_r = size(x_r);
    int n_c = size(x_c);
    matrix[n_r, n_c] out;

    real gp_scale_sq = pow(gp_scale, 2);
    real dist;

    for (ir in 1:n_r) {
      for (ic in 1:n_c) {
        dist = sqrt(dot_self((x_r[ir] - x_c[ic]) ./ gp_length));
        out[ir, ic] =
          gp_scale_sq * (1 + sqrt(3) * dist) * exp(-1 * sqrt(3) * dist);
      }
    }

    return out;
}


/*
 * Matern 3/2 Kernel with Characteristic Length Scales
 *
 * @param x array of vector. Distance between these two vectors are needed.
 * @param gp_scale real. Multiplicative amplitude of the kernel function.
 * @param gp_length vector. Multiple length scales, one for each dimension.
 *
 * @return Covariance matrix with 'size(x)' rows and columns
*/
matrix gp_matern32_cov_ard(vector[] x, real gp_scale, vector gp_length) {
    int n_r = size(x);
    matrix[n_r, n_r] out;

    real gp_scale_sq = pow(gp_scale, 2);
    real dist;

    for (i in 1:(n_r - 1)) {
      out[i, i] = gp_scale_sq;

      for (j in (i + 1):n_r) {
        dist = sqrt(dot_self((x[i] - x[j]) ./ gp_length));
        out[i, j] = gp_scale_sq * (1 + sqrt(3) * dist) * exp(-1 * sqrt(3) * dist);
        out[j, i] = out[i, j];
      }
    }

    out[n_r, n_r] = gp_scale_sq;

    return out;
}
