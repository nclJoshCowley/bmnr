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
