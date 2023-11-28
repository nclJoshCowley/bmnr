/*
 * Matrix Normal Distribution Density
 *
 * @param y matrix. n_s rows and n_y columns. Matrix to be evaluated.
 * @param y_mean. Matrix of expectations, equal in dimension to y.
 * @param L_covar_s. Cholesky decomposition of square (n_s) covariance matrix across rows.
 * @param covar_y. Square (n_y) covariance matrix across columns.
 *
 * @return Log density of matrix normal distribution.
*/
real matrix_normal_halfcholesky_lpdf(matrix y, matrix y_mean, matrix L_covar_s, matrix covar_y) {
    real lp;
    matrix[rows(y), cols(y)] mahalanobis_dist;
    int n_s = rows(y);
    int n_y = cols(y);

    // Algebraically equivalent to
    //   inverse(tri(L_covar_s)) * (y - y_mean)
    mahalanobis_dist = mdivide_left_tri_low(L_covar_s, y - y_mean);

    lp =
      - (0.5 * n_s * n_y * log(2 * pi()))
      - (0.5 * n_s * log_determinant(covar_y))
      // Algebraically equivalent to
      //   - 0.5 * n_y * log_determinant(covar_s);
      - (n_y * sum(log(diagonal(L_covar_s))))
      // Algebraically equivalent to
      //   - 0.5 * trace(inverse_spd(covar_y), crossprod(mahalanobis_dist));
      - (0.5 * trace(mdivide_left_spd(covar_y, crossprod(mahalanobis_dist))));

    /*
    lp =
      - (0.5 * n_s * n_y * LOG_2_PI)
      - (0.5 * n_s * log_determinant(covar_y))
      - (0.5 * n_y * log_determinant(covar_s))
      - (0.5 * trace_gen_quad_form(inverse_spd(covar_y), inverse_spd(covar_s), y - y_mean));
    */

    return lp;
}
