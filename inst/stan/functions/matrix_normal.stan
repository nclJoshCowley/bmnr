/*
 * Matrix Normal Distribution Density
 *
 * @param y matrix. n_s rows and n_y columns. Matrix to be evaluated.
 * @param y_mean. Matrix of expectations, equal in dimension to y.
 * @param covar_s. Square (n_s) covariance matrix across rows.
 * @param covar_y. Square (n_y) covariance matrix across columns.
 *
 * @return Log density of matrix normal distribution.
*/
real matrix_normal_lpdf(matrix y, matrix y_mean, matrix covar_s, matrix covar_y) {
    real lp = 0;

    // TODO: Test if quicker than log(2 * pi());
    real LOG_2_PI = 1.837877066409345;

    int n_s = rows(y);
    int n_y = cols(y);

    lp -= 0.5 * n_s * n_y * LOG_2_PI;
    lp -= 0.5 * n_s * log_determinant(covar_y);
    lp -= 0.5 * n_y * log_determinant(covar_s);

    // May want to reparameterise in terms of precision matrices
    // (inverting covariance matrices directly isn't ideal)
    lp -= 0.5 * trace_gen_quad_form(
      inverse_spd(covar_y),
      inverse_spd(covar_s),
      y - y_mean
    );

    return lp;
  }
