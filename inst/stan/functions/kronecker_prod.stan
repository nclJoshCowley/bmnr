/*
 * Logic by https://jrnold.github.io/ssmodels-in-stan/stan-functions.html
*/
matrix kronecker_prod(matrix a, matrix b) {
  matrix[rows(a) * rows(b), cols(a) * cols(b)] out;
  int rows_b = rows(b);
  int cols_b = cols(b);
  for (i in 1:rows(a)) {
    for (j in 1:cols(a)) {
      int row_start = (i - 1) * rows_b + 1;
      int row_end = (i - 1) * rows_b + rows_b;
      int col_start = (j - 1) * cols_b + 1;
      int col_end = (j - 1) * cols_b + cols_b;
      out[row_start:row_end, col_start:col_end] = a[i, j] * b;
    }
  }
  return out;
}
