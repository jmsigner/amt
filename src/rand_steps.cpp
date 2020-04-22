#include <math.h>
#include <Rcpp.h>
using namespace Rcpp;

/*
// [[Rcpp::export]]
NumericMatrix random_steps_cpp(
    const int n_rand_steps,
    const NumericVector start_x, const NumericVector start_y,
    const NumericVector end_x, const NumericVector end_y,
    const NumericVector rand_sl,
    const NumericVector rand_ta,
    const int include_obs,
    const NumericVector sl_obs, const NumericVector ta_obs) {

  int n = start_x.length();

  int nrow = n * n_rand_steps;
  if (include_obs == 1) {
    nrow = n * (n_rand_steps + 1);
  }
  NumericMatrix X(nrow, 8);

  int counter = 0;
  double slr, tar, abs_angle, new_x, new_y;

  for (int i = 0; i < n; i++) {
    for (int j = 0; j < n_rand_steps; j++) {

      slr = sample(rand_sl, 1, false)[0];
      tar = sample(rand_ta, 1, false)[0];

      abs_angle = atan2(end_y[i] - start_y[i], end_x[i] - start_x[i]);
      new_x = end_x[i] + slr * cos(abs_angle + tar);
      new_y = end_y[i] + slr * sin(abs_angle + tar);

      X(counter, 0) = end_x[i];
      X(counter, 1) = end_y[i];
      X(counter, 2) = new_x;
      X(counter, 3) = new_y;
      X(counter, 4) = slr;
      X(counter, 5) = tar;
      X(counter, 6) = 0;
      X(counter, 7) = i + 1;
      counter++;
    }
  }

  if (include_obs == 1) {
    for (int i = 0; i < n; i++) {
      X(counter, 0) = start_x[i];
      X(counter, 1) = start_y[i];
      X(counter, 2) = end_x[i];
      X(counter, 3) = end_y[i];
      X(counter, 4) = sl_obs[i];
      X(counter, 5) = ta_obs[i];
      X(counter, 6) = 1;
      X(counter, 7) = i + 1;
      counter++;
    }
  }

  colnames(X) = CharacterVector::create("x1_", "y1_", "x2_", "y2_", "sl_", "ta_", "case_", "step_id_");
  return X;
}

 */

// [[Rcpp::export]]
NumericMatrix random_steps_cpp_one_step(
    const int n_rand_steps,
    const double start_x,
    const double start_y,
    const double rel_angle,
    const NumericVector rand_sl,
    const NumericVector rand_ta ) {


  NumericMatrix X(n_rand_steps, 6);

  double slr, tar, new_x, new_y;

  for (int i = 0; i < n_rand_steps; i++) {

    slr = sample(rand_sl, 1, false)[0];
    tar = sample(rand_ta, 1, false)[0];

    new_x = start_x + slr * cos(rel_angle + tar);
    new_y = start_y + slr * sin(rel_angle + tar);

    X(i, 0) = start_x;
    X(i, 1) = start_y;
    X(i, 2) = new_x;
    X(i, 3) = new_y;
    X(i, 4) = slr;
    X(i, 5) = tar;
  }

  colnames(X) = CharacterVector::create("x1_", "y1_", "x2_", "y2_", "sl_", "ta_");
  return X;
}
