#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericVector mk_reg_steps(NumericVector t1, int time_dist, int time_tol, int start) {
  // Create variables
  int k = 1;  // burst
  int i = start - 1;  // counter of points
  int n = t1.size(); // number of points
  NumericVector out(n);   // output
  int t_max, t_min;

  // all positions that are left out in the beginning are 0ed
  for (int j = 0; j < i; j++)
    out[j] = 0;

  out[i] = k;

  while(i < n) {

    t_min = time_dist - time_tol;
    t_max = time_dist + time_tol;
    double waited = t1[i];

    // bring j to the right position
    int j = i + 1;
    while((j < n) && (waited < t_min)) {
      out[j] = 0;
      waited += t1[j];
      j++;
    }

    i = j;

    if (waited >= t_min && waited <= t_max) {
      out[j] = k;
    } else if (j == (n - 1)) {
      out[j] = 0;
    } else {
      k++;
      out[j] = k;
    }
  }
  return out;
}

// [[Rcpp::export]]
NumericVector mk_reg(NumericVector relocs_time, int time_dist, int time_tol, int start) {
  // Create variables
  int k = 1;  // burst
  int i = start - 1;  // counter of points
  int n = relocs_time.size(); // number of points
  NumericVector out(n);   // output
  int t_max, t_min;

  // all positions that are left out in the beginning are 0ed
  for (int j = 0; j < i; j++)
    out[j] = 0;

  out[i] = k;


  while(i < (n - 1)) {

    t_min = relocs_time[i] + time_dist - time_tol;
    t_max = relocs_time[i] + time_dist + time_tol;

    // bring j to the right position
    int j = i + 1;
    while((j < (n-1)) && (relocs_time[j] < t_min)) {
      out[j] = 0;
      j++;
    }

    i = j;

    if (relocs_time[j] >= t_min && relocs_time[j] <= t_max) {
      out[j] = k;
    } else if (j == (n - 1)) {
      out[j] = 0;
    } else {
      k++;
      out[j] = k;
    }
  }
  return out;
}


// [[Rcpp::export]]
NumericVector duration_acuracy(NumericVector x_, NumericVector y_, NumericVector t_,
                               NumericVector dop, NumericVector dim, int duration_accuracy) {

  int i, j, k,
    n = x_.size();

  NumericVector out(n);

  for (i = 1; i < n; i++) {
    // forward
    // bring j into the right position
    for (j = i; (j < n) && std::abs(t_[j] - t_[i]) <= duration_accuracy; j++) ;

    int better = 0; // this indicates a better point
    int fix_sl = pow(x_[i] - x_[i-1], 2) + pow(y_[i] - y_[i - 1], 2);


    if (j > i) {
      for (k = i + 1; k < j; k++) {
        if (dop[i] < dop[k]) {
          better = 1;
          break;
        } else if (dim[k] > dim[i]) {
          better = 1;
          break;
        } /*else if (pow(x_[k] - x_[i-1], 2) + pow(y_[k] - y_[i - 1], 2) < fix_sl) {
          better = 1;
          break;
        } */
      }
    }

    if (better == 0) {
      // backward
      // bring j into the right position
      for (j = i; (j >= 0) && std::abs(t_[i] - t_[j]) <= duration_accuracy; j--) ;
      if (j < i) {
        for (k = i - 1; k > j; k--) {
          if (dop[i] < dop[k]) {
            better = 1;
            break;
          } else if (dim[k] > dim[i]) {
            better = 1;
            break;
          }
          /*else if (pow(x_[k] - x_[i-1], 2) + pow(y_[k] - y_[i - 1], 2) < fix_sl) {
            better =k;
            break;
          }
           */
        }
      }
    }
    out[i] = better;
  }
  return out;
}
