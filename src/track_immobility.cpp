#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericVector track_immobility(NumericVector t, NumericVector x, NumericVector y, double period, double tol) {

  int n = x.length();
  NumericVector out(n);
  for (int i = 0; i < n; i++) out(i) = -1;
  double immobile_period = 0;
  int immo_started;

  int i = 0;

  while (i < n && immobile_period < period) {
    double dist = sqrt(pow(x[i] - x[i + 1], 2) + pow(y[i] - y[i + 1], 2));
    if (dist < tol) {
      immobile_period += t[i+1] - t[i];
    } else {
      immobile_period = 0;
      immo_started = i + 1;
    }
    out[i] = 1;
    i++;
  }

  for (int k = immo_started; k < i; k++)
    out[k] = -1;

  return out;
}



