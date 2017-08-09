#include <Rcpp.h>
using namespace Rcpp;

//[[Rcpp::export]]
NumericVector rolling_mean(NumericVector x, int win) {
  int n = x.length();
  NumericVector out(n, NumericVector::get_na());
  IntegerVector s;
  NumericVector foo;
  for (int i = win; i < (n - win); i++) {
    s = seq(i - win, i + win);
    foo = x[s];
    out(i) = mean(foo);
  }
  return(out);
}

//[[Rcpp::export]]
NumericVector rolling_median(NumericVector x, int win) {
  int n = x.length();
  NumericVector out(n, NumericVector::get_na());
  IntegerVector s;
  NumericVector foo;
  for (int i = win; i < (n - win); i++) {
    s = seq(i - win, i + win);
    foo = x[s];
    out(i) = median(foo);
  }
  return(out);
}
