#include <Rcpp.h>
using namespace Rcpp;

// headers
NumericVector diff_rcpp(NumericVector xs);
LogicalVector within_rcpp(NumericVector x, int a, int b);

// Function taken from: https://stat.ethz.ch/pipermail/r-help/2012-January/301873.html

// [[Rcpp::export]]
NumericVector diff_rcpp(NumericVector xs) {
    NumericVector x(xs);
    NumericVector y = diff(x);
    return y;
}


// Thanks: http://stackoverflow.com/questions/34519811/what-is-the-fastest-way-to-perform-multiple-logical-comparisons-in-r

// [[Rcpp::export]]
LogicalVector within_rcpp(NumericVector x, int a, int b) {
  R_xlen_t i = 0, n = x.size();
  LogicalVector result(n);

  for ( ; i < n; i++) {
    result[i] = (x[i] >= a && x[i] <= b);
  }
  return result;
}


