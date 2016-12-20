#include <Rcpp.h>
using namespace Rcpp;

// Function taken from: https://stat.ethz.ch/pipermail/r-help/2012-January/301873.html

// [[Rcpp::export]]
NumericVector diff_rcpp(NumericVector xs) {
    NumericVector x(xs);
    NumericVector y = diff(x);
    return y;
}

