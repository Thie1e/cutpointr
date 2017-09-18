#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
LogicalVector one_unique_num(NumericVector x) {
  int n = x.size();
  double first_val = x[0];
  for(int i = 1; i < n; ++i) {
        if (x[i] != first_val) return false;
  }
  return true;
}

// [[Rcpp::export]]
LogicalVector one_unique_char(CharacterVector x) {
  int n = x.size();
  String first_val = x[0];
  for(int i = 1; i < n; ++i) {
        if (x[i] != first_val) return false;
  }
  return true;
}