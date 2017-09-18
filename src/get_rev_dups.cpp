#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
LogicalVector get_rev_dups(NumericVector x) {
  return rev(duplicated(rev(x)));
}
