#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
bool any_inf(NumericVector x) {
    int len = x.size();
    for(int i=0; i<len; i++){
        if (!(std::isfinite(x[i]))) return true;
    }
    return false;
}
