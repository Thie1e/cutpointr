#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
IntegerVector which_are_num(NumericVector x, double a) {
    int nx = x.size();
    std::vector<int> y;
    y.reserve(nx);

    for(int i = 0; i < nx; i++) {
        if (x[i] == a) y.push_back(i+1);
    }

    return wrap(y);
}

// [[Rcpp::export]]
IntegerVector which_are_char(CharacterVector x, String a) {
    int nx = x.size();
    std::vector<int> y;
    y.reserve(nx);

    for(int i = 0; i < nx; i++) {
        if (x[i] == a) y.push_back(i+1);
    }

    return wrap(y);
}