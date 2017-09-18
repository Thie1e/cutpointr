#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
LogicalVector is_equal_cpp_char(CharacterVector x, String y){
    Rcpp::LogicalVector r(x.size());
    for(int i=0; i<x.size(); i++){
        r[i] = (x[i] == y);
    }
    return(r);
}

// [[Rcpp::export]]
LogicalVector is_equal_cpp_num(NumericVector x, double y){
    Rcpp::LogicalVector r(x.size());
    for(int i=0; i<x.size(); i++){
        r[i] = (x[i] == y);
    }
    return(r);
}
