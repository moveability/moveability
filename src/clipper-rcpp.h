#pragma once

#include <Rcpp.h>
// [[Rcpp::depends(RcppParallel)]]
#include <RcppParallel.h>

#include "clipper.h"

Rcpp::DataFrame rcpp_clipper (
        const Rcpp::List upper_layer,
        const Rcpp::List lower_layer);

Rcpp::IntegerVector rcpp_activity_points (
        const Rcpp::List layer,
        const Rcpp::DataFrame points);

Rcpp::NumericVector rcpp_areas (
        const Rcpp::List layer);

double rcpp_path_in_poly (
        const Rcpp::List layer,
        const Rcpp::DataFrame path);
