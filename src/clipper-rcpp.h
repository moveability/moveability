#pragma once

#include <Rcpp.h>
// [[Rcpp::depends(RcppParallel)]]
#include <RcppParallel.h>

#include "clipper.h"

Rcpp::DataFrame rcpp_clipper (
        const Rcpp::List upper_layer,
        const Rcpp::List lower_layer);
