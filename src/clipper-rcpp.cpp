#include "clipper-rcpp.h"

const long long mult = 1e12;

//' rcpp_clipper
//'
//' upper_layer are the convex hulls of the moveability points;
//' lower_layer are the sf polygons of green spaces
//' @noRd
// [[Rcpp::export]]
Rcpp::DataFrame rcpp_clipper (
        const Rcpp::List upper_layer,
        const Rcpp::List lower_layer)
{
    const int ll_size = lower_layer.size ();
    ClipperLib::Paths clip (ll_size);
    for (int i = 0; i < ll_size; i++)
    {
        Rcpp::DataFrame ll = Rcpp::as <Rcpp::DataFrame> (lower_layer [i]);
        Rcpp::NumericVector lx = ll ["lon"], ly = ll ["lat"];
        for (size_t j = 0; j < lx.size (); j++)
            clip [i] << ClipperLib::IntPoint (round (lx [j] * mult),
                    round (ly [j] * mult));
    }

    Rcpp::NumericVector green_areas (upper_layer.size (), 0.0),
        hull_areas (upper_layer.size (), 0.0);
    for (int i = 0; i < upper_layer.size (); i++)
    {
        Rcpp::DataFrame ul = Rcpp::as <Rcpp::DataFrame> (upper_layer [i]);
        Rcpp::NumericVector ux = ul ["x"], uy = ul ["y"];

        ClipperLib::Paths subj (1), solution;
        for (size_t j = 0; j < ux.size (); j++)
            subj [0] << ClipperLib::IntPoint (round (ux [j] * mult),
                    round (uy [j] * mult));

        // Signs of areas depend on polygon direction:
        // http://www.angusj.com/delphi/clipper/documentation/Docs/Units/ClipperLib/Functions/Area.htm
        hull_areas [i] = fabs (ClipperLib::Area (subj [0])) /
            static_cast <double> (mult);

        ClipperLib::Clipper c;
        c.AddPaths (subj, ClipperLib::ptSubject, true);
        c.AddPaths (clip, ClipperLib::ptClip, true);
        c.Execute (ClipperLib::ctIntersection, solution,
                ClipperLib::pftNonZero, ClipperLib::pftNonZero);

        for (auto s: solution)
            green_areas [i] += ClipperLib::Area (s);
        green_areas [i] = green_areas [i] / static_cast <double> (mult);
    }

    Rcpp::DataFrame res = Rcpp::DataFrame::create (
            Rcpp::Named ("hull_area") = hull_areas,
            Rcpp::Named ("green_area") = green_areas,
            Rcpp::_["stringsAsFactors"] = false);
    return res;
}
