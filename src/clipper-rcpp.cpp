#include "clipper-rcpp.h"

// clipper only works with integers, so double values have to be multiplied by
// this amount before converting to int:
const long long mult = 1e6;

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

//' rcpp_activity_points
//'
//' layer The convex hulls of the moveability polygons
//' @noRd
// [[Rcpp::export]]
Rcpp::IntegerVector rcpp_activity_points (
        const Rcpp::List layer,
        const Rcpp::DataFrame points)
{
    const std::vector <double> x = points ["x"], y = points ["y"];
    const int n = x.size ();

    Rcpp::IntegerVector count (layer.size (), 0);
    for (int i = 0; i < layer.size (); i++)
    {
        Rcpp::DataFrame li = Rcpp::as <Rcpp::DataFrame> (layer [i]);
        Rcpp::NumericVector lx = li ["x"], ly = li ["y"];
        ClipperLib::Path path;
        for (size_t j = 0; j < lx.size (); j++)
            path << ClipperLib::IntPoint (round (lx [j] * mult),
                    round (ly [j] * mult));

        for (int j = 0; j < n; j++)
        {
            const ClipperLib::IntPoint pj =
                ClipperLib::IntPoint (round (x [j] * mult),
                                      round (y [j] * mult));
            int pip = ClipperLib::PointInPolygon (pj, path);
            if (pip != 0) count (i)++;
        }
    }

    return count;
}

//' rcpp_areas
//'
//' layer The convex hulls of the moveability polygons
//' @noRd
// [[Rcpp::export]]
Rcpp::NumericVector rcpp_areas (
        const Rcpp::List layer)
{
    const int n = layer.size ();

    Rcpp::NumericVector areas (n, 0.0);
    for (int i = 0; i < n; i++)
    {
        Rcpp::DataFrame li = Rcpp::as <Rcpp::DataFrame> (layer [i]);
        Rcpp::NumericVector lx = li ["x"], ly = li ["y"];
        ClipperLib::Path path;
        for (size_t j = 0; j < lx.size (); j++)
            path << ClipperLib::IntPoint (round (lx [j] * mult),
                    round (ly [j] * mult));

        areas [i] = fabs (ClipperLib::Area (path)) / (mult * mult);
    }

    return areas;
}
