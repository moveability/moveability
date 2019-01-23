#include "utils.h"

//' rcpp_reverse_index
//'
//' Return an index of rows, j > i, for which 
//' [from(j) == to (i), to(j) == from (i)]. These are edges duplicated in
//' reverse which can then be removed.
//'
//' @param df Rcpp::DataFrame with 2 columns named "from" and "to"
//'
//' @return 0-indexed Rcpp::NumericVector index into graph of nearest points
//'
//' @noRd
// [[Rcpp::export]]
Rcpp::IntegerVector rcpp_reverse_index (const Rcpp::DataFrame &df)
{
    Rcpp::CharacterVector from = df ["from"];
    Rcpp::CharacterVector to = df ["to"];

    std::unordered_set <std::string> ftset;
    std::set <int> index;

    for (int i = 0; i < from.size (); i++)
    {
        std::string sft = static_cast <std::string> (from [i]) + "-" +
            static_cast <std::string> (to [i]),
                    stf = static_cast <std::string> (to [i]) + "-" +
            static_cast <std::string> (from [i]);
        if (ftset.find (sft) == ftset.end () &&
                ftset.find (stf) == ftset.end ())
        {
            ftset.emplace (sft);
            ftset.emplace (stf);
        } else
        {
            index.emplace (i);
        }
    }

    Rcpp::IntegerVector res (index.size ());
    int i = 0;
    for (auto it : index)
        res (i++) = it + 1;

    return res;
}
