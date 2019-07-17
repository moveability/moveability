
#include "run_sp.h"

#include "dgraph.h"
#include "heaps/bheap.h"

template <typename T>
void inst_graph (std::shared_ptr<DGraph> g, unsigned int nedges,
        const std::map <std::string, unsigned int>& vert_map,
        const std::vector <std::string>& from,
        const std::vector <std::string>& to,
        const std::vector <T>& dist,
        const std::vector <T>& wt)
{
    for (unsigned int i = 0; i < nedges; ++i)
    {
        unsigned int fromi = vert_map.at(from [i]);
        unsigned int toi = vert_map.at(to [i]);
        g->addNewEdge (fromi, toi, dist [i], wt [i]);
    }
}



struct OneDist : public RcppParallel::Worker
{
    RcppParallel::RVector <int> dp_fromi;
    const size_t nfrom;
    const size_t nverts;
    const double d_threshold;
    const std::shared_ptr <DGraph> g;

    RcppParallel::RVector <double> dout;

    // constructor
    OneDist (
            const Rcpp::IntegerVector fromi,
            const size_t nfrom_in,
            const size_t nverts_in,
            const double d_threshold_in,
            const std::shared_ptr <DGraph> g_in,
            Rcpp::NumericVector dout_in) :
        dp_fromi (fromi), nfrom (nfrom_in), nverts (nverts_in),
        d_threshold (d_threshold_in),
        g (g_in), dout (dout_in)
    {
    }

    // Parallel function operator
    void operator() (std::size_t begin, std::size_t end)
    {
        HeapD <BHeap> bheap;
        std::shared_ptr<Dijkstra> dijkstra =
            std::make_shared <Dijkstra> (nverts, bheap, g);
        std::vector <double> w (nverts);
        std::vector <double> d (nverts);
        std::vector <int> prev (nverts);

        for (std::size_t v = begin; v < end; v++)
        {
            // These have to be reserved within the parallel operator function!
            std::fill (w.begin (), w.end (), INFINITE_DOUBLE);
            std::fill (d.begin (), d.end (), INFINITE_DOUBLE);

            dijkstra->run (d, w, prev,
                    static_cast <unsigned int> (dp_fromi [v]), d_threshold);

            // Then need to get the sum of only those terminal distances that
            // are < d_threshold.  
            std::vector <bool> has_prev (nverts, false);
            std::unordered_set <int> prev_set;
            for (unsigned int i = 0; i < nverts; i++)
            {
                if (w [i] > d_threshold)
                    prev [i] = -1;
                else
                {
                    has_prev [i] = true;
                    if (prev [i] > -1)
                        prev_set.emplace (prev [i]);
                }
            }
            for (unsigned int i = 0; i < nverts; i++)
            {
                if (has_prev [i] && prev_set.find (i) == prev_set.end ())
                    dout [v * nverts + i] = d [i];
                    //dout [v] += d [i];
            }
        }
    }
                                   
};

size_t run_sp::make_vert_map (const Rcpp::DataFrame &vert_map_in,
        const std::vector <std::string> &vert_map_id,
        const std::vector <unsigned int> &vert_map_n,
        std::map <std::string, unsigned int> &vert_map)
{
    for (unsigned int i = 0;
            i < static_cast <unsigned int> (vert_map_in.nrow ()); ++i)
    {
        vert_map.emplace (vert_map_id [i], vert_map_n [i]);
    }
    return (static_cast <size_t> (vert_map.size ()));
}

//' rcpp_get_sp_dists_par
//'
//' @noRd
// [[Rcpp::export]]
Rcpp::NumericVector rcpp_get_sp_dists_par (const Rcpp::DataFrame graph,
        const Rcpp::DataFrame vert_map_in,
        Rcpp::IntegerVector fromi,
        const double d_threshold)
{
    Rcpp::NumericVector id_vec;
    size_t nfrom = fromi.size ();

    std::vector <std::string> from = graph ["from"];
    std::vector <std::string> to = graph ["to"];
    std::vector <double> dist = graph ["d"];
    std::vector <double> wt = graph ["w"];

    unsigned int nedges = static_cast <unsigned int> (graph.nrow ());
    std::map <std::string, unsigned int> vert_map;
    std::vector <std::string> vert_map_id = vert_map_in ["vert"];
    std::vector <unsigned int> vert_map_n = vert_map_in ["id"];
    size_t nverts = run_sp::make_vert_map (vert_map_in, vert_map_id,
            vert_map_n, vert_map);

    std::shared_ptr <DGraph> g = std::make_shared <DGraph> (nverts);
    inst_graph (g, nedges, vert_map, from, to, dist, wt);

    // dout is actuall the full matrix, but has to be stored as a vector,
    // because an RcppParalel::RMatrix must iterate over single entries, but we
    // need here to iterate over rows only
    Rcpp::NumericVector dout (static_cast <long int> (nfrom * nverts), 0.0);

    // Create parallel worker
    OneDist one_dist (fromi, nfrom, nverts, d_threshold, g, dout);

    RcppParallel::parallelFor (0, static_cast <size_t> (fromi.length ()),
            one_dist);
    
    return (dout);
}
