
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


std::shared_ptr <HeapDesc> run_sp::getHeapImpl(const std::string& heap_type)
{
    return std::make_shared <HeapD<BHeap> >();
}


struct OneDist : public RcppParallel::Worker
{
    RcppParallel::RVector <int> dp_fromi;
    const size_t nverts;
    const double d_threshold;
    const std::shared_ptr <DGraph> g;
    const std::string heap_type;

    RcppParallel::RVector <double> dout;

    // constructor
    OneDist (
            const Rcpp::IntegerVector fromi,
            const size_t nverts_in,
            const double d_threshold_in,
            const std::shared_ptr <DGraph> g_in,
            const std::string & heap_type_in,
            Rcpp::NumericVector dout_in) :
        dp_fromi (fromi), nverts (nverts_in), d_threshold (d_threshold_in),
        g (g_in), heap_type (heap_type_in), dout (dout_in)
    {
    }

    // Parallel function operator
    void operator() (std::size_t begin, std::size_t end)
    {
        std::shared_ptr<Dijkstra> dijkstra =
            std::make_shared <Dijkstra> (nverts,
                    *run_sp::getHeapImpl (heap_type), g);
        std::vector <double> w (nverts);
        std::vector <double> d (nverts);
        std::vector <int> prev (nverts);

        for (std::size_t i = begin; i < end; i++)
        {
            // These have to be reserved within the parallel operator function!
            std::fill (w.begin (), w.end (), INFINITE_DOUBLE);
            std::fill (d.begin (), d.end (), INFINITE_DOUBLE);

            dijkstra->run (d, w, prev,
                    static_cast <unsigned int> (dp_fromi [i]),
                    d_threshold);

            // Then need to get the sum of only those terminal distances that
            // are < d_threshold.

            // Then need to get the sum of only those terminal distances that
            // are < d_threshold.  
            std::vector <bool> has_prev (nverts, false);
            std::unordered_set <int> prev_set;
            for (unsigned int j = 0; j < nverts; j++)
            {
                if (w [j] > d_threshold)
                    prev [j] = -1;
                else
                {
                    has_prev [j] = true;
                    if (prev [j] > -1)
                        prev_set.emplace (prev [j]);
                }
            }
            for (unsigned int j = 0; j < nverts; j++)
            {
                if (has_prev [j] && prev_set.find (j) == prev_set.end ())
                    dout [i] += d [j];
            }
        }
    }
                                   
};

int run_sp::trace_back (const std::vector <int> &prev,
        std::vector <bool> &vert_done, int &ndone, const int here_in)
{
    int here = prev [here_in];
    if (here >= 0)
    {
        vert_done [here] = true;
        ndone++;
    }
    return here;
}


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
    size_t nverts = static_cast <size_t> (vert_map.size ());
    return (nverts);
}

size_t run_sp::get_fromi (const Rcpp::DataFrame &vert_map_in,
        Rcpp::IntegerVector &fromi, Rcpp::NumericVector &id_vec)
{
    if (fromi [0] < 0) // use all vertices
    {
        id_vec = vert_map_in ["id"];
        fromi = id_vec;
    }
    return static_cast <size_t> (fromi.size ());
}

// Flows from the dijkstra output are reallocated based on matching vertex
// pairs to edge indices. Note, however, that contracted graphs frequently
// have duplicate vertex pairs with different distances. The following
// therefore uses two maps, one to hold the ultimate index from vertex
// pairs, and the other to hold minimal distances. This is used in flow routines
// only.
void run_sp::make_vert_to_edge_maps (const std::vector <std::string> &from,
        const std::vector <std::string> &to, const std::vector <double> &wt,
        std::unordered_map <std::string, unsigned int> &verts_to_edge_map,
        std::unordered_map <std::string, double> &verts_to_dist_map)
{
    for (unsigned int i = 0; i < from.size (); i++)
    {
        std::string two_verts = "f" + from [i] + "t" + to [i];
        verts_to_edge_map.emplace (two_verts, i);
        if (verts_to_dist_map.find (two_verts) == verts_to_dist_map.end ())
            verts_to_dist_map.emplace (two_verts, wt [i]);
        else if (wt [i] < verts_to_dist_map.at (two_verts))
        {
            verts_to_dist_map [two_verts] = wt [i];
            verts_to_edge_map [two_verts] = i;
        }
    }
}

//' rcpp_get_sp_dists_par
//'
//' @noRd
// [[Rcpp::export]]
Rcpp::NumericVector rcpp_get_sp_dists_par (const Rcpp::DataFrame graph,
        const Rcpp::DataFrame vert_map_in,
        Rcpp::IntegerVector fromi,
        const double d_threshold,
        const std::string& heap_type)
{
    Rcpp::NumericVector id_vec;
    size_t nfrom = run_sp::get_fromi (vert_map_in, fromi, id_vec);

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

    Rcpp::NumericVector dout (static_cast <int> (nfrom), 0.0);

    // Create parallel worker
    OneDist one_dist (fromi, nverts, d_threshold, g, heap_type, dout);

    RcppParallel::parallelFor (0, static_cast <size_t> (fromi.length ()),
            one_dist);
    
    return (dout);
}

//' rcpp_get_sp_dists
//'
//' @noRd
// [[Rcpp::export]]
Rcpp::NumericVector rcpp_get_sp_dists (const Rcpp::DataFrame graph,
        const Rcpp::DataFrame vert_map_in,
        Rcpp::IntegerVector fromi,
        const double d_threshold,
        const std::string& heap_type)
{
    Rcpp::NumericVector id_vec;
    size_t nfrom = run_sp::get_fromi (vert_map_in, fromi, id_vec);

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

    std::shared_ptr<DGraph> g = std::make_shared<DGraph>(nverts);
    inst_graph (g, nedges, vert_map, from, to, dist, wt);

    std::shared_ptr <Dijkstra> dijkstra =
        std::make_shared <Dijkstra> (
            nverts, *run_sp::getHeapImpl(heap_type), g);

    std::vector<double> w (nverts);
    std::vector<double> d (nverts);
    std::vector<int> prev (nverts);

    dijkstra->init (g); // specify the graph

    Rcpp::NumericVector dout (static_cast <int> (nfrom), 0.0);

    for (unsigned int v = 0; v < nfrom; v++)
    {
        Rcpp::checkUserInterrupt ();
        std::fill (w.begin(), w.end(), INFINITE_DOUBLE);
        std::fill (d.begin(), d.end(), INFINITE_DOUBLE);

        dijkstra->run (d, w, prev,
                static_cast <unsigned int> (fromi [v]), d_threshold);

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
                dout [v] += d [i];
        }

        Rcpp::checkUserInterrupt ();
    }
    return (dout);
}


