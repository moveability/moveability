#' moveability
#'
#' Calculate moveability statistics for a specified city
#'
#' @param city City for which moveability statistics are to be calcualted.
#' @param streetnet Instead of city, a pre-downloaded or prepared street network
#' can be submitted. Must be either an \pkg{sf}, \pkg{osmdata} or \pkg{dodgr}
#' format.
#' @param quiet If `TRUE`, dump progress information to screen.
#' @return Nothing (open interactive map)
#' @export
moveability <- function (city = NULL, streetnet = NULL, quiet = FALSE)
{
    if (is.null (city) & is.null (streetnet))
        stop ("city must be specified")
    if (!is.null (city) & !is.null (streetnet))
        message ("City will be ignored, as streetnet has been provided")

    if (!is.null (city))
        streetnet <- dodgr::dodgr_streetnet (bbox = city, expand = 0.05)
    else if (!(methods::is (streetnet, "sf") |
               methods::is (streetnet, "osmdata") |
               methods::is (streetnet, "dodgr")))
        stop ("streetnet must be of format sf, osmdata, or dodgr")
       
    if (methods::is (streetnet, "osmdata"))
        streetnet <- osmdata::osm_poly2line (streetnet)$osm_lines

    if (!methods::is (streetnet, "dodgr_streetnet"))
        streetnet <- dodgr::weight_streetnet (streetnet, wt_profile = "foot")
    netc <- dodgr::dodgr_contract_graph (streetnet)
    verts <- dodgr::dodgr_vertices (netc$graph)
    ids_all <- verts$id
    ids <- split (verts, cut (verts$n,
                              breaks = ceiling (nrow (verts) / 1000)))

    netc_w <- netc$graph
    netc_w$d <- netc_w$d_weighted

    get1d <- function (netc_w, from, to)
    {
        d <- dodgr::dodgr_dists (netc_w, from = from, to = to)

        # fixed walking radius of 1km for the moment
        d [is.na (d)] <- d [d > 1] <- 0
        rowSums (d)
    }
    
    m <- pbapply::pblapply (seq (ids), function (i)
                            get1d (netc_w, ids [[i]]$id, ids_all))
    verts$m <- do.call (c, m)
    verts <- verts [which (verts$component == 1), ]
    verts$component <- verts$n <- NULL
    return (verts)
}
