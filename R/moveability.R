#' moveability
#'
#' Calculate moveability statistics for a specified city
#'
#' @param city City for which moveability statistics are to be calcualted.
#' @param streetnet Instead of city, a pre-downloaded or prepared street network
#' can be submitted. Must be either an \pkg{sf}, \pkg{osmdata} or \pkg{dodgr}
#' format.
#' @param d_threshold Distance threshold below which distances are to be
#' aggreagted (in kilometres).
#' @param quiet If `TRUE`, dump progress information to screen.
#' @return Nothing (open interactive map)
#' @export
moveability <- function (city = NULL, streetnet = NULL, d_threshold = 1,
                         quiet = FALSE)
{
    if (is.null (city) & is.null (streetnet))
        stop ("city or streetnet must be specified")
    if (!is.null (city) & !is.null (streetnet))
        message ("City will be ignored, as streetnet has been provided")

    if (!is.null (city))
        streetnet <- dodgr::dodgr_streetnet (bbox = city, expand = 0.05)
    else if (!(methods::is (streetnet, "sf") |
               methods::is (streetnet, "osmdata") |
               methods::is (streetnet, "dodgr_streetnet")))
        stop ("streetnet must be of format sf, osmdata, or dodgr")
       
    if (methods::is (streetnet, "osmdata"))
        streetnet <- osmdata::osm_poly2line (streetnet)$osm_lines

    if (!methods::is (streetnet, "dodgr_streetnet"))
        streetnet <- dodgr::weight_streetnet (streetnet, wt_profile = "foot")
    netc <- dodgr::dodgr_contract_graph (streetnet)
    verts <- dodgr::dodgr_vertices (netc$graph)

    netc_w <- netc$graph
    netc_w$d <- netc_w$d_weighted

    verts$m <- move_dists (netc_w, from = verts$id, quiet = quiet)
    return (verts)
}
