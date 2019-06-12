#' moveability
#'
#' Calculate moveability statistics for a specified city
#'
#' @param streetnet Pre-downloaded or prepared street network in either
#' `osmdata_sc` or `dodgr_sc` format.
#' @param green_polys Polygons of green space obtained from
#' \link{get_green_space}
#' @param d_threshold Distance threshold below which distances are to be
#' aggreagted (in kilometres).
#' @param mode Mode of transport: either "foot" or "bicycle"
#' @param quiet If `TRUE`, dump progress information to screen.
#' @return Nothing (open interactive map)
#' @examples
#' m <- moveability (streetnet = castlemaine, green_polys = castlemaine_green)
#' @export
moveability <- function (streetnet = NULL, green_polys = NULL, d_threshold = 1,
                         mode = "foot", quiet = FALSE)
{
    if (is.null (streetnet))
        stop ("streetnet must be provided")
    if (is.null (green_polys))
        stop ("green_polys must be provided")
    else if (!(methods::is (streetnet, "osmdata_sc") |
               methods::is (streetnet, "dodgr_streetnet_sc")))
        stop ("streetnet must be of format osmdata_sc, or dodgr_streetnet_sc")
       
    obj <- construct_moveability_objects (streetnet, mode, quiet)

    m <- move_stats (obj$net,
                     from = obj$from,
                     green_polys = green_polys,
                     d_threshold = d_threshold,
                     quiet = quiet)
    obj$verts$m <- m$m
    obj$verts$green_area = m$area

    return (obj$verts)
}

# The primary objects are the contracted street network, the table of junction
# vertices, and the names of central vertices to be used for routing. Note that
# the inclusion of turn penalties for mode = "bicycle" means that the `from`
# vertices include potential "_start" suffixes, whereas names in the `verts`
# table do not include these. Thus, the explicit `from` object must also be
# constructed and passed to `move_stats`
construct_moveability_objects <- function (streetnet, mode, quiet)
{
    if (!methods::is (streetnet, "dodgr_streetnet_sc"))
    { # then convert to dodgr fmt
        res <- convert_streetnet (streetnet, mode, quiet)
    } else
    {
        netc <- dodgr::dodgr_contract_graph (streetnet)
        from <- unique (netc$.vx0)
        verts <- dodgr::dodgr_vertices (netc)
        verts <- verts [which (verts$id %in% from), ]

        res <- list (net = netc, from = from, verts = verts)
    }

    return (res)
}

convert_streetnet <- function (streetnet, mode, quiet)
{
    if (!mode %in% c ("foot", "bicycle"))
        stop ("mode must be either foot or bicycle")
    if (!quiet)
    {
        message ("contracting street network ... ", appendLF = FALSE)
        pt0 <- proc.time ()
    }

    dodgr::dodgr_cache_off ()
    if (mode == "foot")
    {
        streetnet <- dodgr::weight_streetnet (streetnet,
                                              wt_profile = mode)
        netc <- dodgr::dodgr_contract_graph (streetnet)
        verts <- dodgr::dodgr_vertices (netc)
        from <- verts$id
    } else # bicycle
    {
        streetnet_t <- dodgr::weight_streetnet (streetnet,
                                                wt_profile = "bicycle",
                                                turn_penalty = TRUE)
        streetnet <- dodgr::weight_streetnet (streetnet,
                                              wt_profile = "bicycle",
                                              turn_penalty = FALSE)

        # select vertices from `turn_penalty = F`, then re-map them onto
        # graph with turn penalties:
        streetnet_c <- dodgr::dodgr_contract_graph (streetnet)
        verts <- dodgr::dodgr_vertices (streetnet_c)

        v0 <- gsub ("_start", "",
                    streetnet_t$.vx0 [grep ("_start", streetnet_t$.vx0)])
        from <- verts$id
        from [from %in% v0] <- paste0 (from [from %in% v0], "_start")

        netc <- dodgr::dodgr_contract_graph (streetnet_t, verts = from)

        # some vertices may nevertheless only end up as destination vertices
        # in contracted graph, so:
        index <- which (from %in% netc$.vx0)
        from <- from [index]
        verts <- verts [index, ]
    }
    if (!quiet)
    {
        pt <- paste0 (round ((proc.time () - pt0) [3]))
        message (paste ("done in", pt, "seconds."))
    }
    
    list (net = netc, from = from, verts = verts)
}
