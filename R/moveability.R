#' moveability
#'
#' Calculate moveability statistics for a specified city
#'
#' @param streetnet Instead of city, a pre-downloaded or prepared street network
#' can be submitted. Must be either an \pkg{sf}, \pkg{osmdata} or \pkg{dodgr}
#' format.
#' @param city City for which moveability statistics are to be calcualted.
#' @param d_threshold Distance threshold below which distances are to be
#' aggreagted (in kilometres).
#' @param mode Mode of transport: either "foot" or "bicycle"
#' @param quiet If `TRUE`, dump progress information to screen.
#' @return Nothing (open interactive map)
#' @examples
#' m <- moveability (streetnet = castlemaine)
#' @export
moveability <- function (streetnet = NULL, city = NULL, d_threshold = 1,
                         mode = "foot", quiet = FALSE)
{
    if (is.null (city) & is.null (streetnet))
        stop ("city or streetnet must be provided")
    if (!is.null (city) & !is.null (streetnet))
        message ("City will be ignored, as streetnet has been provided")

    if (!is.null (city))
    {
        # nocov start
        streetnet <- osmdata::opq (city) %>%
            osmdata::add_osm_feature (key = "highway") %>%
            osmdata::osmdata_sc (quiet = FALSE)
        # nocov end
    } else if (!(methods::is (streetnet, "osmdata_sc") |
               methods::is (streetnet, "dodgr_streetnet_sc")))
        stop ("streetnet must be of format osmdata_sc, or dodgr_streetnet_sc")
       
    if (!methods::is (streetnet, "dodgr_streetnet_sc"))
    { # then convert to dodgr fmt
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
    } else
    {
        netc <- dodgr::dodgr_contract_graph (streetnet)
        from <- unique (netc$.vx0)
        verts <- dodgr::dodgr_vertices (netc)
        verts <- verts [which (verts$id %in% from), ]
    }

    verts$m <- move_stats (netc, from = from, quiet = quiet)
    return (verts)
}
