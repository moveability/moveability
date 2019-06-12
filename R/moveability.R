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
        stop ("city or streetnet must be specified")
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

#' moveability_to_polygons
#'
#' Project moveability statistics from \link{moveability} on to polygonal blocks
#' of street network, in order to plot polygons rather than points.
#'
#' @note This function may take a long time to execute, because of the
#' calculation of fundamental cycles in the street network graph used to
#' identify street blocks.
#'
#' @param m Result of \link{moveability} function
#' @param streetnet Street network in either \pkg{sf}, \pkg{osmdata} or \pkg{dodgr}
#' format.
#' @return \pkg{sf} collection of `POLYGON` objects corresponding to street
#' blocks, with average moveability statistics from all points defining that
#' block.
#' @examples
#' m <- moveability (streetnet = castlemaine)
#' p <- moveability_to_polygons (m = m, streetnet = castlemaine)
#' @export
moveability_to_polygons <- function (m, streetnet)
{
    if (!methods::is (streetnet, "dodgr_streetnet_sc"))
        streetnet <- dodgr::weight_streetnet (streetnet, wt_profile = 1)

    streetnet$flow <- 1
    streetnet <- dodgr::merge_directed_flows (streetnet)
    streetnet$flow <- NULL
    streetnet <- streetnet [streetnet$component == 1, ]

    message ("calculating fundamental cycles ... ")
    cycles <- dodgr::dodgr_full_cycles (streetnet)
    # attach mean moveability to each cycles:
    cycles <- lapply (cycles, function (i) {
                          indx <- match (i$id, m$id)
                          if (length (which (!is.na (indx))) > 0)
                          {
                              i$m <- mean (m$m [indx], na.rm = TRUE)
                              return (i)
                          } else
                              return (NULL) # nocov
                         })
    # then remove all NULL entries
    polygons_to_sf (cycles [which (!vapply (cycles, is.null, logical (1)))])
}

# Convert polygons produced from \link{moveability_to_polygons} into equivalent
# \pkg{sf} format.
polygons_to_sf <- function (polygons)
{
    requireNamespace ("sf")

    mvals <- unlist (lapply (polygons, function (i) i$m [1]))

    xy <- do.call (rbind, polygons)
    xvals <- xy [, 2]
    yvals <- xy [, 3]
    bb <- structure (rep (NA_real_, 4),
                     names = c("xmin", "ymin", "xmax", "ymax"))
    bb [1:4] <- c (min (xvals), min (yvals), max (xvals), max (yvals))
    class (bb) <- "bbox"

    crs <- list (epsg = 4326L,
                 proj4string = "+proj=longlat +datum=WGS84 +no_defs")
    class (crs) <- "crs"
    attr (bb, "crs") <- crs

    polygons <- lapply (polygons, function (i) {
                            res <- as.matrix (i [, c ("x", "y")])
                            colnames (res) <- NULL
                            structure (list (res),
                                       class = c ("XY", "POLYGON", "sfg"))
                 })
    attr (polygons, "n_empty") <- 0
    attr (polygons, "precision") <- 0.0
    class (polygons) <- c ("sfc_POLYGON", "sfc")
    attr (polygons, "bbox") <- bb
    attr (polygons, "crs") <- crs

    sf::st_sf (moveability = mvals, geometry = polygons)
}

#' moveability_to_lines
#'
#' Project moveability statistics from \link{moveability} on to lines
#' of street network.
#'
#' @param m Result of \link{moveability} function
#' @param streetnet Street network in either \pkg{sf}, \pkg{osmdata} or \pkg{dodgr}
#' format.
#' @return \pkg{sf} collection of `LINE` objects of the street network,
#' with moveability statistics averaged between the two end points of each line
#' segment.
#' @examples
#' m <- moveability (streetnet = castlemaine)
#' # l <- moveability_to_lines (m = m, streetnet = castlemaine)
#' # lsf <- sf::st_sf (l$dat, geometry = l$geometry)
#' @export
moveability_to_lines <- function (m, streetnet)
{
    if (!methods::is (streetnet, "dodgr_streetnet_sc"))
        streetnet <- dodgr::weight_streetnet (streetnet, wt_profile = 1)

    streetnet <- streetnet [streetnet$component == 1, ]

    graphc <- dodgr::dodgr_contract_graph (streetnet)
    v <- dodgr::dodgr_vertices (graphc)
    m <- m [which (m$id %in% v$id), ]

    m_from <- m$m [match (graphc$.vx0, m$id)]
    m_to <- m$m [match (graphc$.vx1, m$id)]
    mvals <- apply (cbind (m_from, m_to), 1, function (i)
                    mean (i, na.rm = TRUE))
    mvals [is.nan (mvals)] <- NA

    graphc$flow <- mvals
    graph <- dodgr::dodgr_uncontract_graph (graphc)
    graph <- dodgr::merge_directed_flows (graph)
    dodgr::dodgr_to_sf (graph)
}
