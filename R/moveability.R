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
#' m <- moveability (streetnet = dodgr::hampi)
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
        streetnet <- mv_streetnet (city = city, quiet = quiet)
    } else if (!(methods::is (streetnet, "sf") |
               methods::is (streetnet, "osmdata") |
               methods::is (streetnet, "osmdata_sc") |
               methods::is (streetnet, "dodgr_streetnet")))
        stop ("streetnet must be of format sf, osmdata, or dodgr")
       
    if (methods::is (streetnet, "osmdata"))
        streetnet <- osmdata::osm_poly2line (streetnet)$osm_lines

    if (!methods::is (streetnet, "dodgr_streetnet"))
    {
        if (!mode %in% c ("foot", "bicycle"))
            stop ("mode must be either foot or bicycle")
        if (!quiet)
        {
            message ("contracting street network ... ", appendLF = FALSE)
            pt0 <- proc.time ()
        }

        if (mode == "foot")
        {
            streetnet <- dodgr::weight_streetnet (streetnet,
                                                  wt_profile = mode)
            netc <- dodgr::dodgr_contract_graph (streetnet)
            verts <- dodgr::dodgr_vertices (netc$graph)
            from <- verts$id
        } else # bicycle
        {
            streetnet_t <- dodgr::weight_streetnet (streetnet,
                                                    wt_profile = "bicycle",
                                                    turn_angle = TRUE)
            streetnet <- dodgr::weight_streetnet (streetnet,
                                                  wt_profile = "bicycle",
                                                  turn_angle = FALSE)

            # select vertices from `turn_angle = F`, then re-map them onto graph
            # with turn angles:
            streetnet_c <- dodgr_contract_graph (streetnet)
            verts <- dodgr_vertices (streetnet_c$graph)

            v0 <- gsub ("_start", "",
                        streetnet_t$.vx0 [grep ("_start", streetnet_t$.vx0)])
            from <- verts$id
            from [from %in% v0] <- paste0 (from [from %in% v0], "_start")

            netc <- dodgr::dodgr_contract_graph (streetnet_t, verts = from)

            # some vertices may nevertheless only end up as destination vertices
            # in contracted graph, so:
            index <- which (from %in% netc$graph$.vx0)
            from <- from [index]
            verts <- verts [index, ]
        }
        if (!quiet)
        {
            pt <- paste0 (round ((proc.time () - pt0) [3]))
            message (paste ("done in", pt, "seconds."))
        }
    }

    verts$m <- move_stats (netc$graph, from = from, quiet = quiet)
    return (verts)
}

# largely from dodgr::dodgr_streetnet
mv_streetnet <- function (city = NULL, quiet)
{
    is_poly <- TRUE
    bb_poly <- osmdata::getbb (city, format_out = "polygon")
    if (is.list (bb_poly))
    {
        bb_poly <- bb_poly [[1]]
    } else if (nrow (bb_poly) == 2)
    {
        is_poly <- FALSE
    }
    bb <- apply (bb_poly, 2, range)
        
    qq <- osmdata::opq (bb)
    qq <- osmdata::add_osm_feature (qq, key = "highway")
    dat <- osmdata::osmdata_sf (qq, quiet = quiet)
    dat <- osmdata::osm_poly2line (dat)
    if (is_poly)
        dat <- osmdata::trim_osmdata (dat, bb_poly)
    return (dat$osm_lines)
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
#' m <- moveability (streetnet = dodgr::hampi)
#' p <- moveability_to_polygons (m = m, streetnet = dodgr::hampi)
#' @export
moveability_to_polygons <- function (m, streetnet)
{
    if (methods::is (streetnet, "osmdata"))
        streetnet <- osmdata::osm_poly2line (streetnet)$osm_lines
    if (!methods::is (streetnet, "dodgr_streetnet"))
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
                              return (NULL)
                         })
    # then remove all NULL entries
    cycles [which (!vapply (cycles, is.null, logical (1)))]
}

#' polygons_to_sf
#'
#' Convert polygons produced from \link{moveability_to_polygons} into equivalent
#' \pkg{sf} format.
#'
#' @param polygons Result of \link{moveability_to_polygons} function
#' @return Equivalent \pkg{sf} collection of `POLYGON` objects corresponding to
#' street blocks, with average moveability statistics from all points defining
#' that block.
#' @examples
#' m <- moveability (streetnet = dodgr::hampi)
#' p <- moveability_to_polygons (m = m, streetnet = dodgr::hampi)
#' psf <- polygons_to_sf (p)
#' mvals <- unlist (lapply (p, function (i) i$m [1]))
#' psf <- sf::st_sf (dat = mvals, geometry = psf)
#' @export
polygons_to_sf <- function (polygons)
{
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
                            res <- as.matrix (i [, 2:3])
                            colnames (res) <- NULL
                            structure (list (res),
                                       class = c ("XY", "POLYGON", "sfg"))
                 })
    attr (polygons, "n_empty") <- 0
    attr (polygons, "precision") <- 0.0
    class (polygons) <- c ("sfc_POLYGON", "sfc")
    attr (polygons, "bbox") <- bb
    attr (polygons, "crs") <- crs
    return (polygons)
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
#' m <- moveability (streetnet = dodgr::hampi)
#' l <- moveability_to_lines (m = m, streetnet = dodgr::hampi)
#' # lsf <- sf::st_sf (l$dat, geometry = l$geometry)
#' @export
moveability_to_lines <- function (m, streetnet)
{
    requireNamespace ("sf")

    if (methods::is (streetnet, "osmdata"))
        streetnet <- osmdata::osm_poly2line (streetnet)$osm_lines
    if (!methods::is (streetnet, "dodgr_streetnet"))
        streetnet <- dodgr::weight_streetnet (streetnet, wt_profile = 1)

    if (!"component" %in% names (streetnet))
        streetnet <- dodgr::dodgr_components (streetnet)
    streetnet <- streetnet [streetnet$component == 1, ]

    graphc <- dodgr::dodgr_contract_graph (streetnet)
    v <- dodgr::dodgr_vertices (graphc$graph)
    m <- m [which (m$id %in% v$id), ]

    gr_cols <- get_graph_cols (graphc$graph)
    m_from <- m$m [match (graphc$graph [[gr_cols$from]], m$id)]
    m_to <- m$m [match (graphc$graph [[gr_cols$to]], m$id)]
    mvals <- apply (cbind (m_from, m_to), 1, function (i)
                    mean (i, na.rm = TRUE))
    mvals [is.nan (mvals)] <- NA

    graphc$graph$flow <- mvals
    graph <- uncontract_graph (graphc$graph, graphc$edge_map, streetnet)
    s <- dodgr::dodgr_to_sfc (graph)
    # merge_directed_flows can't be used here, so new rcpp_reverse_index fn
    # finds rows which are repeats of former rows but in reverse.
    g <- data.frame (from = s$dat$from_id, to = s$dat$to_id,
                     stringsAsFactors = FALSE)
    indx <- rcpp_reverse_index (g)
    indx <- seq (nrow (s$dat)) [!seq (nrow (s$dat)) %in% indx]
    s$dat <- s$dat [indx, ]
    s$geometry <- s$geometry [indx]
    indx <- which (!is.na (s$dat$flow))
    s$dat <- s$dat [indx, ]
    s$geometry <- s$geometry [indx]

    sf::st_sf (s$dat, geometry = s$geometry)
}

# direct copy from dodgr/R/flows.R
# map contracted flows back onto full graph
uncontract_graph <- function (graph, edge_map, graph_full)
{
    gr_cols <- get_graph_cols (graph_full)
    indx_to_full <- match (edge_map$edge_old, graph_full [[gr_cols$edge_id]])
    indx_to_contr <- match (edge_map$edge_new, graph [[gr_cols$edge_id]])
    # edge_map only has the contracted edges; flows from the original
    # non-contracted edges also need to be inserted
    edges <- graph [[gr_cols$edge_id]] [which (!graph [[gr_cols$edge_id]] %in%
                                               edge_map$edge_new)]
    indx_to_full <- c (indx_to_full, match (edges, graph_full [[gr_cols$edge_id]]))
    indx_to_contr <- c (indx_to_contr, match (edges, graph [[gr_cols$edge_id]]))
    graph_full$flow <- 0
    graph_full$flow [indx_to_full] <- graph$flow [indx_to_contr]

    return (graph_full)
}

