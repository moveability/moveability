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
#' m <- moveability (streetnet = castlemaine, green_polys = castlemaine_green,
#'                   activity_points = castlemaine_attr)
#' p <- moveability_to_polygons (m = m, streetnet = castlemaine)
#' @export
moveability_to_polygons <- function (m, streetnet)
{
    if (!methods::is (streetnet, "dodgr_streetnet_sc"))
        streetnet <- dodgr::weight_streetnet (streetnet, wt_profile = 1)

    streetnet$flow <- 1
    streetnet <- dodgr::merge_directed_graph (streetnet)
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
#' m <- moveability (streetnet = castlemaine, green_polys = castlemaine_green,
#'                   activity_points = castlemaine_attr)
#' l <- moveability_to_lines (m = m, streetnet = castlemaine)
#' @export
moveability_to_lines <- function (m, streetnet)
{
    if (!methods::is (streetnet, "dodgr_streetnet_sc"))
        streetnet <- dodgr::weight_streetnet (streetnet, wt_profile = 1)

    streetnet <- streetnet [streetnet$component == 1, ]

    graphc <- dodgr::dodgr_contract_graph (streetnet)
    v <- dodgr::dodgr_vertices (graphc)
    m <- m [which (m$id %in% v$id), ]
    # scale moveability to number of activities:
    m$m <- m$m * m$activity_centres

    m_from <- m$m [match (graphc$.vx0, m$id)]
    m_to <- m$m [match (graphc$.vx1, m$id)]
    mvals <- apply (cbind (m_from, m_to), 1, function (i)
                    mean (i, na.rm = TRUE))
    mvals [is.nan (mvals)] <- NA

    graphc$flow <- mvals
    graph <- dodgr::dodgr_uncontract_graph (graphc)
    graph <- dodgr::merge_directed_graph (graph)
    dodgr::dodgr_to_sf (graph)
}
