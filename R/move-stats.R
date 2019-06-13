#' move_stats
#'
#' Calculate vector of moveability statistics for a given input street network.
#'
#' @inheritParams moveability
#' @param graph Street network in \pkg{dodgr} format obtained through applying
#' `dodgr::weight_streetnet` to an `osmdata_sc` object.
#' @param from Vector of points from which moveability statistics are to be be
#' calculated.
#' @return Vector of moveability values for each point in `from`, with
#' moveability quantified as `$m`.
#'
#' @export 
#' @examples
#' graph <- dodgr::weight_streetnet (castlemaine)
#' green_polys <- castlemaine_green # green polygon data included with package
#' from <- sample (graph$.vx0, size = 100)
#' d <- move_stats (graph, green_polys = green_polys, from = from)
#' # d is a `data.frame` of the coordinates of all `from` points and
#' # correponding moveability statisics 
move_stats <- function (graph, from, green_polys, d_threshold = 1, quiet = FALSE)
{
    if (missing (from))
        stop ("from must be provided")

    v <- dodgr::dodgr_vertices (graph) # used to extract points below
    d_threshold <- d_threshold * 1000 # convert to metres

    vert_map <- make_vert_map (graph)
    from_index <- match (from, vert_map$vert) - 1 # 0-based
    vert_id <- vert_map$vert [from_index + 1]

    graph <- convert_graph (graph)

    if (!quiet)
    {
        message ("Calculating shortest paths from ",
                 format (length (from), big.mark = ","),
                 " points ... ", appendLF = FALSE)
        pt0 <- proc.time ()
    }

    d <- rcpp_get_sp_dists_par (graph, vert_map, from_index, d_threshold,
                                heap_type = "BHeap")
    # returns a matrix which can be used to extract all points within
    # d_threshold, but for the moment, just calculate the total sums of
    # distances:
    d <- matrix (d, nrow = nrow (vert_map), ncol = length (from_index))

    if (!quiet)
        message ("done\nCalculating areas of green space ... ", appendLF = FALSE)
    pt1 <- proc.time ()
    areas <- green_areas (d, green_polys, v, vert_id, vert_map)
    pt1 <- proc.time () [3] - pt1 [3]

    m <- colSums (d)
    res <- data.frame (id = vert_id,
                       m = m,
                       hull_area = areas$hull_area,
                       green_area = areas$green_area,
                       stringsAsFactors = FALSE)

    if (!quiet)
    {
        pt <- format ((proc.time () - pt0) [3], format = "f", digits = 2)
        message (paste ("done; elapsed time = ", pt, "seconds."))
        message ("Polygon intersection calculation took ",
                 formatC (pt1, format="f", digits = 2), " seconds.")
    }

    return (res)
}

#' move_statistics
#'
#' Alias for \link{move_stats}
#' @inherit move_stats
#' @export
move_statistics <- function (graph, from, green_polys, d_threshold = 1,
                             quiet = TRUE)
{
    move_stats (graph, from, green_polys, d_threshold, quiet)
}

#' make_vert_map
#'
#' Map unique vertex names to sequential numbers in matrix
#' @noRd
make_vert_map <- function (graph)
{
    verts <- c (graph$.vx0, graph$.vx1)
    indx <- which (!duplicated (verts))
    # Note id has to be 0-indexed:
    data.frame (vert = paste0 (verts [indx]), id = seq (indx) - 1,
                stringsAsFactors = FALSE)
}

#' convert_graph
#'
#' Convert graph to a standard form suitable for submission to C++ routines
#' NOTE: hard-coded here for SC-format only
#' @noRd
convert_graph <- function (graph, gr_cols)
{
    graph <- tbl_to_df (graph)
    data.frame ("edge_id" = graph$edge_,
                "from" = graph$.vx0,
                "to" = graph$.vx1,
                "component" = graph$component,
                "d" = graph$d,
                "w" = graph$d_weighted,
                "time" = graph$time,
                "time_weighted" = graph$time_weighted,
                stringsAsFactors = FALSE)
}

tbl_to_df <- function (graph)
{
    if (methods::is (graph, "tbl"))
    {
        classes <- class (graph) [!grepl ("tbl", class (graph))]
        graph <- as.data.frame (graph)
        class (graph) <- classes
    }
    return (graph)
}

green_areas <- function (dmat, green_polys, vertices, vert_id, vert_map)
{
    pts <- apply (dmat, 2, function (i) which (i > 0))
    names (pts) <- vert_id

    # get convex hulls:
    pts <- lapply (pts, function (i) {
                       ids <- vert_map$vert [i]
                       v <- vertices [match (ids, vertices$id), ]
                       index <- grDevices::chull (v$x, v$y)
                       v [c (index, index [1]), ]   })

    green <- lapply (green_polys$geometry, function (i) i [[1]])
    x <- rcpp_clipper (pts, green)

    res <- data.frame (id = names (pts),
                       hull_area = x$hull_area,
                       green_area = x$green_area)
    res$id <- gsub ("_start", "", res$id)

    return (res)
}
