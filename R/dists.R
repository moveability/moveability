#' movedists
#'
#' Calculate matrix of pair-wise distances between points.
#'
#' @param graph `data.frame` or equivalent object representing the network
#' graph (see Details)
#' @param from Vector or matrix of points **from** which route distances are to
#' be calculated (see Details)
#' @param to Vector or matrix of points **to** which route distances are to be
#' calculated (see Details)
#' @param wt_profile Name of weighting profile for street networks (one of foot,
#' horse, wheelchair, bicycle, moped, motorcycle, motorcar, goods, hgv, psv).
#' @param expand Only when `graph` not given, the multiplicative factor by
#' which to expand the street network surrounding the points defined by
#' `from` and/or `to`.
#' @param parallel If `TRUE`, perform routing calculation in parallel (see
#' details)
#' @param quiet If `FALSE`, display progress messages on screen.
#' @return square matrix of distances between nodes
#'
#' @note `graph` must minimally contain three columns of `from`,
#' `to`, `dist`. If an additional column named `weight` or
#' `wt` is present, shortest paths are calculated according to values
#' specified in that column; otherwise according to `dist` values. Either
#' way, final distances between `from` and `to` points are calculated
#' according to values of `dist`. That is, paths between any pair of points
#' will be calculated according to the minimal total sum of `weight`
#' values (if present), while reported distances will be total sums of
#' `dist` values.
#'
#' The `from` and `to` columns of `graph` may be either single
#' columns of numeric or character values specifying the numbers or names of
#' graph vertices, or combinations to two columns specifying geographical
#' (longitude and latitude) coordinates. In the latter case, almost any sensible
#' combination of names will be accepted (for example, `fromx, fromy`,
#' `from_x, from_y`, or `fr_lat, fr_lon`.)
#'
#' `from` and `to` values can be either two-column matrices of
#' equivalent of longitude and latitude coordinates, or else single columns
#' precisely matching node numbers or names given in `graph$from` or
#' `graph$to`. If `to` is missing, pairwise distances are calculated
#' between all points specified in `from`. If neither `from` nor
#' `to` are specified, pairwise distances are calculated between all nodes
#' in `graph`.
#'
#' Calculations in parallel (`parallel = TRUE`) ought very generally be
#' advantageous. For small graphs, Calculating distances in parallel is likely
#' to offer relatively little gain in speed, but increases from parallel
#' computation will generally markedly increase with increasing graph sizes.
#'
#' @export 
#' @examples
#' # A larger example from the included [hampi()] data.
#' graph <- dodgr::weight_streetnet (dodgr::hampi)
#' from <- sample (graph$from_id, size = 100)
#' to <- sample (graph$to_id, size = 50)
#' d <- move_dists (graph, from = from, to = to)
#' # d is a 100-by-50 matrix of distances between `from` and `to`
move_dists <- function (graph, from, to, wt_profile = "bicycle", expand = 0,
                         parallel = TRUE, quiet = TRUE)
{
    heap <- "BHeap"

    gr_cols <- c (2, 3, 6, 9, 10, 4, 5, 7, 8, 13)
    names (gr_cols) <- c ("edge_id", "from", "to", "d", "w",
                          "xfr", "yfr", "xto", "yto", "component")
    class (gr_cols) <- c (class (gr_cols), "graph_columns")
    vert_map <- make_vert_map (graph, gr_cols)

    index_id <- get_index_id_cols (graph, gr_cols, vert_map, from)
    from_index <- index_id$index - 1 # 0-based
    from_id <- index_id$id
    index_id <- get_index_id_cols (graph, gr_cols, vert_map, to)
    to_index <- index_id$index - 1 # 0-based
    to_id <- index_id$id

    #graph <- dodgr:::convert_graph (graph, gr_cols)
    graph$geom_num <- graph$from_lon <- graph$from_lat <-
        graph$to_lon <- graph$to_lat <- graph$highway <-
        graph$way_id <- graph$component <- NULL
    names (graph) <- c ("edge_id", "from", "to", "d", "w")
    graph$edge_id <- paste0 (graph$edge_id)

    if (!quiet)
        message ("Calculating shortest paths ... ", appendLF = FALSE)

    flip <- FALSE
    if (length (from_index) > length (to_index))
    {
        flip <- TRUE
        graph <- flip_graph (graph)
        temp <- from_index
        from_index <- to_index
        to_index <- temp
    }

    if (parallel)
        d <- rcpp_get_sp_dists_par (graph, vert_map, from_index, to_index, heap)
    else
        d <- rcpp_get_sp_dists (graph, vert_map, from_index, to_index, heap)

    if (flip)
        d <- t (d)

    if (!is.null (from_id))
        rownames (d) <- from_id
    else
        rownames (d) <- vert_map$vert
    if (!is.null (to_id))
        colnames (d) <- to_id
    else
        colnames (d) <- vert_map$vert

    if (!quiet)
        message ("done.")

    return (d)
}

#' move_distances
#'
#' Alias for \link{move_dists}
#' @inherit move_dists
#' @export
move_distances <- function (graph, from, to, wt_profile = "bicycle", expand = 0,
                         parallel = TRUE, quiet = TRUE)
{
    move_dists (graph, from, to, wt_profile = wt_profile, expand = expand,
                parallel = parallel, quiet = quiet)
}

#' get_index_id_cols
#'
#' Get an index of `pts` matching `vert_map`, as well as the
#' corresonding names of those `pts`
#'
#' @return list of `index`, which is 0-based for C++, and corresponding
#' `id` values.
#' @noRd
get_index_id_cols <- function (graph, gr_cols, vert_map, pts)
{
    index <- -1
    id <- NULL
    if (!missing (pts))
    {
        index <- get_pts_index (graph, gr_cols, vert_map, pts)
        if (length (pts == 2) & is.numeric (pts) &
            ( (any (grepl ("x", names (pts), ignore.case = TRUE)) &
             any (grepl ("y", names (pts), ignore.case = TRUE))) |
             (any (grepl ("lon", names (pts), ignore.case = TRUE) &
                   (any (grepl ("lat", names (pts), ignore.case = TRUE)))))))
            names (pts) <- NULL
        id <- get_id_cols (pts)
        if (is.null (id))
            id <- vert_map$vert [index] # from_index is 1-based
    }
    list (index = index, id = id)
}


#' get_id_cols
#'
#' Get the ID columns or rownames from a matrix or data.frame of from or to
#' points
#'
#' @param pts The `from` or `to` args passed to `move_dists`
#' @return Character vector of names of points, if they exist in `pts`
#' @noRd
get_id_cols <- function (pts)
{
    ids <- NULL
    if (any (grepl ("id", colnames (pts), ignore.case = TRUE)))
    {
        nmc <- which (grepl ("id", colnames (pts)))
        if (is.data.frame (pts))
            ids <- pts [[nmc]]
        else if (is.matrix (pts))
            ids <- pts [, nmc, drop = TRUE]
    } else if (is.vector (pts) & !is.null (names (pts)))
        ids <- names (pts)
    else if (!is.null (rownames (pts)))
        ids <- rownames (pts)
    return (ids)
}

#' make_vert_map
#'
#' Map unique vertex names to sequential numbers in matrix
#' @noRd
make_vert_map <- function (graph, gr_cols)
{
    # gr_cols are (edge_id, from, to, d, w, component, xfr, yfr, xto, yto)
    verts <- c (paste0 (graph [[gr_cols [2] ]]),
                paste0 (graph [[gr_cols [3] ]]))
    indx <- which (!duplicated (verts))
    # Note id has to be 0-indexed:
    data.frame (vert = paste0 (verts [indx]), id = seq (indx) - 1,
                stringsAsFactors = FALSE)
}

#' get_pts_index
#'
#' Convert `from` or `to` args of `move_dists` to indices into
#' `vert_map`
#'
#' @param graph A dodgr graph
#' @param vert_map Two-column `data.frame` of unique vertices and
#' corresponding IDs, obtained from `make_vert_map`
#' @param gr_cols Returned from `dodgr_graph_cols()`
#' @param pts Either a vector of names, or a matrix or `data.frame` of
#' arbitrary geographical coordinates for which to get index into vertices of
#' graph.
#'
#' @noRd
get_pts_index <- function (graph, gr_cols, vert_map, pts)
{
    if (!(is.matrix (pts) | is.data.frame (pts)))
    {
        if (!is.numeric (pts))
            pts <- matrix (pts, ncol = 1)
        else
        {
            nms <- names (pts)
            if (is.null (nms))
                nms <- c ("x", "y")
            pts <- matrix (pts, nrow = 1) # vector of (x,y) vals
            colnames (pts) <- nms
        }
    }

    if (ncol (pts) == 1)
    {
        pts <- pts [, 1]
        if (!is.numeric (pts))
        {
            indx <- match (pts, vert_map$vert)
            if (any (is.na (indx)))
                stop (paste0 ("from/to are not numeric yet can not be",
                              " matched onto graph vertices"))
            pts <- indx
        }
        if (any (pts < 1 | pts > nrow (vert_map)))
            stop (paste0 ("points exceed numbers of vertices"))
    } else
    {
        nms <- names (pts)
        if (is.null (nms))
            nms <- colnames (pts)
        ix <- which (grepl ("x", nms, ignore.case = TRUE) |
                     grepl ("lon", nms, ignore.case = TRUE))
        iy <- which (grepl ("y", nms, ignore.case = TRUE) |
                     grepl ("lat", nms, ignore.case = TRUE))
        if (length (ix) != 1 | length (iy) != 1)
            stop (paste0 ("Unable to determine geographical ",
                          "coordinates in from/to"))

        # gr_cols are (edge_id, from, to, d, w, xfr, yfr, xto, yto, component
        if (any (is.na (gr_cols [6:9])))
            stop (paste0 ("Cannot determine geographical coordinates ",
                          "against which to match pts"))

        if (is.data.frame (pts))
        {
            names (pts) [ix] <- "x"
            names (pts) [iy] <- "y"
        } else
        {
            colnames (pts) [ix] <- "x"
            colnames (pts) [iy] <- "y"
        }

        # Result of rcpp_points_index is 0-indexed for C++
        pts <- rcpp_points_index_par (dodgr::dodgr_vertices (graph), pts) + 1
        # xy has same order as vert_map
    }

    pts
}

#' flip_graph
#'
#' Flip from and two vertices of a graph
#' @noRd
flip_graph <- function (graph)
{
    fr_cols <- c ("from_id", "from_lon", "from_lat")
    fr_cols <- fr_cols [which (fr_cols %in% names (graph))]
    to_cols <- c ("to_id", "to_lon", "to_lat")
    to_cols <- to_cols [which (to_cols %in% names (graph))]
    fr_temp <- graph [, fr_cols]
    graph [, fr_cols] <- graph [, to_cols]
    graph [, to_cols] <- fr_temp
    return (graph)
}
