#' move_stats
#'
#' Calculate vector of moveability statistics for a given input street network.
#'
#' @param graph An `dodgr_streetnet` object
#' @param from Vector of points from which moveability statistics are to be be
#' calculated.
#' @param d_threshold Distance threshold below which moveability statistics are
#' to be aggreagted (in kilometres).
#' @param quiet If `FALSE`, display progress messages on screen.
#' @return Vector of moveability values for each point in `from`, with
#' moveability quantified as `$m`.
#'
#' @export 
#' @examples
#' # A larger example from the included [hampi()] data.
#' graph <- dodgr::weight_streetnet (dodgr::hampi)
#' from <- sample (graph$from_id, size = 100)
#' d <- move_stats (graph, from = from)
#' # d is a `data.frame` of the coordinates of all `from` points and
#' # correponding moveability statisics 
move_stats <- function (graph, from, d_threshold = 1, quiet = FALSE)
{
    if (missing (from))
        stop ("from must be provided")

    d_threshold <- d_threshold * 1000 # convert to metres

    gr_cols <- get_graph_cols (graph)
    vert_map <- make_vert_map (graph, unlist (gr_cols))

    index_id <- get_index_id_cols (graph, gr_cols, vert_map, from)
    from_index <- index_id$index - 1 # 0-based
    from_id <- index_id$id

    graph <- convert_graph (graph, gr_cols)

    if (!quiet)
        message ("Calculating shortest paths from ",
                 format (length (from), big.mark = ","),
                 " points ... ", appendLF = FALSE)

    d <- rcpp_get_sp_dists_par (graph, vert_map, from_index, d_threshold,
                                heap_type = "BHeap")
    names (d) <- from_id

    if (!quiet)
        message ("done.")

    return (d)
}

#' move_statistics
#'
#' Alias for \link{move_stats}
#' @inherit move_stats
#' @export
move_statistics <- function (graph, from, quiet = TRUE)
{
    move_stats (graph, from, quiet = quiet)
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
#' @param pts The `from` or `to` args passed to `move_stats`
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
#' Convert `from` or `to` args of `move_stats` to indices into
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
