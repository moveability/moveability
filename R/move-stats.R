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
#' graph <- dodgr::weight_streetnet (castlemaine)
#' from <- sample (graph$.vx0, size = 100)
#' d <- move_stats (graph, from = from)
#' # d is a `data.frame` of the coordinates of all `from` points and
#' # correponding moveability statisics 
move_stats <- function (graph, from, d_threshold = 1, quiet = FALSE)
{
    if (missing (from))
        stop ("from must be provided")

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
    names (d) <- vert_id

    if (!quiet)
    {
        pt <- paste0 (round ((proc.time () - pt0) [3]))
        message (paste ("done in", pt, "seconds."))
    }

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
