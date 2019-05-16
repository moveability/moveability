# all copes from dodgr/R/graph-function.R + dodgr/R/graph-functions-misc.R

null_to_na <- function (x)
{
    if (length (x) == 0)
        x <- NA
    return (x)
}

#' get_graph_cols
#'
#' Identify the essential columns of the graph table (data.frame, tibble,
#' whatever) to be analysed in the C++ routines.
#'
#' @param graph A `data.frame` containing the edges of the graph
#' @return A list of column numbers of `edge_id`, `from`,
#' `to`, `d`, `w`, `time`, `xfr`, `yfr`, `xto`, `yto`, and `component`, some of
#' which may be NA.
#'
#' @noRd
get_graph_cols <- function (graph)
{
    nms <- names (graph)
    component <- grep ("comp", nms) %>% null_to_na ()
    if (methods::is (graph, "dodgr_streetnet") & 
        !methods::is (graph, "dodgr_streetnet_sc") & ncol (graph) >= 11)
    {
        # columns are always identically structured
        edge_id <- which (nms == "edge_id")
        fr_col <- which (nms == "from_id") %>% null_to_na ()
        to_col <- which (nms == "to_id") %>% null_to_na ()
        d_col <- which (nms == "d")
        w_col <- which (nms == "d_weighted")

        xfr <- which (nms == "from_lon")
        if (length (xfr) == 0) xfr <- NA
        yfr <- which (nms == "from_lat")
        if (length (yfr) == 0) yfr <- NA
        xto <- which (nms == "to_lon")
        if (length (xto) == 0) xto <- NA
        yto <- which (nms == "to_lat")
        if (length (yto) == 0) yto <- NA
    } else
    {
        edge_id <- grep ("edge_id|edge_$", nms) %>% null_to_na ()

        d_col <- find_d_col (graph)
        w_col <- find_w_col (graph)
        if (length (w_col) == 0)
            w_col <- d_col

        fr_col <- find_fr_id_col (graph)
        to_col <- find_to_id_col (graph)

        xfr <- yfr <- xto <- yto <- NA
        # TODO: Modify for other complex but non-spatial types of graph
        if (is_graph_spatial (graph))
        {
            spcols <- find_spatial_cols (graph)
            graph <- tbl_to_df (graph)

            if (!(all (apply (graph [, spcols$fr_col], 2, is.numeric)) |
                  all (apply (graph [, spcols$to_tol], 2, is.numeric))))
                stop (paste0 ("graph appears to have non-numeric ",
                              "longitudes and latitudes"))

            xfr <- spcols$fr_col [1]
            yfr <- spcols$fr_col [2]
            xto <- spcols$to_col [1]
            yto <- spcols$to_col [2]
        } else
        {
            if (length (fr_col) != 1 & length (to_col) != 1)
                stop ("Unable to determine from and to columns in graph")
        }
    }

    time_col <- grep ("time", nms)
    if (length (time_col) != 1)
    {
        time_col <- grep ("time$", nms)
        if (length (time_col) != 1)
            time_col <- NA
    }
    timew_col <- grep ("time_w|timew|tw", nms)
    if (length (timew_col) != 1)
    {
        timew_col <- grep ("time_w|timew|^tw", nms)
        if (length (timew_col) != 1)
            timew_col <- NA
    }

    ret <- c (edge_id, fr_col, to_col, d_col, w_col, time_col, timew_col,
              xfr, yfr, xto, yto, component)
    names (ret) <- c ("edge_id", "from", "to", "d", "w", "time", "time_weighted",
                      "xfr", "yfr", "xto", "yto", "component")
    class (ret) <- c (class (ret), "graph_columns")

    # This is passed to many C++ routines, in which case it needs to be
    # converted to a vector (`do.call (c, gr_cols)`), and the R-style 1-indexeso
    # need to be converted to equivalent 0-indexed forms
    return (as.list (ret))
}

#' convert_graph
#'
#' Convert graph to a standard form suitable for submission to C++ routines
#' @noRd
convert_graph <- function (graph, gr_cols)
{
    keep_cols <- c ("edge_id", "from", "to", "d", "w", "time", "time_weighted")
    index <- do.call (c, gr_cols [keep_cols])
    index <- index [!is.na (index)]
    graph <- graph [, index]
    names (graph) <- names (index)

    if ("edge_id" %in% names (graph))
        graph$edge_id <- convert_to_char (graph$edge_id)
    graph$from <- convert_to_char (graph$from)
    graph$to <- convert_to_char (graph$to)

    if (!"time_weighted" %in% names (graph))
        graph$time_weighted <- graph$time

    return (graph)
}

convert_to_char <- function (x)
{
    if (!is.character (x)) x <- paste0 (x)
    return (x)
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



#' is_graph_spatial
#'
#' Is the graph spatial or not?
#' @param graph A `data.frame` of edges
#' @return `TRUE` is `graph` is spatial, otherwise `FALSE`
#' @noRd
is_graph_spatial <- function (graph)
{
    ncol (graph) > 4 &
        (any (grepl ("x$", names (graph) [find_fr_col (graph)],
                     ignore.case = TRUE)) |
         any (grepl ("y$", names (graph) [find_to_col (graph)],
                     ignore.case = TRUE)) |
         any (grepl ("lon", names (graph), ignore.case = TRUE)) |
         any (grepl ("lat", names (graph), ignore.case = TRUE)))
}

#' Get graph columns containing the from vertex
#' "vx0" is silicate vertex
#' @noRd
find_fr_col <- function (graph)
{
    grep ("^fr|^sta|.vx0", names (graph), ignore.case = TRUE)
}

#' Get graph columns containing the to vertex
#' "vx1" is silicate vertex
#' @noRd
find_to_col <- function (graph)
{
    grep ("^to|^sto|.vx1", names (graph), ignore.case = TRUE)
}

#' Get single graph column containing the ID of the from vertex
#' @noRd
find_fr_id_col <- function (graph)
{
    fr_col <- find_fr_col (graph)
    if (is_graph_spatial (graph))
    {
        frx_col <- find_xy_col (graph, fr_col, x = TRUE)
        fry_col <- find_xy_col (graph, fr_col, x = FALSE)
        fr_col <- fr_col [which (!fr_col %in%
                                 c (frx_col, fry_col))]
    }
    if (length (fr_col) != 1)
    {
        fr_col <- fr_col [grep ("id", names (graph) [fr_col]) ]
        if (length (fr_col) != 1)
            stop ("Unable to determine column with ID of from vertices")
    }
    return (fr_col)
}

#' Get single graph column containing the ID of the to vertex
#' @noRd
find_to_id_col <- function (graph)
{
    to_col <- find_to_col (graph)
    if (is_graph_spatial (graph))
    {
        tox_col <- find_xy_col (graph, to_col, x = TRUE)
        toy_col <- find_xy_col (graph, to_col, x = FALSE)
        to_col <- to_col [which (!to_col %in%
                                 c (tox_col, toy_col))]
    }
    if (length (to_col) != 1)
    {
        to_col <- to_col [grep ("id|vx", names (graph) [to_col]) ]
        if (length (to_col) != 1)
            stop ("Unable to determine column with ID of to vertices")
    }
    return (to_col)
}

#' find_xy_col
#'
#' Find columns in graph containing lon and lat coordinates
#' @param indx columns of graph containing either to or from values, so xy
#' columns can be returned separately for each case
#' @noRd
find_xy_col <- function (graph, indx, x = TRUE)
{
    if (x)
    {
        coli <- grep ("x|lon", names (graph) [indx], ignore.case = TRUE)
        if (length (coli) > 1) # silicate
            coli <- grep ("x$", names (graph) [indx], ignore.case = TRUE)
    } else
    {
        coli <- grep ("y|lat", names (graph) [indx], ignore.case = TRUE)
        if (length (coli) > 1) # silicate
            coli <- grep ("y$", names (graph) [indx], ignore.case = TRUE)
    }

    indx [coli]
}

#' find_spatial_cols
#'
#' @return `fr_col` and `to_col` as vectors of 2 values of `x`
#' then `y` coordinates
#'
#' @noRd
find_spatial_cols <- function (graph)
{
    graph <- tbl_to_df (graph)

    fr_col <- find_fr_col (graph)
    to_col <- find_to_col (graph)

    if (length (fr_col) < 2 | length (to_col) < 2)
        stop (paste0 ("Graph appears to be spatial yet unable to ",
                      "extract coordinates."))

    if (length (fr_col) == 3)
    {
        frx_col <- find_xy_col (graph, fr_col, x = TRUE)
        fry_col <- find_xy_col (graph, fr_col, x = FALSE)
        frid_col <- fr_col [which (!fr_col %in% c (frx_col, fry_col))]
        fr_col <- c (frx_col, fry_col)
        xy_fr_id <- graph [, frid_col]
        if (!is.character (xy_fr_id))
            xy_fr_id <- paste0 (xy_fr_id)

        tox_col <- find_xy_col (graph, to_col, x = TRUE)
        toy_col <- find_xy_col (graph, to_col, x = FALSE)
        toid_col <- to_col [which (!to_col %in% c (tox_col, toy_col))]
        to_col <- c (tox_col, toy_col)
        xy_to_id <- graph [, toid_col]
        if (!is.character (xy_to_id))
            xy_to_id <- paste0 (xy_to_id)
    } else # len == 2, so must be only x-y
    {
        if (length (grep ("lon|lat|x|y", names (graph) [fr_col])) != 2)
            stop ("Unable to determine coordinate columns of graph")
        xy_fr_id <- paste0 (graph [, fr_col [1]], "-",
                            graph [, fr_col [2]])
        xy_to_id <- paste0 (graph [, to_col [1]], "-",
                            graph [, to_col [2]])
    }

    list (fr_col = fr_col,
          to_col = to_col,
          xy_id = data.frame (xy_fr_id = xy_fr_id,
                              xy_to_id = xy_to_id,
                              stringsAsFactors = FALSE))
}

find_d_col <- function (graph)
{
    d_col <- which (tolower (substring (names (graph), 1, 1)) == "d" &
                    tolower (substring (names (graph), 1, 2)) != "dz" &
                    tolower (substring (names (graph), 2, 2)) != "w" &
                    tolower (substring (names (graph), 2, 2)) != "_")
    if (length (d_col) != 1)
        stop ("Unable to determine distance column in graph")
    return (d_col)
}

find_w_col <- function (graph)
{
    w_col <- match (c ("w", "wt"), names (graph))
    if (all (is.na (w_col)) | length (w_col) != 1)
        w_col <- grep ("weight", names (graph))
    if (length (w_col) != 1)
        w_col <- which (tolower (substring (names (graph), 1, 2)) == "dw" |
                        tolower (substring (names (graph), 1, 3)) == "d_w")
    if (length (w_col) > 1)
        stop ("Unable to determine weight column in graph")
    return (w_col)
}

