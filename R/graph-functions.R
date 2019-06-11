null_to_na <- function (x)
{
    if (length (x) == 0)
        x <- NA
    return (x)
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
