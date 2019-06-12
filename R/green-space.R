#' get_green_space
#'
#' Get polygons of all green areas for a given location
#' @param city City for which green space polygons are to be extracted.
#' @param quiet If `TRUE`, dump progress information to screen.
#' @return An \pkg{sf}-format `data.frame` of polygons representing all green
#' areas.
#' @export
get_green_space <- function (city, quiet = FALSE)
{
    # nocov start
    key <- c ("leisure", "leisure", "leisure", "leisure", "surface",
              "landuse", "landuse", "landuse", "landuse")
    value <- c ("garden", "park", "nature_reserve", "playground", "grass",
                "forest", "meadow", "recreation_ground", "village_green")
    green <- list ()
    for (i in seq (key))
    {
        if (!quiet)
            message ("-----", key [i], "--", value [i], "-----")
        green [[i]] <- osmdata::opq (city) %>%
            osmdata::add_osm_feature (key = key [i], value = value [i]) %>%
            osmdata::osmdata_sf (quiet = quiet)
    }
    green <- do.call (c, green)

    # Then presume anywhere within any large green area is green, and so use
    # only containing object of any multipolygons. This gets names of all
    # **contained** polygons:
    ids <- lapply (green$osm_multipolygons$geometry,
                   function (i) names (i [[1]]) [-1])
    green$osm_polygons <- green$osm_polygons [!green$osm_polygons$osm_id %in%
                                              ids, ]
    sf::st_sf (geometry = green$osm_polygons$geometry)
    # nocov end
}
