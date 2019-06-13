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
}

#' get_attractors
#'
#' Get points of trip attraction
#' @inheritParams get_green_space
#' @export
get_attractors <- function (city, quiet = FALSE)
{
    value <- c ("", "bar", "bbq", "biergarten", "cafe", "drinking_water",
                "fast_food", "food_court", "ice_cream", "pub", "restuarant")
    key <- c ("shop", rep ("amenity", length (value) - 1))
    sustenance <- get_one_kv (city, key = key, value = value, quiet = quiet)

    value <- c ("college", "kingergarten", "library", "public_bookcase",
                "school", "music_school", "driving_school", "language_school",
                "university", "research_institute")
    key <- rep ("amenity", length (value))
    education <- get_one_kv (city, key = key, value = value, quiet = quiet)

    value <- c ("bicycle_parking", "bicycle_repair_station", "bicycle_rental",
                "boat_rental", "boat_sharing", "buggy_parking", "bus_station",
                "car_rental", "car_sharing", "car_wash", "vehicle_inspection",
                "charging_station", "ferry_terminal", "fuel", "grit_bin",
                "motorcycle_parking", "parking", "parking_entrance",
                "parking_space", "taxi", "ticket_validator")
    key <- rep ("amenity", length (value))
    transportation <- get_one_kv (city, key = key, value = value, quiet = quiet)

    value <- c ("baby_hatch", "clinic", "dentist", "doctors", "hospital",
                "nursing_home", "pharmacy", "social_facility", "veterinary")
    key <- rep ("amenity", length (value))
    healthcare <- get_one_kv (city, key = key, value = value, quiet = quiet)

    value <- c ("arts_centre", "brothel", "casino", "cinema",
                "community_centre", "fountain", "gambling", "music_venue",
                "nightclub", "planetarium", "social_centre", "stripclub",
                "studio", "swingerclub", "theatre")
    key <- rep ("amenity", length (value))
    entertainment <- get_one_kv (city, key = key, value = value, quiet = quiet)

    value <- c ("animal_boarding", "animal_shelter", "baking_oven", "bench",
                "clock", "courthouse", "coworking_space", "crematorium",
                "crypt", "dive_centre", "dojo", "embassy", "fire_station",
                "firepit", "game_feeding", "grave_yard", "gym", "hunting_stand",
                "internet_cafe", "kitchen", "kneipp_water_cure", "marketplace",
                "monastery", "photo_booth", "place_of_worship", "police",
                "post_box", "post_depot", "post_office", "prison",
                "public_bath", "public_building", "ranger_station", "recycling",
                "sanitary_dump_station", "sauna", "shelter", "shower", "table",
                "telephone", "toilets", "townhall", "vending_machine",
                "waste_basket", "waste_disposal", "waste_transfer_station",
                "watering_place", "water_point")
    key <- rep ("amenity", length (value))
    other <- get_one_kv (city, key = key, value = value, quiet = quiet)
    # TODO: some of these last ones have alternative non-amenity keys
    # https://wiki.openstreetmap.org/wiki/Key:amenity
}

get_one_kv <- function (city, key, value, quiet = quiet)
{
    keys <- unique (key [!key == ""]) # used below
    temp <- list () # OSM category for these values
    for (i in seq (key))
    {
        if (!quiet)
            message ("-----", key [i], "--", value [i], "-----")
        if (value [i] != "")
            temp [[i]] <- osmdata::opq (city) %>%
                osmdata::add_osm_feature (key = key [i], value = value [i]) %>%
                osmdata::osmdata_sf (quiet = quiet)
        else
            temp [[i]] <- osmdata::opq (city) %>%
                osmdata::add_osm_feature (key = key [i]) %>%
                osmdata::osmdata_sf (quiet = quiet)
    }
    temp <- do.call (c, temp)

    xy_point <- NULL
    if (!is.null (temp$osm_points) & nrow (temp$osm_points) > 0)
    {
        index <- lapply (keys, function (i) !is.na (temp$osm_points [[i]]))
        if (length (index) > 0)
        {
            i <- index [[1]]
            index [[1]] <- NULL
            for (j in index)
                i <- i | j
            index <- which (i)
            temp$osm_points <- temp$osm_points [index, ]
            xy_point <- sf::st_coordinates (temp$osm_points)
        }
    }

    xy_poly <- NULL
    if (!is.null (temp$osm_polygons))
    {
        index <- lapply (keys, function (i) !is.na (temp$osm_polygons [[i]]))
        if (length (index) > 0)
        {
            i <- index [[1]]
            index [[1]] <- NULL
            for (j in index)
                i <- i | j
            index <- which (i)
            temp$osm_polygons <- temp$osm_polygons [index, ]
            suppressWarnings (
                  xy_poly <- sf::st_centroid (temp$osm_polygons$geometry) %>%
                      sf::st_coordinates ())
        }
    }

    res <- data.frame (id = c (temp$osm_points$osm_id,
                               temp$osm_polygons$osm_id),
                       name = c (temp$osm_points$name,
                                 temp$osm_polygons$name),
                       stringsAsFactors = FALSE)
    for (k in keys)
        res [[k]] <- c (temp$osm_points [[k]],
                        temp$osm_polygons [[k]])
    res$x = c (xy_point [, 1], xy_poly [, 1])
    res$y = c (xy_point [, 2], xy_poly [, 2])

    if (nrow (res) == 0) res <- NULL

    return (res)
}
