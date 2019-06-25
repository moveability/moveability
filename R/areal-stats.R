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
    if (!quiet) message ("-----sustenance (1/6)----")
    sustenance_shop <- get_one_kv (city, key = "shop")
    value <- c ("bar", "bbq", "biergarten", "cafe", "drinking_water",
                "fast_food", "food_court", "ice_cream", "pub", "restuarant")
    sustenance <- get_one_kv (city, key = "amenity", value = value)

    if (!quiet) message ("-----education (2/6)----")
    value <- c ("college", "kingergarten", "library", "public_bookcase",
                "school", "music_school", "driving_school", "language_school",
                "university", "research_institute")
    education <- get_one_kv (city, key = "amenity", value = value)

    if (!quiet) message ("-----transportation (3/6)----")
    value <- c ("bicycle_parking", "bicycle_repair_station", "bicycle_rental",
                "boat_rental", "boat_sharing", "buggy_parking", "bus_station",
                "car_rental", "car_sharing", "car_wash", "vehicle_inspection",
                "charging_station", "ferry_terminal", "fuel", "grit_bin",
                "motorcycle_parking", "parking", "parking_entrance",
                "parking_space", "taxi", "ticket_validator")
    transportation <- get_one_kv (city, key = "amenity", value = value)

    if (!quiet) message ("-----healthcare (4/6)----")
    value <- c ("baby_hatch", "clinic", "dentist", "doctors", "hospital",
                "nursing_home", "pharmacy", "social_facility", "veterinary")
    healthcare <- get_one_kv (city, key = "amenity", value = value)

    if (!quiet) message ("-----entertainment (5/6)----")
    value <- c ("arts_centre", "casino", "cinema", "community_centre",
                "fountain", "gambling", "music_venue", "nightclub",
                "planetarium", "social_centre", "studio", "theatre")
    entertainment <- get_one_kv (city, key = "amenity", value = value)

    if (!quiet) message ("-----other (6/6)----")
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
    other <- get_one_kv (city, key = "amenity", value = value)
    # TODO: some of these last ones have alternative non-amenity keys
    # https://wiki.openstreetmap.org/wiki/Key:amenity

    dplyr::bind_rows (insert_category_column (sustenance_shop, "sustenance"),
                      insert_category_column (sustenance, "sustenance"),
                      insert_category_column (education, "education"),
                      insert_category_column (transportation, "transportation"),
                      insert_category_column (healthcare, "healthcare"),
                      insert_category_column (entertainment, "entertainment"),
                      insert_category_column (other, "other"))
}

get_one_kv <- function (city, key, value = NULL)
{

    if (is.null (value))
        temp <- osmdata::opq (city) %>%
            osmdata::add_osm_feature (key = key) %>%
            osmdata::osmdata_sf (quiet = TRUE)
    else
        temp <- osmdata::opq (city) %>%
            osmdata::add_osm_feature (key = key, value = value) %>%
            osmdata::osmdata_sf (quiet = TRUE)

    xy_point <- NULL
    if (!is.null (temp$osm_points) & nrow (temp$osm_points) > 0)
    {
        index <- which (!is.na (temp$osm_points [[key]]))
        if (length (index) > 0)
        {
            temp$osm_points <- temp$osm_points [index, ]
            xy_point <- sf::st_coordinates (temp$osm_points)
        }
    }

    xy_poly <- NULL

    if (!is.null (temp$osm_polygons))
    {
        index <- which (!is.na (temp$osm_polygons [[key]]))
        if (length (index) > 0)
        {
            temp$osm_polygons <- temp$osm_polygons [index, ]
            suppressWarnings ({
                  xy_poly <- sf::st_centroid (temp$osm_polygons$geometry) %>%
                      sf::st_coordinates () })
            # car parking capacity:
            if ("parking" %in% value)
            {
                capacity <- NULL
                p_index <- which (temp$osm_polygons$amenity == "parking")
                if ("capacity" %in% names (temp$osm_polygons))
                    suppressWarnings (
                        capacity <- as.numeric (temp$osm_polygons$capacity))

                area <- sf::st_area (temp$osm_polygons$geometry)
                # cars per acre estimates vary between 80 and 130, with most
                # suggesting 80-100 in practice. Take 80 as a base value.
                # 1 acre = 4047 m^2, so density is 80 / 4047 = 0.01976773 cars
                # per square metre
                areal_capacity <- as.numeric (floor (area * 80 / 4047))
                areal_capacity [areal_capacity == 0] <- 1
                if (!is.null (capacity))
                {
                    index <- p_index [is.na (temp$osm_polygons$capacity [p_index])]
                    temp$osm_polygons$capacity [index] <- areal_capacity [index]
                } else
                    temp$osm_polygons$capacity [p_index] <-
                        areal_capacity [p_index]
            }
        }
    }

    res <- data.frame (id = c (temp$osm_points$osm_id,
                               temp$osm_polygons$osm_id),
                       name = c (temp$osm_points$name,
                                 temp$osm_polygons$name),
                       stringsAsFactors = FALSE)
    res [[key]] <- c (temp$osm_points [[key]],
                      temp$osm_polygons [[key]])
    if ("parking" %in% value)
        res$capacity <- c (rep (NA_integer_, nrow (temp$osm_points)),
                           temp$osm_polygons$capacity)

    res$x = c (xy_point [, 1], xy_poly [, 1])
    res$y = c (xy_point [, 2], xy_poly [, 2])

    if (nrow (res) == 0) res <- NULL

    return (res)
}

insert_category_column <- function (x, category_name)
{
    i <- which (names (x) == "x")
    data.frame (x [, 1:(i - 1)],
                "category" = rep (category_name, nrow (x)),
                x [, i:ncol (x)],
                stringsAsFactors = FALSE)
}
