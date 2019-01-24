context("moveability")

test_all <- (identical (Sys.getenv ("MPADGE_LOCAL"), "true") |
             identical (Sys.getenv ("TRAVIS"), "true"))

test_that("moveability fn", {
              expect_message (m <- moveability (streetnet = dodgr::hampi),
                              "The following highway types are present in data")
              expect_is (m, "data.frame")
              expect_equal (ncol (m), 6)
              expect_equal (names (m), c ("id", "x", "y", "component", "n", "m"))

              expect_message (net <- dodgr::weight_streetnet (dodgr::hampi,
                                                              wt_profile = "foot"),
                              "The following highway types are present in data")
              net <- net [net$component == 1, ]
              net$component <- NULL
              expect_message (m2 <- moveability (streetnet = net),
                              "Calculating shortest paths from 169 points")
              expect_true (ncol (m2) < ncol (m)) # no component column
              expect_true (nrow (m2) < nrow (m)) # fewer points
             })

test_that ("moveability to polygons", {
              net <- dodgr::weight_streetnet (dodgr::hampi, wt_profile = "foot")
              net <- net [net$component == 1, ]
              m <- moveability (streetnet = net)
              expect_message (p <- moveability_to_polygons (m = m,
                                                            streetnet = dodgr::hampi),
                              "calculating fundamental cycles")
              expect_is (p, "list")
              expect_length (p, 43)
              expect_true (all (sapply (p, is.data.frame)))
              expect_true (all (sapply (p, ncol) == 4))
              expect_true (all (sapply (p, names) == c ("id", "x", "y", "m")))
             })

test_that ("polygons_to_sf", {
               m <- moveability (streetnet = dodgr::hampi)
               p <- moveability_to_polygons (m = m, streetnet = dodgr::hampi)
               psf <- polygons_to_sf (p)
               expect_is (psf, "sfc_POLYGON")
               expect_equal (length (p), length (psf))
               xy <- lapply (psf, function (i) i [[1]])
               expect_equal (nrow (do.call (rbind, p)),
                             nrow (do.call (rbind, xy)))
               expect_equal (ncol (do.call (rbind, p)), 4)
               expect_equal (ncol (do.call (rbind, xy)), 2)
             })

test_that ("moveability_to_lines", {
               m <- moveability (streetnet = dodgr::hampi)
               expect_silent (l <- moveability_to_lines (m = m,
                                                         streetnet = dodgr::hampi))
               expect_is (l, "list")
               expect_length (l, 2)
               expect_named (l, c ("dat", "geometry"))
               expect_equal (length (l$geometry), nrow (l$dat))
               #expect_is (l$geometry, "sfc_LINESTRING")
               #expect_is (l$geometry, "list")
               expect_is (l$dat, "data.frame")
             })
