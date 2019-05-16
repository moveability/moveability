context("moveability")

test_all <- (identical (Sys.getenv ("MPADGE_LOCAL"), "true") |
             identical (Sys.getenv ("TRAVIS"), "true"))

test_that("moveability fn", {
              expect_message (m <- moveability (streetnet = dodgr::hampi),
                              "Calculating shortest paths from")
              expect_is (m, "data.frame")
              expect_equal (ncol (m), 6)
              expect_equal (names (m), c ("id", "x", "y", "component", "n", "m"))

              expect_silent (net <- dodgr::weight_streetnet (dodgr::hampi,
                                                             wt_profile = "foot"))
              net <- net [net$component == 1, ]
              net$component <- NULL
              expect_message (m2 <- moveability (streetnet = net),
                              "Calculating shortest paths from")
              # CRAN dodgr: 167 points; current version: 169 points
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
               requireNamespace ("sf")
               expect_silent (l <- moveability_to_lines (m = m,
                                                         streetnet = dodgr::hampi))
               expect_is (l, c ("sf", "data.frame"))
               expect_length (l, 17)
               expect_true (all (c ("geom_num", "edge_id", "flow", "geometry") %in%
                                 names (l)))
               expect_true (nrow (l) > nrow (dodgr::hampi))
             })
