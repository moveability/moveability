context("moveability")

test_all <- (identical (Sys.getenv ("MPADGE_LOCAL"), "true") |
             identical (Sys.getenv ("TRAVIS"), "true"))

test_that("moveability fn", {
              expect_message (m <- moveability (streetnet = castlemaine),
                              "Calculating shortest paths from")
              expect_is (m, "data.frame")
              expect_equal (ncol (m), 6)
              expect_equal (names (m), c ("id", "x", "y", "component", "n", "m"))

              expect_silent (net <- dodgr::weight_streetnet (castlemaine,
                                                             wt_profile = "foot"))
              net <- net [net$component == 1, ]
              expect_message (m2 <- moveability (streetnet = net),
                              "Calculating shortest paths from")
              expect_equal (ncol (m2), ncol (m)) # no component column
              expect_true (nrow (m2) < nrow (m)) # fewer points
             })

test_that ("moveability to polygons", {
              net <- dodgr::weight_streetnet (castlemaine, wt_profile = "foot")
              net <- net [net$component == 1, ]
              m <- moveability (streetnet = net)
              expect_message (p <- moveability_to_polygons (m = m,
                                                            streetnet = castlemaine),
                              "calculating fundamental cycles")
              expect_is (p, "sf")
              expect_equal (nrow (p), 423)
              expect_equal (ncol (p), 2)
              expect_identical (names (p), c ("moveability", "geometry"))
             })

test_that ("moveability_to_lines", {
               m <- moveability (streetnet = castlemaine)
               requireNamespace ("sf")
               expect_silent (l <- moveability_to_lines (m = m,
                                                         streetnet = castlemaine))
               expect_is (l, c ("sf", "data.frame"))
               expect_length (l, 18)
               expect_true (all (c ("object_", "edge_id", "flow", "geometry") %in%
                                 names (l)))
             })
