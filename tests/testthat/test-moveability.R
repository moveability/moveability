context("moveability")

test_all <- (identical (Sys.getenv ("MPADGE_LOCAL"), "true") |
             identical (Sys.getenv ("TRAVIS"), "true"))

test_that("moveability checks", {
              expect_error (m <- moveability (),
                            "streetnet must be provided")

              expect_error (m <- moveability (streetnet = list ()),
                            "green_polys must be provided")
              expect_error (m <- moveability (streetnet = castlemaine,
                                              green_polys = castlemaine_green,
                                              mode = "horse"),
                            "mode must be either foot or bicycle")

              net <- dodgr::weight_streetnet (castlemaine)
              expect_error (m <- move_stats (graph = net),
                            "from must be provided")
              expect_error (m <- move_statistics (graph = net),
                            "from must be provided")
             })

test_that("moveability fn", {
              expect_message (m <- moveability (streetnet = castlemaine,
                                                green_polys = castlemaine_green),
                              "Calculating shortest paths from")
              expect_is (m, "data.frame")
              expect_equal (ncol (m), 8)
              expect_equal (names (m), c ("id", "x", "y", "component",
                                          "n", "m", "hull_area", "green_area"))

              expect_silent (net <- dodgr::weight_streetnet (castlemaine,
                                                             wt_profile = "foot"))
              net <- net [net$component == 1, ]
              expect_message (m2 <- moveability (streetnet = net,
                                                 green_polys = castlemaine_green),
                              "Calculating shortest paths from")
              expect_equal (ncol (m2), ncol (m)) # no component column
              expect_true (nrow (m2) < nrow (m)) # fewer points

              expect_message (m3 <- moveability (streetnet = castlemaine,
                                                 green_polys = castlemaine_green,
                                                 mode = "bicycle"),
                              "Calculating shortest paths from")
              expect_is (m3, "data.frame")
              expect_equal (ncol (m3), 8)
              expect_equal (names (m3), c ("id", "x", "y", "component",
                                           "n", "m", "hull_area", "green_area"))
              expect_true (nrow (m3) < nrow (m))
              # bike moveability should be greater:
              expect_true (mean (m3$m) > mean (m$m))
             })
