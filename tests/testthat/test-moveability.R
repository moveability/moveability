context("moveability")

test_all <- (identical (Sys.getenv ("MPADGE_LOCAL"), "true") |
             identical (Sys.getenv ("TRAVIS"), "true"))

test_that("moveability checks", {
              m <- moveability (streetnet = castlemaine, city = "castlemaine")
              # first message is "city or streetnet must be specified", but
              # `expect_message` matches on last message, so:
              expect_message (m <- moveability (streetnet = castlemaine,
                                                city = "castlemaine"),
                              "Calculating shortest paths from")
              expect_error (m <- moveability (streetnet = list ()),
                            paste0 ("streetnet must be of format osmdata_sc, ",
                                    "or dodgr_streetnet_sc"))
              expect_error (m <- moveability (streetnet = castlemaine,
                                              mode = "horse"),
                            "mode must be either foot or bicycle")

             })

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

              expect_message (m3 <- moveability (streetnet = castlemaine,
                                                 mode = "bicycle"),
                              "Calculating shortest paths from")
              expect_is (m3, "data.frame")
              expect_equal (ncol (m3), 6)
              expect_equal (names (m3), c ("id", "x", "y", "component", "n", "m"))
              expect_true (nrow (m3) < nrow (m))
              # bike moveability should be greater:
              expect_true (mean (m3$m) > mean (m$m))
             })
