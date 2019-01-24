context("moveability")

test_all <- (identical (Sys.getenv ("MPADGE_LOCAL"), "true") |
             identical (Sys.getenv ("TRAVIS"), "true"))

test_that("moveability fn", {
              m <- moveability (streetnet = dodgr::hampi)
              expect_is (m, "data.frame")
              expect_equal (ncol (m), 6)
              expect_equal (names (m), c ("id", "x", "y", "component", "n", "m"))

              net <- dodgr::weight_streetnet (dodgr::hampi, wt_profile = "foot")
              net <- net [net$component == 1, ]
              net$component <- NULL
              m2 <- moveability (streetnet = net)
             })
