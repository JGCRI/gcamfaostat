# Test the module helper utilities

context("module_helpers")

test_that("downscale_FAO_country", {
  # catches bad input
  expect_error(downscale_FAO_country(1, "1", 1993))
  expect_error(downscale_FAO_country(tibble(), 1, 1993))
  expect_error(downscale_FAO_country(tibble(), "1", "1993"))
  expect_error(downscale_FAO_country(tibble(), "1", 1993, years = 2000:2002))

  d <- tibble(item = "item", element = "element", countries = c("x", "y", "z"),
              `1998` = c(0, 0, 3), `1999` = c(0, 0, 2), `2000` = c(1, 2, 0), `2001` = c(2, 1, 0))
  cn <- "z"
  dy <- 2000L
  yrs <- 1998:2001

  dnew <- gcamfaostat:::downscale_FAO_country(d, country_name = cn, dissolution_year = dy, years = yrs)
  expect_equal(ncol(d), ncol(dnew))
  expect_equal(nrow(d) - 1, nrow(dnew))  # post-dissolution country should be gone
  # ungrouped return
  expect_equal(dplyr::groups(dnew), list())

  # Pre-dissolution year should be calculated from ratio of dissolution year data
  expect_equal(dnew[as.character(yrs[yrs < dy])], tibble(`1998` = c(1, 2), `1999` = c(2/3, 4/3)))

  # Post-dissolution year should be untouched
  expect_equal(dnew[as.character(yrs[yrs >= dy])], d[1:2, as.character(yrs[yrs >= dy])])
})

