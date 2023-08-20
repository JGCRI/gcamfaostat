# Test driver_drake

context("driver_drake")

# Setup
hasdrake <- FALSE
if(require('drake')){
  hasdrake <- TRUE
  test_cache <- new_cache()
}
xml_dir <- paste0(normalizePath(file.path("../..", XML_DIR), winslash = "/"), "/" )

test_that("driver_drake runs with no errors",{
  if(!hasdrake) {
    skip("No drake package - skipping test")
    }
  expect_error(driver_drake(stop_after = c("module_xfaostat_L103_ProducerPrices"), xmldir = xml_dir, cache = test_cache), NA)
})

test_that("catches bad input", {
  if(!hasdrake) {
    skip("No drake package - skipping test")
  }
   expect_error(driver_drake(return_inputs_of = 1))
   expect_error(driver_drake(return_outputs_of = 1))
   expect_error(driver_drake(return_data_names = 1))
   expect_error(driver_drake(return_data_map_only = 1))
   expect_error(driver_drake(return_plan_only = 1))
   expect_error(driver_drake(quiet = 1))
 })

test_that("plan is a dataframe",{
  if(!hasdrake) {
    skip("No drake package - skipping test")
  }
   plan <- driver_drake(stop_before = c("module_xfaostat_L103_ProducerPrices"), return_plan_only = TRUE)
   expect_is(plan, "data.frame")
})

test_that("load_from_cache works", {
  if(!hasdrake) {
    skip("No drake package - skipping test")
  }
   data <- load_from_cache(outputs_of("module_xfaostat_L103_ProducerPrices"))

   expect_type(data, "list")

})

