test_that("helper function work", {

  Curr_Envir <- environment()


  expect_type(gcamfaostat_metadata(OnlyReturnDatasetCostRequired = T), "character")
  expect_type(DIR_RAW_DATA_FAOSTAT, "character")


  expect_type(FAOSTAT_metadata(), "list")
  expect_equal(length(FAOSTAT_metadata()), 12)
  expect_equal(nrow(FAOSTAT_metadata("QCL")), 1)

  expect_error(FF_download_FAOSTAT(DATASETCODE = "XXX"))
  expect_error(FF_download_RemoteArchive(DATASETCODE = "XXX"))

  expect_type(PREBUILT_DATA, "list")


  tibble(area_code = c(21, 231),
         area = c("Brazil", "United States of America"),
         element  = "Production",
         item = "Soybeans", item_code = 236,
         `1980` = c(15155804, 48921904),
         `2015` = c(97464936, 106953940),
         unit = "tonnes" ) ->
    QCL_wide
    FF_summary("QCL_wide", .ENVIR = Curr_Envir) -> df
  expect_equal(length(df), 9)
  expect_equal(nrow(df), 1)
  expect_type(df, "list")


})

