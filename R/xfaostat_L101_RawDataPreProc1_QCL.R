# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_xfaostat_L101_RawDataPreProc1_QCL
#'
#' Preprocess raw faostat data part 1 QCL data
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs, a vector of output names, or (if
#'   \code{command} is "MAKE") all the generated outputs
#' @details This chunk compiles balanced supply utilization data in primary equivalent in GCAM region and commodities.
#' @importFrom assertthat assert_that
#' @importFrom dplyr summarize bind_rows filter if_else inner_join left_join mutate rename select n group_by_at
#' first case_when vars
#' @importFrom tibble tibble
#' @importFrom tidyr complete drop_na gather nesting spread replace_na
#' @author XZ 2023
module_xfaostat_L101_RawDataPreProc1_QCL <- function(command, ...) {

  MODULE_INPUTS <-
    c(FAOSTAT_FILE = file.path(DIR_RAW_DATA_FAOSTAT, "Production_Crops_Livestock_E_All_Data_Normalized"))

  MODULE_OUTPUTS <-
    c("QCL_wide",          # Ag production quantity and harvested area
      "QCL_area_code_map"  # Country code
    )

  if(command == driver.DECLARE_INPUTS) {
    return(MODULE_INPUTS)
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(MODULE_OUTPUTS)
  } else if(command == driver.MAKE) {

    year <- value <- Year <- Value <- FAO_country <- iso <- NULL    # silence package check.
    QCL <- element_code <- element <- area_code <- item_code <- area <-
      item <- unit <-  NULL

    all_data <- list(...)[[1]]

    Curr_Envir <- environment()


    if(Process_Raw_FAO_Data == FALSE) {

    # Load from Prebuilt data ----
      QCL_wide <- extract_prebuilt_data("QCL_wide")
      QCL_area_code_map <- extract_prebuilt_data("QCL_area_code_map")

      } else {

    # Load required inputs ----
    get_data_list(all_data, MODULE_INPUTS, strip_attributes = TRUE)


    # *[QCL] FAOSTAT Production and area ----

    ## Load raw data
    FAOSTAT_load_raw_data(DATASETCODE = "QCL", .Envir = Curr_Envir)

    #FAOSTAT_load_raw_data("QCL", GET_MAPPINGCODE = "AreaCodes", .Envir = Curr_Envir)
    #FAOSTAT_load_raw_data("QCL", GET_MAPPINGCODE = "ItemCodes", .Envir = Curr_Envir)

    QCL %>% distinct(element_code, element)

    QCL %>%
      filter(year >= min(FAOSTAT_Hist_Year)) %>%
      # Remove aggregated areas and items
      filter(area_code < 350, item_code < 1700) %>%
      select(area_code,
             area,
             item_code,
             item,
             element_code,
             element,
             year,
             value,
             unit) %>%
      # When dealing with animal/livestock data, units are important
      # Prod Popultn (5314) for Beewax and honey is removed since data is only before 1990
      filter(element_code != 5314) %>%
      # Remove NA for simplicity for now; expend.grid later
      # All Coir (coconut fiber; item_code == 813) was previously filtered out due to NA, but now available for a few regions
      filter(!is.na(value)) %>%
      # remove accent
      rm_accent("item", "area") -> QCL1

    QCL1 %>% spread(year, value) ->
      QCL_wide


    ### output QCL_area_code_map and QCL ----
    # Other data uses OCL area for consistency
    QCL_wide %>%
      distinct(area_code, area) %>%
      add_title("FAO primary production country and code") %>%
      add_units("NA") %>%
      add_comments("FAO Country and code") %>%
      add_precursors(file.path(DIR_RAW_DATA_FAOSTAT, "Production_Crops_Livestock_E_All_Data_Normalized")) ->
      QCL_area_code_map

    QCL_wide %>%
      add_title("FAO primary production (QCL)", overwrite = TRUE) %>%
      add_units("ha/tonne") %>%
      add_comments("Preprocessed FAOSTAT primary production") %>%
      add_precursors(file.path(DIR_RAW_DATA_FAOSTAT, "Production_Crops_Livestock_E_All_Data_Normalized")) ->
      QCL_wide

      verify_identical_prebuilt(QCL_area_code_map)
      verify_identical_prebuilt(QCL_wide)

      }

    return_data(MODULE_OUTPUTS)

  } else {
    stop("Unknown command")
  }
}
