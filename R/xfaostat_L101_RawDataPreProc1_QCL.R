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
    c(FILE = "aglu/AGLU_ctry")

  MODULE_OUTPUTS <-
    c("QCL",              # Ag production quantity and harvested area
      "QCL_area_code_map" # Country code
    )

  if(command == driver.DECLARE_INPUTS) {
    return(MODULE_INPUTS)
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(MODULE_OUTPUTS)
  } else if(command == driver.MAKE) {

    year <- value <- Year <- Value <- FAO_country <- iso <- NULL    # silence package check.

    all_data <- list(...)[[1]]

    # Load required inputs ----

    get_data_list(all_data, MODULE_INPUTS, strip_attributes = TRUE)


    # *[QCL] FAOSTAT Production and area ----

    ## Load raw data
    FAOSTAT_load_raw_data(DATASETCODE = "QCL", DATA_FOLDER = DIR_RAW_DATA_FAOSTAT)


    QCL %>%
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
      # All Coir (coconut fiber) is filtered out due to NA
      filter(!is.na(value)) %>%
      # remove accent
      rm_accent("item", "area") -> QCL1

    ### output QCL_area_code_map ----
    # Other data uses OCL area for consistency
    QCL1 %>%
      distinct(area_code, area) %>%
      add_title("FAO primary production country and code") %>%
      add_units("NA") %>%
      add_comments("FAO Country and code") ->
      QCL_area_code_map

    ### output QCL ----
    QCL1 %>%
      add_title("FAO primary production") %>%
      add_units("USD/tonne") %>%
      add_comments("Preprocessed FAOSTAT primary production") ->
      QCL

    return_data(MODULE_OUTPUTS)

  } else {
    stop("Unknown command")
  }
}
