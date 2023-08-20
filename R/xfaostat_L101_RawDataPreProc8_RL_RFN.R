# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_xfaostat_L101_RawDataPreProc8_RL_RFN
#'
#' Preprocess raw faostat data part 8 land and fertilizer data
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
module_xfaostat_L101_RawDataPreProc8_RL_RFN <- function(command, ...) {

  MODULE_INPUTS <-
    c(OPTIONAL_FILE = "aglu/FAO/FAOSTAT/Inputs_LandUse_E_All_Data_(Normalized)_PalceHolder",
      OPTIONAL_FILE = "aglu/FAO/FAOSTAT/Inputs_FertilizersNutrient_E_All_Data_(Normalized)_PalceHolder")

  MODULE_OUTPUTS <-
    c("RL",                # Land
      "RFN")               # Fertilizer

  if(command == driver.DECLARE_INPUTS) {
    return(MODULE_INPUTS)
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(MODULE_OUTPUTS)
  } else if(command == driver.MAKE) {

    year <- value <- Year <- Value <- FAO_country <- iso <- NULL    # silence package check.
    element_code <- element <- area_code <- item_code <- area <- item <- unit <- NULL

    all_data <- list(...)[[1]]

    Curr_Envir <- environment()


    if(Process_Raw_FAO_Data == FALSE) {

      # Prebuilt data is read here ----
      RL <- extract_prebuilt_data("RL")
      RFN <- extract_prebuilt_data("RFN")


    } else {

      # Load required inputs ----
      get_data_list(all_data, MODULE_INPUTS, strip_attributes = TRUE)

      FAOSTAT_load_raw_data(DATASETCODE = "RL", DATA_FOLDER = DIR_RAW_DATA_FAOSTAT, .Envir = Curr_Envir)

      RL %>%
        filter(year %in% FAOSTAT_Hist_Year,
               area_code < 350,       # Rm aggregated area
               item_code %in% c(6621, 6630, 6640)) ->  # Keep Arable land, Temporary crops, Fallow land
        RL


      ### output RL ----
      RL %>%
        add_title("FAO land data") %>%
        add_units("ha") %>%
        add_comments("FAO raw land data") %>%
        add_precursors("aglu/FAO/FAOSTAT/Inputs_LandUse_E_All_Data_(Normalized)_PalceHolder") ->
        RL

      verify_identical_prebuilt(RL)



      # RFN ----
      FAOSTAT_load_raw_data(DATASETCODE = "RFN", DATA_FOLDER = DIR_RAW_DATA_FAOSTAT, .Envir = Curr_Envir)

      RFN %>%
        filter(year %in% FAOSTAT_Hist_Year,
               element_code %in% c(5510, 5157),  # Prod and Ag use
               area_code < 350,                  # Rm aggregated area
               item_code %in% c(3102)) ->       # Nutrient nitrogen N (total)
        RFN

      ### output RFN ----
      RFN %>%
        add_title("FAO fertilizer data") %>%
        add_units("t N") %>%
        add_comments("FAO raw fertilizer data") %>%
        add_precursors("aglu/FAO/FAOSTAT/Inputs_FertilizersNutrient_E_All_Data_(Normalized)_PalceHolder") ->
        RFN

      verify_identical_prebuilt(RFN)
    }


    return_data(MODULE_OUTPUTS)

  } else {
    stop("Unknown command")
  }
}
