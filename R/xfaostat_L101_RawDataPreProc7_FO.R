# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_xfaostat_L101_RawDataPreProc7_FO
#'
#' Preprocess raw faostat data part 7 forestry data
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
module_xfaostat_L101_RawDataPreProc7_FO <- function(command, ...) {

  MODULE_INPUTS <-
    c(OPTIONAL_FILE = "aglu/FAO/FAOSTAT/Forestry_E_All_Data_(Normalized)_PalceHolder")

  MODULE_OUTPUTS <-
    c("FO_Roundwood")       # Forestry data

  if(command == driver.DECLARE_INPUTS) {
    return(MODULE_INPUTS)
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(MODULE_OUTPUTS)
  } else if(command == driver.MAKE) {

    year <- value <- Year <- Value <- FAO_country <- iso <- NULL    # silence package check.
    FO <- element_code <- element <- area_code <- item_code <- area <-
      item <- unit <- NULL

    all_data <- list(...)[[1]]

    Curr_Envir <- environment()



    if (Process_Raw_FAO_Data == FALSE) {
      # Prebuilt data is read here ----
      FO_Roundwood <- extract_prebuilt_data("FO_Roundwood")


    } else {

      # Load required inputs ----
      get_data_list(all_data, MODULE_INPUTS, strip_attributes = TRUE)

      FAOSTAT_load_raw_data(DATASETCODE = "FO", DATA_FOLDER = DIR_RAW_DATA_FAOSTAT, .Envir = Curr_Envir)

      # Only keep roundwood
      FO %>% filter(year >= min(FAOSTAT_Hist_Year),
                    area_code < 350,     # Rm aggregated area
                    item_code == 1861) %>% # Roundwood
        select(area_code,
               area,
               item_code,
               item,
               element_code,
               element,
               year,
               value,
               unit) %>%
        filter(!is.na(value)) %>%
        rm_accent("item", "area") ->
        FO_Roundwood


      ### output FO ----
      FO_Roundwood %>%
        add_title("FAO forestry data") %>%
        add_units("m3") %>%
        add_comments("FAO raw forestry data") %>%
        add_precursors("aglu/FAO/FAOSTAT/Forestry_E_All_Data_(Normalized)_PalceHolder") ->
        FO_Roundwood


      verify_identical_prebuilt(FO_Roundwood)

    }

    return_data(MODULE_OUTPUTS)

  } else {
    stop("Unknown command")
  }
}
