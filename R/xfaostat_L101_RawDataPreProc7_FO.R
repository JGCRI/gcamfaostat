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
    c(FAOSTAT_FILE = file.path(DIR_RAW_DATA_FAOSTAT, "Forestry_E_All_Data_Normalized"))

  MODULE_OUTPUTS <-
    c("FO_RoundwoodProducts")       # Forestry data

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
      FO_RoundwoodProducts <- extract_prebuilt_data("FO_RoundwoodProducts")


    } else {

      # Load required inputs ----
      get_data_list(all_data, MODULE_INPUTS, strip_attributes = TRUE)

      FAOSTAT_load_raw_data(DATASETCODE = "FO", .Envir = Curr_Envir)

      FO %>% filter(year >= min(FAOSTAT_Hist_Year),
                    area_code < 350,     # Rm aggregated area
                    item_code %in% c(1861, 1864, 1865, 2038, 1868, 1871,
                                     1634, 1873, 1872, 1875, 1876)) %>%
      # see meta data in https://www.fao.org/faostat/en/#data/FO
      # 1861	Roundwood
      # 1864	Wood fuel
      # 1865	Industrial roundwood
      # 2038	Pulpwood, round and split, all species (production)
      # 1868	Sawlogs and veneer logs
      # 1871	Other industrial roundwood
      # 1634	Veneer sheets
      # 1873	Wood-based panels
      # 1872	Sawnwood
      # 1875	Wood pulp
      # 1876  Paper and paperboard
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
        FO_RoundwoodProducts


      ### output FO ----
      FO_RoundwoodProducts %>%
        add_title("FAO forestry data") %>%
        add_units("m3") %>%
        add_comments("FAO raw forestry data and main products") %>%
        add_precursors(file.path(DIR_RAW_DATA_FAOSTAT, "Forestry_E_All_Data_Normalized")) ->
        FO_RoundwoodProducts


      verify_identical_prebuilt(FO_RoundwoodProducts)

    }

    return_data(MODULE_OUTPUTS)

  } else {
    stop("Unknown command")
  }
}

# developer

# FO %>% distinct(element, unit)
# FO %>% filter(area == "World", year == 2015, element == "Production", unit %in% c("t", "m3")) %>%
#   mutate(value = value / 10^9)  %>% write.csv("CheckItems_FO.csv")
