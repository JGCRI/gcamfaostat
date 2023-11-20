# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_xfaostat_L101_RawDataPreProc5_TCL
#'
#' Preprocess raw faostat data part 5 Gross Trade
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
module_xfaostat_L101_RawDataPreProc5_TCL <- function(command, ...) {

  MODULE_INPUTS <-
    c(FAOSTAT_FILE = "aglu/FAO/FAOSTAT/Trade_CropsLivestock_E_All_Data_Normalized",
      "QCL_area_code_map")

  MODULE_OUTPUTS <-
    c("TCL_wide")            # Gross trade


  if(command == driver.DECLARE_INPUTS) {
    return(MODULE_INPUTS)
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(MODULE_OUTPUTS)
  } else if(command == driver.MAKE) {

    year <- value <- Year <- Value <- FAO_country <- iso <- NULL    # silence package check.
    QCL_area_code_map <- element_code <- element <- area_code <- item_code <- area <-
      item <- unit <- TCL <- NULL

    all_data <- list(...)[[1]]

    Curr_Envir <- environment()



    if(Process_Raw_FAO_Data == FALSE) {

      # Prebuilt data is read here ----
      TCL_wide <- extract_prebuilt_data("TCL_wide")

    } else {

    # Load required inputs ----
    get_data_list(all_data, MODULE_INPUTS, strip_attributes = TRUE)


    QCL_area_code <- QCL_area_code_map %>% distinct(area_code) %>% pull()

    # *[TCL] Gross trade ----
    FAOSTAT_load_raw_data("TCL", .Envir = Curr_Envir)   # Gross trade

    TCL %>% distinct(element, element_code, unit)
    TCL %>% distinct(item, item_code)

    TCL %>%
      filter(item_code <= 1700,
             year >= min(FAOSTAT_Hist_Year_TCL),
             # only keep quantity
             !element_code %in% c(5622 , 5922),
             area_code %in% QCL_area_code) %>%
      select(area_code, area, item_code, item, element_code, element, year, value, unit) %>%
      rm_accent("item", "area") -> TCL1

    TCL1 %>% spread(year, value) -> TCL_wide
    ### output TCL ----
    TCL_wide %>%
      add_title("FAO TCL") %>%
      add_units("tonne") %>%
      add_comments("Preprocessed FAO TCL") %>%
      add_precursors("aglu/FAO/FAOSTAT/Trade_CropsLivestock_E_All_Data_Normalized",
                     "QCL_area_code_map") ->
      TCL_wide

    verify_identical_prebuilt(TCL_wide)

    }


    return_data(MODULE_OUTPUTS)

  } else {
    stop("Unknown command")
  }
}
