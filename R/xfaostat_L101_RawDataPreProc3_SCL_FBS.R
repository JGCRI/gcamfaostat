# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_xfaostat_L101_RawDataPreProc3_SCL_FBS
#'
#' Preprocess raw faostat data part 3 SCL and FBS
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
module_xfaostat_L101_RawDataPreProc3_SCL_FBS <- function(command, ...) {

  MODULE_INPUTS <-
    c("QCL_area_code_map")

  MODULE_OUTPUTS <-
    c("SCL",              # Supply utilization accounting
      "FBS")              # New food balance sheet


  if(command == driver.DECLARE_INPUTS) {
    return(MODULE_INPUTS)
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(MODULE_OUTPUTS)
  } else if(command == driver.MAKE) {

    year <- value <- Year <- Value <- FAO_country <- iso <- NULL    # silence package check.

    all_data <- list(...)[[1]]

    # Load required inputs ----

    get_data_list(all_data, MODULE_INPUTS, strip_attributes = TRUE)

    # Get area code ----
    QCL_area_code <- QCL_area_code_map %>% distinct(area_code) %>% pull()

    ## *[SCL] SUA: supply utilization accounting ----

    FAOSTAT_load_raw_data("SCL")   # SUA      2010+

    SCL %>% distinct(element, element_code, unit)


    if (is.numeric(SCL$item_code)) {
      SCL %>% filter(item_code <= 1700, item_code != 1) -> SCL
    }

    SCL %>% filter(!element_code %in% c(664, 665, 674, 684, 511),
                   # it is not useful to calculate cal/g using `Food supply (kcal/capita/day)` /`Food supply quantity (g/capita/day)`
                   # unit too small so remove them here
                   # `Calories/Year` / `Food supply quantity (tonnes)` is more accurate!
                   # similarly for protein and fat
                   # Use annual value in SUA to calculate the conversion rate!
                   area_code %in% QCL_area_code) %>%
      select(area_code, area, item_code, item, element_code, element, year, value, unit) %>%
      rm_accent("item", "area") -> SCL1


     ### output SCL----
    SCL1 %>%
       add_title("FAO SCL") %>%
       add_units("tonne") %>%
       add_comments("Preprocessed FAOSTAT SCL") ->
      SCL
     rm(SCL1)


     # Food balance and Supply-Utilization-Account

     ## *[FBS] new food balance sheet (2010-) ----

     ## Load raw data
     FAOSTAT_load_raw_data("FBS") # New FBS  2010+
     FBS %>% distinct(element, element_code, unit)

     FBS %>% filter(item_code < 2901, item_code != 2501,
                    !element_code %in% c(511, 5301),
                    area_code %in% QCL_area_code) %>%
       select(area_code, area, item_code, item, element_code, element, year, value, unit) %>%
       rm_accent("item", "area") -> FBS1


     ### output FBS ----
     FBS1 %>%
       add_title("FAO SCL") %>%
       add_units("tonne") %>%
       add_comments("Preprocessed FAOSTAT SCL") ->
       FBS


    return_data(MODULE_OUTPUTS)

  } else {
    stop("Unknown command")
  }
}
