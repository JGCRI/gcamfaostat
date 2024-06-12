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
    c(FAOSTAT_FILE = file.path(DIR_RAW_DATA_FAOSTAT, "SUA_Crops_Livestock_E_All_Data_Normalized"),
      FAOSTAT_FILE = file.path(DIR_RAW_DATA_FAOSTAT, "FoodBalanceSheets_E_All_Data_Normalized"),
      "QCL_area_code_map")

  MODULE_OUTPUTS <-
    c("SCL_wide",              # Supply utilization accounting
      "FBS_wide")              # New food balance sheet


  if(command == driver.DECLARE_INPUTS) {
    return(MODULE_INPUTS)
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(MODULE_OUTPUTS)
  } else if(command == driver.MAKE) {

    year <- value <- Year <- Value <- FAO_country <- iso <- NULL    # silence package check.
    QCL_area_code_map <- element_code <- element <- area_code <- item_code <- area <-
      item <- unit <- SCL_ItemCodes <- cpc_code <- FBS <- NULL

    all_data <- list(...)[[1]]

    Curr_Envir <- environment()


    if(Process_Raw_FAO_Data == FALSE) {

      # Prebuilt data is read here ----
      SCL_wide <- extract_prebuilt_data("SCL_wide")
      FBS_wide <- extract_prebuilt_data("FBS_wide")

    } else {

    # Load required inputs ----
    get_data_list(all_data, MODULE_INPUTS, strip_attributes = TRUE)

    # Get area code ----
    QCL_area_code <-
      QCL_area_code_map %>% distinct(area_code) %>% pull()

    ## *[SCL] SUA: supply utilization accounting ----

    FAOSTAT_load_raw_data("SCL", .Envir = Curr_Envir)   # SUA      2010+

    SCL %>% distinct(element, element_code, unit)


    if (is.character(SCL$item_code)){

      FAOSTAT_load_raw_data("SCL", GET_MAPPINGCODE = "ItemCodes", .Envir = Curr_Envir)

      SCL %>% rename(cpc_code = item_code) %>%
        left_join_error_no_match(
          SCL_ItemCodes %>% distinct(cpc_code, item_code) %>%
            mutate(cpc_code = gsub("^'", "", cpc_code)), by = "cpc_code") %>%
        select(-cpc_code) ->
        SCL

    }

    if (is.numeric(SCL$item_code)){
      SCL %>% filter(item_code <= 1700, item_code != 1) -> SCL
    }

    SCL %>% filter(!element_code %in% c(664, 665, 674, 684, 511),
                   # it is not useful to calculate cal/g using `Food supply (kcal/capita/day)` /`Food supply quantity (g/capita/day)`
                   # unit too small so remove them here
                   # `Calories/Year` / `Food supply quantity (tonnes)` is more accurate!
                   # similarly for protein and fat
                   # Use annual value in SUA to calculate the conversion rate!
                   area_code %in% QCL_area_code) %>%
      select(area_code,
             area,
             item_code,
             item,
             element_code,
             element,
             year,
             value,
             unit) %>%
      rm_accent("item", "area") -> SCL1

    SCL1 %>% spread(year, value) ->
      SCL_wide
    rm(SCL1)

    ### output SCL----
    SCL_wide %>%
      add_title("FAO supply utilization account dataset, 2010+, wide") %>%
      add_units("tonne") %>%
      add_comments("Preprocessed FAOSTAT SCL")  %>%
      add_precursors(file.path(DIR_RAW_DATA_FAOSTAT, "SUA_Crops_Livestock_E_All_Data_Normalized"),
                     "QCL_area_code_map") ->
      SCL_wide

    verify_identical_prebuilt(SCL_wide)


    # Food balance and Supply-Utilization-Account

    ## *[FBS] new food balance sheet (2010-) ----

    ## Load raw data
    FAOSTAT_load_raw_data("FBS", .Envir = Curr_Envir) # New FBS  2010+
    FBS %>% distinct(element, element_code, unit)

    FBS %>% filter(
      item_code < 2901,
      item_code != 2501,!element_code %in% c(511, 5301),
      area_code %in% QCL_area_code
    ) %>%
      select(area_code,
             area,
             item_code,
             item,
             element_code,
             element,
             year,
             value,
             unit) %>%
      rm_accent("item", "area") -> FBS1

    FBS1 %>% spread(year, value) ->
      FBS_wide

    ### output FBS ----
    FBS_wide %>%
      add_title("FAO food balance sheet, 2010-") %>%
      add_units("1000 tonne") %>%
      add_comments("Preprocessed FAOSTAT SCL") %>%
      add_precursors(file.path(DIR_RAW_DATA_FAOSTAT, "FoodBalanceSheets_E_All_Data_Normalized"),
                     "QCL_area_code_map") ->
      FBS_wide

    verify_identical_prebuilt(FBS_wide)

    }

    return_data(MODULE_OUTPUTS)

  } else {
    stop("Unknown command")
  }
}
