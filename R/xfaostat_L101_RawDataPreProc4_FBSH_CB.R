# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_xfaostat_L101_RawDataPreProc4_FBSH_CB
#'
#' Preprocess raw faostat data part 4 FBSH and CB
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
module_xfaostat_L101_RawDataPreProc4_FBSH_CB <- function(command, ...) {

  MODULE_INPUTS <-
    c(OPTIONAL_FILE = "aglu/FAO/FAOSTAT/FoodBalanceSheetsHistoric_E_All_Data_(Normalized)_PalceHolder",
      OPTIONAL_FILE = "aglu/FAO/FAOSTAT/CommodityBalances_(non-food)_E_All_Data_(Normalized)_PalceHolder",
      "QCL_area_code_map")

  MODULE_OUTPUTS <-
    c(#"FBSH",                 # Old food balance sheet
      #"CB",                   # Old non food utilization accounting
      "FBSH_CB_wide")          # Combined FBSH and CB


  if(command == driver.DECLARE_INPUTS) {
    return(MODULE_INPUTS)
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(MODULE_OUTPUTS)
  } else if(command == driver.MAKE) {

    year <- value <- Year <- Value <- FAO_country <- iso <- NULL    # silence package check.

    all_data <- list(...)[[1]]

    Curr_Envir <- environment()



    if(Process_Raw_FAO_Data == FALSE) {

      # Prebuilt data is read here ----
      FBSH_CB_wide <- extract_prebuilt_data("FBSH_CB_wide")

    } else {

    # Load required inputs ----
    get_data_list(all_data, MODULE_INPUTS, strip_attributes = TRUE)

    ## Get area code ----
    QCL_area_code <-
      QCL_area_code_map %>% distinct(area_code) %>% pull()


    # Food balance and Supply-Utilization-Account

    ## *[FBSH] old food balance sheet (-2013) ----

    FAOSTAT_load_raw_data("FBSH", .Envir = Curr_Envir)  # Old FBS -2013
    FBSH %>% distinct(element, element_code, unit)
    # Keep population (old)
    FBSH %>%
      filter(item_code < 2901,
             year >= min(FAOSTAT_Hist_Year_FBSH),
             !element_code %in% c(5301),
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
      rm_accent("item", "area") -> FBSH1

    ## *[CB] Non-food Balance ----

    FAOSTAT_load_raw_data("CB", .Envir = Curr_Envir)    # Old FBS-nonfood -2013

    CB %>% distinct(element, element_code, unit)
    # Keep population (old)
    CB %>% filter(item_code < 2901,
                  year >= min(FAOSTAT_Hist_Year_FBSH),
                  !element_code %in% c(5300),
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
      rm_accent("item", "area") %>%
      mutate(value = value / 1000,
             unit = "1000 tonnes") -> CB1 # convert to Kton


    ## *FBSH_CB merge the two----
    # load processed data

    FBSH1 %>% distinct(item_code) %>%
      dplyr::intersect(CB %>% distinct(item_code)) %>%
      pull ->
      dup_item_code

    FBSH1 %>% distinct(element, element_code) %>%
      filter(!element_code %in% c(645, 664, 674, 684, 511)) %>%  # remove non-balance items
      mutate(element = gsub(
        " Quantity| supply quantity \\(tonnes\\)| \\(non-food\\)",
        "",
        element
      )) %>%
      mutate(
        element = replace(element, element == "Losses", "Loss"),
        element = replace(element, element == "Processing", "Processed")
      ) -> element_code_map

    FBSH1 %>%
      bind_rows(CB1 %>% filter(!item_code %in% dup_item_code)) %>%
      filter(!element_code %in% c(645, 664, 674, 684, 511)) %>%  # remove non-balance items
      mutate(element = gsub(
        " Quantity| supply quantity \\(tonnes\\)| \\(non-food\\)",
        "",
        element
      )) %>%
      mutate(
        element = replace(element, element == "Losses", "Loss"),
        element = replace(element, element == "Processing", "Processed")
      ) %>%
      # remove element code since FBSH and CB have different ones
      select(-element_code) %>%
      # but adding back the FBSH one since FBS and FBSH have the same one
      left_join(element_code_map,
                by = "element") ->
      FBSH_CB

    # Rice and products and Groundnuts related adjustments
    # to make FBS and FBSH more consistent
    SHELL_RATE_groundnuts <- 0.7
    Mill_RATE_rice <- 0.667

    FBSH_CB %>% distinct(element, element_code, unit)
    FBSH_CB %>%
      filter(!item_code %in% c(2805, 2556)) %>%
      # Adjust to Rice and products and Groundnuts in FBS and bind
      bind_rows(
        FBSH_CB %>% filter(item_code %in%  c(2805)) %>%
          mutate(
            item = "Rice and products",
            item_code = 2807,
            value = value / Mill_RATE_rice
          ) %>%
          bind_rows(
            FBSH_CB %>% filter(item_code %in%  c(2556)) %>%
              mutate(
                item = "Groundnuts",
                item_code = 2552,
                value = value / SHELL_RATE_groundnuts
              )
          )
      ) ->
      FBSH_CB1

    FBSH_CB1 %>% spread(year, value) ->
      FBSH_CB_wide

    ### output FBSH_CB and clean memory ----

    FBSH_CB_wide %>%
      add_title("FAO FBSH and CB, food and commodity balance before 2013, wide", overwrite = T) %>%
      add_units("1000 tonne") %>%
      add_comments("Preprocessed FAO FBSH_CB") %>%
      add_precursors("aglu/FAO/FAOSTAT/FoodBalanceSheetsHistoric_E_All_Data_(Normalized)_PalceHolder",
                     "aglu/FAO/FAOSTAT/CommodityBalances_(non-food)_E_All_Data_(Normalized)_PalceHolder",
                     "QCL_area_code_map") ->
      FBSH_CB_wide

    verify_identical_prebuilt(FBSH_CB_wide)
    }


    return_data(MODULE_OUTPUTS)

  } else {
    stop("Unknown command")
  }
}
