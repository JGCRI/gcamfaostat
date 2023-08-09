# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_xfaostat_L101_RawDataPreProc2_PP_PD_OA
#'
#' Preprocess raw faostat data part 2 prices deflators and population
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
module_xfaostat_L101_RawDataPreProc2_PP_PD_OA <- function(command, ...) {

  MODULE_INPUTS <-
    c(FILE = "aglu/FAO/FAOSTAT/Other_supplementary/GDP_deflator_Taiwan",
      "QCL_area_code_map")

  MODULE_OUTPUTS <-
    c("PP",               # Producer prices
      "PD",               # GDP deflator
      "OA")               # Population

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


    # *[PP] Producer price ----

    FAOSTAT_load_raw_data(DATASETCODE = "PP", DATA_FOLDER = DIR_RAW_DATA_FAOSTAT)
    # check data
    PP %>% distinct(element, element_code, unit)

    PP %>%
      filter(
        area_code < 350,
        # rm aggregated regions
        item_code < 1700,
        #rm aggregated items
        area_code %in% QCL_area_code,
        # only keep regions with production
        element_code %in% c(5532, 5539)
      ) %>% #keep USD/tonne and index
      rm_accent("item", "area") -> PP1


    # Using index to fill in missing across years
    PP1 %>%
      filter(element_code %in% c(5532, 5539)) %>%
      select(area_code, area, item_code, item, element, year, value) %>%
      # Not completing year and area here
      spread(element, value) %>%
      left_join(
        PP1 %>% filter(element_code %in% c(5532, 5539)) %>%
          select(area_code, area, item_code, item, element, year, value) %>%
          spread(element, value) %>%
          rename(pp_base = `Producer Price (USD/tonne)`,
                 pp_baseindex = `Producer Price Index (2014-2016 = 100)`) %>%
          filter(!is.na(pp_base)) %>%
          group_by(area, area_code, item) %>%
          filter(year == 2015) %>% within(rm(year)) %>%
          ungroup(),
        by = c("area_code", "area", "item_code", "item")
      ) %>% mutate(
        `Producer Price (USD/tonne)` = if_else(
          is.na(`Producer Price (USD/tonne)`),
          pp_base * `Producer Price Index (2014-2016 = 100)` /
            pp_baseindex,
          `Producer Price (USD/tonne)`
        )
      )  %>%
      select(area_code,
             area,
             item_code,
             item,
             year,
             `Producer Price (USD/tonne)`) %>%
      gather(element, value, `Producer Price (USD/tonne)`) %>%
      mutate(element_code = 5532) -> PP2


    ### output PP ----
    PP2 %>%
      add_title("FAO producer prices") %>%
      add_units("USD/tonne") %>%
      add_comments("Preprocessed FAOSTAT producer prices") %>%
      add_precursors("QCL_area_code_map") ->
      PP


    # [PD] FAO_GDP_deflators ----
    #**************************************

    FAOSTAT_load_raw_data(DATASETCODE = "PD", DATA_FOLDER = DIR_RAW_DATA_FAOSTAT)
    # read in Taiwan values as FAO does not have Taiwan price data
    # GDP_deflator_Taiwan

    PD %>% distinct(element, element_code, unit)
    PD %>% distinct(item, item_code)

    PD %>% filter(
      year %in% FAOSTAT_Hist_Year,
      area_code < 350,
      area_code %in% QCL_area_code,
      # only keep regions with production
      item_code == 22024,
      element_code == 6179
    ) %>% #keep US$
      rm_accent("item", "area") -> PD1

    PD2 <- PD1 %>%
      filter(item == "GDP Deflator", grepl("US\\$", element)) %>%
      select(area,
             area_code,
             item,
             item_code,
             element,
             element_code,
             year,
             value) %>%
      bind_rows(
        GDP_deflator_Taiwan %>%
          mutate(
            area = "China, Taiwan Province of",
            area_code = 214,
            item = "GDP Deflator",
            item_code = 22024,
            element = "Value US$, 2015 prices",
            element_code = 6179,
            value = round(100 * value / value[year == 2015], 2)
          )
      )

    ### output PD ----
    PD2 %>%
      add_title("FAO GDP deflators by country (2015 = 100)") %>%
      add_units("Unitless") %>%
      add_comments("Preprocessed FAOSTAT regional gdp deflators") %>%
      add_precursors("QCL_area_code_map",
                     "aglu/fao/FAOSTAT/Other_supplementary/GDP_deflator_Taiwan") ->
      PD



    # *[OA]: Population ----

    FAOSTAT_load_raw_data(DATASETCODE = "OA", DATA_FOLDER = DIR_RAW_DATA_FAOSTAT)

    OA %>% distinct(element, element_code)
    OA %>% distinct(item, item_code)

    OA %>% filter(area_code %in% QCL_area_code,
                  # only keep regions with production
                  element_code == 511,
                  item_code == 3010)  %>%
      select(area_code,
             area,
             item_code,
             item,
             element_code,
             element,
             year,
             value,
             unit) %>%
      rm_accent("item", "area") -> OA1

    ### output OA ----
    OA1 %>%
      add_title("FAO population") %>%
      add_units("tonne") %>%
      add_comments("Preprocessed FAO OA") ->
      OA

    return_data(MODULE_OUTPUTS)

  } else {
    stop("Unknown command")
  }
}
