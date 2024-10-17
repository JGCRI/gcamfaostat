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
    c(FAOSTAT_FILE = file.path(DIR_RAW_DATA_FAOSTAT, "Prices_E_All_Data_Normalized"),
      FAOSTAT_FILE = file.path(DIR_RAW_DATA_FAOSTAT, "Deflators_E_All_Data_Normalized"),
      FAOSTAT_FILE = file.path(DIR_RAW_DATA_FAOSTAT, "Population_E_All_Data_Normalized"),
      FAOSTAT_FILE = file.path(DIR_RAW_DATA_FAOSTAT, "Investment_CapitalStock_E_All_Data_Normalized"),
      FAOSTAT_FILE = file.path(DIR_RAW_DATA_FAOSTAT, "Macro-Statistics_Key_Indicators_E_All_Data_Normalized"),
      FILE = file.path(DIR_RAW_DATA_FAOSTAT, "Other_supplementary/GDP_deflator_Taiwan"),
      "QCL_area_code_map")

    MODULE_OUTPUTS <-
    c("PP_wide",          # Producer prices
      "PD",               # GDP deflator
      "OA",               # Population
      "CS",               # Capital stock
      "MK")               # Macro-Statistics (GDP)

  if(command == driver.DECLARE_INPUTS) {
    return(MODULE_INPUTS)
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(MODULE_OUTPUTS)
  } else if(command == driver.MAKE) {

    year <- value <- Year <- Value <- FAO_country <- iso <- NULL    # silence package check.
    QCL_area_code_map <- element_code <- element <- area_code <- item_code <- area <-
      item <- unit <- PP <- `Producer Price (USD/tonne)` <- pp_base <- pp_baseindex <-
      `Producer Price Index (2014-2016 = 100)` <- GDP_deflator_Taiwan <- NULL

    all_data <- list(...)[[1]]

    Curr_Envir <- environment()


    if(Process_Raw_FAO_Data == FALSE) {

      # Load from Prebuilt data ----
      PP_wide <- extract_prebuilt_data("PP_wide")
      PD <- extract_prebuilt_data("PD")
      OA <- extract_prebuilt_data("OA")
      CS <- extract_prebuilt_data("CS")
      MK <- extract_prebuilt_data("MK")

    } else {

    # Load required inputs ----
    get_data_list(all_data, MODULE_INPUTS, strip_attributes = TRUE)

    # Get area code ----
    QCL_area_code <- QCL_area_code_map %>% distinct(area_code) %>% pull()


    # *[PP] Producer price ----

    FAOSTAT_load_raw_data(DATASETCODE = "PP", .Envir = Curr_Envir)
    # check data
    PP %>% distinct(element, element_code, unit)

    assertthat:: assert_that(
      PP %>% distinct(element, element_code, unit) %>%
        filter(element_code == 5539) %>% pull(element) == "Producer Price Index (2014-2016 = 100)",
      msg = "Price index element changed; please check and update."
    )
    PriceIndexYear = 2015


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
          filter(year == PriceIndexYear) %>% within(rm(year)) %>%
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

    PP2 %>% spread(year, value) ->
      PP_wide


    ### output PP ----
    PP_wide %>%
      add_title("FAO producer prices") %>%
      add_units("USD/tonne") %>%
      add_comments("Preprocessed FAOSTAT producer prices") %>%
      add_precursors("QCL_area_code_map",
                     file.path(DIR_RAW_DATA_FAOSTAT, "Prices_E_All_Data_Normalized")) ->
      PP_wide

    verify_identical_prebuilt(PP_wide)

    # [PD] FAO_GDP_deflators ----
    #**************************************

    FAOSTAT_load_raw_data(DATASETCODE = "PD", .Envir = Curr_Envir)
    # read in Taiwan values as FAO does not have Taiwan price data
    # GDP_deflator_Taiwan

    PD %>% distinct(element, element_code, unit)
    PD %>% distinct(item, item_code)

    PD %>% filter(
      year >= min(FAOSTAT_Hist_Year),
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
                     file.path(DIR_RAW_DATA_FAOSTAT, "Deflators_E_All_Data_Normalized"),
                     file.path(DIR_RAW_DATA_FAOSTAT, "Other_supplementary/GDP_deflator_Taiwan")) ->
      PD

    verify_identical_prebuilt(PD)


    # *[OA]: Population ----

    FAOSTAT_load_raw_data(DATASETCODE = "OA", .Envir = Curr_Envir)

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
      add_comments("Preprocessed FAO OA")  %>%
      add_precursors(file.path(DIR_RAW_DATA_FAOSTAT, "Population_E_All_Data_Normalized")) ->
      OA

    verify_identical_prebuilt(OA)

    # *[CS]: Capital stock ----
    FAOSTAT_load_raw_data(DATASETCODE = "CS", .Envir = Curr_Envir)

    #CS %>% distinct(element, element_code)
    #CS %>% distinct(item, item_code)
    # check area mapping
    # CS %>% filter(area_code < 400) %>%
    #   distinct(area, area_code) %>% full_join(QCL_area_code_map, by = c("area_code"))

    CS %>% filter(area_code %in% QCL_area_code,
                  # only keep regions with production
                  element_code == 6184 # Value US$, 2015 prices
    )  %>%
      select(area_code,
             area,
             item_code,
             item,
             element_code,
             element,
             year,
             value,
             unit) %>%
      rm_accent("item", "area") -> CS1

    ### output OA ----
    CS1 %>%
      add_title("FAO capiral stock") %>%
      add_units("2015 USD") %>%
      add_comments("Preprocessed FAO CS")  %>%
      add_precursors(file.path(DIR_RAW_DATA_FAOSTAT, "Investment_CapitalStock_E_All_Data_Normalized")) ->
      CS

    verify_identical_prebuilt(CS)


    # *[MK]: macroeconomic stat (GDP) ----
    FAOSTAT_load_raw_data(DATASETCODE = "MK", .Envir = Curr_Envir)

    #MK %>% distinct(element, element_code)
    #MK %>% distinct(item, item_code)

    MK %>%
      filter(
        area_code < 350, # rm aggregated
        element_code %in% c(6110, 6184),
        item_code == 22008)  %>%
      select(area_code,
             area,
             item_code,
             item,
             element_code,
             element,
             year,
             value,
             unit) %>%
      rm_accent("item", "area") -> MK1

    ### output MK ----
      MK1 %>%
      add_title("FAO GDP") %>%
      add_units("2015 million USD or nominal million USD") %>%
      add_comments("Preprocessed FAO MK")  %>%
      add_precursors(file.path(DIR_RAW_DATA_FAOSTAT, "Macro-Statistics_Key_Indicators_E_All_Data_Normalized")) ->
      MK

    verify_identical_prebuilt(MK)

    }

    return_data(MODULE_OUTPUTS)

  } else {
    stop("Unknown command")
  }
}
