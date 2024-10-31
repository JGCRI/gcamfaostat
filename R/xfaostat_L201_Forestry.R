# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_xfaostat_L201_Forestry
#'
#' process FAOSTAT forestry data (FO) into gcamdata inputs
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
#' @importFrom tidyr complete drop_na gather nesting spread replace_na fill
#' @author XZ 2023
module_xfaostat_L201_Forestry <- function(command, ...) {

  MODULE_INPUTS <-
    c("FO_RoundwoodProducts")

  MODULE_OUTPUTS <-
    c("L201.For_Balance",
      "L201.FO_RoundwoodProducts_Export_Q_V")

  if(command == driver.DECLARE_INPUTS) {
    return(MODULE_INPUTS)
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(MODULE_OUTPUTS)
  } else if(command == driver.MAKE) {

    year <- value <- Year <- Value <- FAO_country <- iso <- NULL    # silence package check.
    item_code <- item <- area_code <- area <- unit <- element_code <- element <-
      Demand <- Export <- Import <- Production <- FO_Roundwood <- NULL

    all_data <- list(...)[[1]]

    # Load required inputs ----
    get_data_list(all_data, MODULE_INPUTS, strip_attributes = TRUE)


    ## Proprocess and quick clean ----

    c(1865, 1634, 1873, 1872, 1875, 1876) -> Key_FO_Items
    # 1865	Industrial roundwood
    # 1634	Veneer sheets
    # 1873	Wood-based panels
    # 1872	Sawnwood
    # 1875	Wood pulp
    # 1876  Paper and paperboard

    FO_RoundwoodProducts %>% filter(item_code %in% c(1865, 1634, 1873, 1872, 1875, 1876)) ->
      L201.FO_RoundwoodProducts



    # 215+ unique areas with production data in roundwood
    # BUT 185 in industrial round wood
    # areas are indeed different across items! Check again later

    # FO_RoundwoodProducts %>% filter(item_code %in% c(1865, 1634, 1873, 872, 1875)) %>%
    #   FF_check_count_plot()
    FO_area_item <-
      L201.FO_RoundwoodProducts %>%
      filter(item_code %in% 1865, element_code == 5516) %>%
      distinct(area, area_code) %>%
      full_join(
        L201.FO_RoundwoodProducts %>% distinct(item, item_code), by = character()
      )

    L201.FO_RoundwoodProducts %>% distinct(item_code, element, element_code, unit) -> UnitMap


    ## FO_RoundwoodProducts_Prod: Production ----
    L201.FO_RoundwoodProducts %>%
      filter(element %in% "Production") %>%  # Production
      # Areas with production data
      right_join(FO_area_item, by = c("area_code", "area", "item_code", "item")) %>%
      filter(!is.na(year)) %>%
      # Complete all dimensions
      group_by(item_code, item) %>%
      tidyr::complete(
        nesting(area_code, area),
        nesting(item_code, item),
        nesting(element_code, element, unit),
        year
      ) %>%
      group_by(area_code, area, item_code, item) %>%
      # linearly interpolate only forward!
      # then NA = 0
      mutate(value = approx_fun(year, value)) %>%
      ungroup() %>%
      tidyr::replace_na(list(value = 0)) %>%
      # Remove area x year that should not exist
      FAOSTAT_AREA_RM_NONEXIST(RM_AREA_CODE = NULL)  ->
      L201.FO_RoundwoodProducts_Prod


    ## FO_RoundwoodProducts_Export_Q_V: Export  ----
    L201.FO_RoundwoodProducts %>%
      filter(element %in% c("Export Quantity", "Export Value") )  %>%  # Export quantity and value
      # Areas with production data
      right_join(FO_area_item, by = c("area_code", "area", "item_code", "item")) %>%
      filter(!is.na(year)) %>%
      # Complete all dimensions
      group_by(item_code, item) %>%
      complete(
        nesting(area_code, area),
        nesting(item_code, item),
        nesting(element_code, element, unit),
        year
      ) %>% ungroup() %>%
      select(-unit, -element_code) %>%
      spread(element, value) %>%
      # Fill in missing based on price relationship
      FF_FILL_NUMERATOR_DENOMINATOR(NUMERATOR_c = "Export Value",
                                    DENOMINATOR_c = "Export Quantity")  %>%
      # Remove area x year that should not exist
      FAOSTAT_AREA_RM_NONEXIST(RM_AREA_CODE = NULL) %>%
      # Get unit and element_code back
      left_join(UnitMap, by = c("item_code", "element")) ->
      L201.FO_RoundwoodProducts_Export_Q_V


    ## FO_RoundwoodProducts_Import_Q_V: Import  ----
    L201.FO_RoundwoodProducts %>%
      filter(element %in% c("Import Quantity", "Import Value") )  %>%  # Import quantity and value
      # Areas with production data
      right_join(FO_area_item, by = c("area_code", "area", "item_code", "item")) %>%
      filter(!is.na(year)) %>%
      # Complete all dimensions
      group_by(item_code, item) %>%
      complete(
        nesting(area_code, area),
        nesting(item_code, item),
        nesting(element_code, element, unit),
        year
      ) %>% ungroup %>%
      select(-unit, -element_code) %>%
      spread(element, value) %>%
      # Fill in missing based on price relationship
      FF_FILL_NUMERATOR_DENOMINATOR(NUMERATOR_c = "Import Value",
                                    DENOMINATOR_c = "Import Quantity")  %>%
      # Remove area x year that should not exist
      FAOSTAT_AREA_RM_NONEXIST(RM_AREA_CODE = NULL) %>%
      # Get unit and element_code back
      left_join(UnitMap, by = c("item_code", "element")) ->
      L201.FO_RoundwoodProducts_Import_Q_V


    ## Bind and Balance ----

    # Adjust Export when Demand = Production + Import - Export < 0
    # Adjust Export as Production * Export_Production_ratio
    # initial value in constants.R was For_Export_Production_Ratio_Adj = 0.9

    L201.FO_RoundwoodProducts_Prod %>%
      bind_rows(L201.FO_RoundwoodProducts_Export_Q_V %>% filter(element == "Export Quantity")) %>%
      bind_rows(L201.FO_RoundwoodProducts_Import_Q_V %>% filter(element == "Import Quantity")) %>%
      mutate(element = gsub(" Quantity", "", element)) %>%
      select(-unit, -element_code) %>%
      GROSS_TRADE_ADJUST %>%
      spread(element, value) %>%
      replace_na(list(Export = 0, Import = 0)) %>%
      # also setting production = 0, if NA
      replace_na(list(Production = 0)) %>%
      # Remove area x year that should not exist
      FAOSTAT_AREA_RM_NONEXIST(RM_AREA_CODE = NULL) %>%
      mutate(
        Demand = Production + Import - Export,
        Export = if_else(Demand < 0,
                         Production * For_Export_Production_Ratio_Adj,
                         Export)
      ) %>%
      gather(element, value, -area_code, -area, -item_code, -item, -year) %>%
      # Adjust trade again
      GROSS_TRADE_ADJUST %>%
      spread(element, value) %>%
      mutate(Demand = Production + Import - Export) %>%
      gather(element, value, -area_code, -area, -item_code, -item, -year) %>%
      left_join(UnitMap %>% filter(element == "Production") %>% distinct(item_code, unit), by = "item_code") ->
      L201.For_Balance


    # clean up
    rm(FO_area_item, UnitMap,  L201.FO_RoundwoodProducts,  L201.FO_RoundwoodProducts_Prod,  L201.FO_RoundwoodProducts_Import_Q_V)

    L201.For_Balance %>%
      add_title("FAO forestry consumption, production, export, and import (roundwood total) by country_year") %>%
      add_units("m3 ") %>%
      add_comments("Data is preprocessed and generated by gcamdata-FAOSTAT. Gross trade is balanced") %>%
      add_precursors("FO_RoundwoodProducts") ->
      L201.For_Balance

    L201.FO_RoundwoodProducts_Export_Q_V %>%
      add_title("FAO forests export qunatity and export value by country_year") %>%
      add_units("m3 and 1000 USD") %>%
      add_comments("Data is generated by gcamdata-FAOSTAT for deriving export prices") %>%
      add_precursors("FO_RoundwoodProducts") ->
      L201.FO_RoundwoodProducts_Export_Q_V

    return_data(MODULE_OUTPUTS)

  } else {
    stop("Unknown command")
  }
}
