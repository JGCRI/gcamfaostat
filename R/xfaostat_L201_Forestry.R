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
    c("FO_Roundwood")

  MODULE_OUTPUTS <-
    c("GCAMDATA_FAOSTAT_ForProdTrade_215Regs_Roundwood_1973to2020",
      "GCAMDATA_FAOSTAT_ForExportPrice_214Regs_Roundwood_1973to2020")

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


    # 215 unique areas with production data
    FO_area <-
      FO_Roundwood %>% filter(element_code == 5516) %>% distinct(area, area_code)
    FO_Roundwood %>% distinct(element, element_code, unit) -> UnitMap

    ## FO_Roundwood_Prod: Production ----
    FO_Roundwood %>%
      # Areas with production data
      right_join(FO_area, by = c("area_code", "area")) %>%
      filter(element_code %in% c(5516)) %>%  # Production
      # Complete all dimensions
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
      FO_Roundwood_Prod

    ## FO_Roundwood_Export_Q_V: Export  ----
    FO_Roundwood %>%
      # Areas with production data
      right_join(FO_area, by = c("area_code", "area")) %>%
      filter(element_code %in% c(5922 , 5916)) %>%  # Export quantity and value
      # Complete all dimensions
      complete(
        nesting(area_code, area),
        nesting(item_code, item),
        nesting(element_code, element, unit),
        year
      ) %>%
      select(-unit, -element_code) %>%
      spread(element, value) %>%
      # Fill in missing based on price relationship
      FF_FILL_NUMERATOR_DENOMINATOR(NUMERATOR_c = "Export Value",
                                    DENOMINATOR_c = "Export Quantity")  %>%
      # Remove area x year that should not exist
      FAOSTAT_AREA_RM_NONEXIST(RM_AREA_CODE = NULL) %>%
      # Get unit and element_code back
      left_join(UnitMap, by = "element") ->
      FO_Roundwood_Export_Q_V

    ## FO_Roundwood_Import_Q_V: Export  ----
    FO_Roundwood %>%
      right_join(FO_area, by = c("area_code", "area")) %>%
      filter(element_code %in% c(5622 , 5616)) %>%  # Export quantity and value
      # Complete all dimensions
      complete(
        nesting(area_code, area),
        nesting(item_code, item),
        nesting(element_code, element, unit),
        year
      ) %>%
      select(-unit, -element_code) %>%
      spread(element, value) %>%
      # Fill in missing based on price relationship
      FF_FILL_NUMERATOR_DENOMINATOR(NUMERATOR_c = "Import Value",
                                    DENOMINATOR_c = "Import Quantity")  %>%
      # Remove area x year that should not exist
      FAOSTAT_AREA_RM_NONEXIST(RM_AREA_CODE = NULL) %>%
      # Get unit and element_code back
      left_join(UnitMap, by = "element")  ->
      FO_Roundwood_Import_Q_V

    ## Bind and Balance ----

    # Adjust Export when Demand = Production + Import - Export < 0
    # Adjust Export as Production * Export_Production_ratio
    # initial value in constants.R was For_Export_Production_Ratio_Adj = 0.9

    FO_Roundwood_Prod %>%
      bind_rows(FO_Roundwood_Export_Q_V %>% filter(element == "Export Quantity")) %>%
      bind_rows(FO_Roundwood_Import_Q_V %>% filter(element == "Import Quantity")) %>%
      mutate(element = gsub(" Quantity", "", element)) %>%
      select(-unit, -element_code) %>%
      GROSS_TRADE_ADJUST %>%
      spread(element, value) %>%
      replace_na(list(Export = 0, Import = 0)) %>%
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
      mutate(unit = "m3") ->
      For_Balance

    # clean up
    rm(FO_area, UnitMap, FO_Roundwood, FO_Roundwood_Prod, FO_Roundwood_Import_Q_V)


    ## Produce output and export CSV ----
    GCAMDATA_FAOSTAT_ForProdTrade_215Regs_Roundwood_1973to2020 <-
      For_Balance %>% filter(element != "Demand") %>%
      spread(year, value)

    GCAMDATA_FAOSTAT_ForExportPrice_214Regs_Roundwood_1973to2020 <-
      FO_Roundwood_Export_Q_V %>% spread(year, value)


    GCAMDATA_FAOSTAT_ForProdTrade_215Regs_Roundwood_1973to2020 %>%
      add_title("FAO forestry production, export, and import (roundwood total) by country_year") %>%
      add_units("m3 ") %>%
      add_comments("Data is preprocessed and generated by gcamdata-FAOSTAT. Gross trade is balanced") %>%
      add_precursors("FO_Roundwood") ->
      GCAMDATA_FAOSTAT_ForProdTrade_215Regs_Roundwood_1973to2020

    GCAMDATA_FAOSTAT_ForExportPrice_214Regs_Roundwood_1973to2020 %>%
      add_title("FAO forests export qunatity and export value by country_year") %>%
      add_units("m3 and 1000 USD") %>%
      add_comments("Data is generated by gcamdata-FAOSTAT for deriving export prices") %>%
      add_precursors("FO_Roundwood") ->
      GCAMDATA_FAOSTAT_ForExportPrice_214Regs_Roundwood_1973to2020


    if (OUTPUT_Export_CSV == T) {
      output_csv_data(
        "GCAMDATA_FAOSTAT_ForProdTrade_215Regs_Roundwood_1973to2020",
        col_type_nonyear = "iciccc",
        title = "FAO forestry production, export, and import (roundwood total) by country_year",
        unit = "m3",
        code = "FO",
        description = "Data is preprocessed and generated by gcamdata-FAOSTAT. Gross trade is balanced",
        out_dir = DIR_OUTPUT_CSV
      )

      output_csv_data(
        "GCAMDATA_FAOSTAT_ForExportPrice_214Regs_Roundwood_1973to2020",
        col_type_nonyear = "iciccic",
        title = "FAO forests export qunatity and export value by country_year",
        unit = "Export Quantity in m3; Export Value in 1000 US$",
        description = "Data is generated by gcamdata-FAOSTAT for deriving export prices",
        code = "FO",
        out_dir = DIR_OUTPUT_CSV
      )
    }

    return_data(MODULE_OUTPUTS)

  } else {
    stop("Unknown command")
  }
}
