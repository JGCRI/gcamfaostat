# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_yfaostat_GCAM_DataExport
#'
#' Generate supply utilization balance in primary equivalent
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
#' @author XZ Nov2023
module_yfaostat_GCAM_DataExport <- function(command, ...) {

  MODULE_INPUTS <-
    c("CS",
      "PD",
      "MK",
      "FBSH_CBH_wide",
      "L102.QCL_PROD",
      "L102.QCL_AN_LIVEANIMAL",
      "L102.QCL_AN_PRIMARY_MILK",
      "L102.QCL_CROP_PRIMARY",
      "L104.QCL_FODDERCROP",
      "L103.QCL_PRIMARY_PROD_PV",
      "L105.Bal_new_all",
      "TM_bilateral_wide",
      "L106.SUA_food_macronutrient_rate",
      "L201.For_Balance",
      "L201.FO_RoundwoodProducts_Export_Q_V",
      "L301.RL_LandCover",
      "L401.RFN_ProdDemand")

    MODULE_OUTPUTS <-
      c("yfaostat_GCAM_GCAMFAOSTAT_CSV",
        CSV = "GCAMFAOSTAT_SUA",
        CSV = "GCAMFAOSTAT_BiTrade",
        CSV = "GCAMFAOSTAT_FBSH_CB",
        CSV = "GCAMFAOSTAT_NonFodderProdArea",
        CSV = "GCAMFAOSTAT_FodderProdArea",
        CSV = "GCAMFAOSTAT_AnimalStock",
        CSV = "GCAMFAOSTAT_ProdPrice",
        CSV = "FAO_GDP_Deflators",
        CSV = "GCAMFAOSTAT_CapitalStock",
        CSV = "CAMFAOSTAT_GDP",
        CSV = "GCAMFAOSTAT_MacroNutrientRate",
        CSV = "GCAMFAOSTAT_ForProdTrade",
        CSV = "GCAMFAOSTAT_ForExpPrice",
        CSV = "GCAMFAOSTAT_LandCover",
        CSV = "GCAMFAOSTAT_NFertilizer")

  if(command == driver.DECLARE_INPUTS) {
    return(MODULE_INPUTS)
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(MODULE_OUTPUTS)
  } else if(command == driver.MAKE) {

    year <- value <- Year <- Value <- FAO_country <- iso <- NULL    # silence package check.
    area_code <- source_code <- area_code1 <- item_code <- L105.Bal_new_all <- element <- FBSH_CBH_wide <-
      element_code <- L102.QCL_CROP_PRIMARY <- L102.QCL_PROD <- unit <- L104.QCL_FODDERCROP <- L102.QCL_AN_LIVEANIMAL <-
      L102.QCL_AN_PRIMARY_MILK <- L103.QCL_PRIMARY_PROD_PV <- area <- PD <- L106.SUA_food_macronutrient_rate  <-
      TM_bilateral_wide <- NULL

    all_data <- list(...)[[1]]


    Curr_Envir <- environment()

    # adding dummy output ----
    yfaostat_GCAM_GCAMFAOSTAT_CSV <-
      tibble(CSV_export = MODULE_OUTPUTS)


    if (OUTPUT_Export_CSV == "GCAM") {

      # Load required inputs ----
      get_data_list(all_data, MODULE_INPUTS, strip_attributes = TRUE)


      # Helper functions
      yfaostat_GCAM_GCAMFAOSTAT_CSV <- NULL
      add_to_output_meta <- function(.df = DF_TEMP,
                                     .datasetname){

        if ("year" %in% names(.df)) {

          .df %>% gather_years() -> .df
           YearRange = paste0(.df %>% distinct(year) %>% min," to ", .df %>% distinct(year) %>% max)

        } else{
          YearRange = "BaseYears"
        }

        yfaostat_GCAM_GCAMFAOSTAT_CSV %>%
          bind_rows(
            data.frame(dataset = .datasetname,
                       nReg = .df %>% distinct(area_code) %>% nrow,
                       nItem = .df %>% distinct(item_code) %>% nrow,
                       Years = YearRange)
          ) -> .df1

        assign("yfaostat_GCAM_GCAMFAOSTAT_CSV", .df1, envir = Curr_Envir)
      }


      # Bilateral trade ----
      ## *BiTrade ----

      TM_bilateral_wide %>%
        gather_years() %>%
        filter(year >= min(FAOSTAT_Hist_Year_FBS)) %>%
        filter(value > 0) %>%
        FAOSTAT_AREA_RM_NONEXIST() %>%
        rename(area_code1 = area_code, area_code = source_code) %>%
        FAOSTAT_AREA_RM_NONEXIST() %>%
        rename(source_code = area_code, area_code = area_code1) ->
        TM_bilateral

      TM_bilateral %>%
        mutate(value = value / 1000) %>% # change unit to 1000 tonnes
        # only export quality data years
        filter(year >= min(FAOSTAT_Hist_Year_FBS)) %>%
        FAO_AREA_DISAGGREGATE_HIST_DISSOLUTION_ALL(SUDAN2012_MERGE = T) %>%
        # merge Sudan and South Sudan
        FAO_AREA_DISAGGREGATE_HIST_DISSOLUTION_ALL(
          .FAO_AREA_CODE_COL = "source_code",
          .AREA_COL = "source",
          SUDAN2012_MERGE = T
        ) %>%
        filter(value != 0.0) %>%
        transmute(area_code, item_code, source_code, year, value) %>%
        add_title("GCAMFAOSTAT_BiTrade") %>%
        add_units("1000 tonnes") %>%
        add_comments("gcamfaostat Export CSV") %>%
        add_precursors("TM_bilateral_wide",
                       "yfaostat_GCAM_GCAMFAOSTAT_CSV")->
        GCAMFAOSTAT_BiTrade

      add_to_output_meta(.df = GCAMFAOSTAT_BiTrade, "GCAMFAOSTAT_BiTrade")

      output_csv_data(
        gcam_dataset = GCAMFAOSTAT_BiTrade,
        out_filename = "GCAMFAOSTAT_BiTrade",
        col_type_nonyear = "iiiin",
        title = "BiTrade for all available FAO items in FAOSTAT_Hist_Year_TMBilateral",
        unit = "1000 tonnes",
        code = "TM",
        description = "Data is compiled and generated by gcamfaostat. Bilateral trade data.",
        out_dir = DIR_OUTPUT_CSV,
        GZIP = T)

      rm(TM_bilateral_wide, TM_bilateral)


      # SUA and FBS ----
      ## *SUA ----

      L105.Bal_new_all %>% filter(value != 0.0) %>%
        transmute(area_code, item_code, element, year, value) %>%
        add_title("GCAMFAOSTAT_SUA") %>%
        add_units("1000 tonnes") %>%
        add_comments("gcamfaostat Export CSV") %>%
        add_precursors("L105.Bal_new_all",
                       "yfaostat_GCAM_GCAMFAOSTAT_CSV") ->
        GCAMFAOSTAT_SUA

      add_to_output_meta(.df = GCAMFAOSTAT_SUA, "GCAMFAOSTAT_SUA")

      output_csv_data(
        gcam_dataset = GCAMFAOSTAT_SUA,
        out_filename = "GCAMFAOSTAT_SUA",
        col_type_nonyear = "iifin",
        title = "Supply_utilization_accounting for all FAO items in FAOSTAT_Hist_Year_FBS",
        unit = "1000 tonnes",
        code = "SCL",
        description = "Data is compiled and generated by gcamfaostat. Data is balanced in trade, supply_utilization, and storage",
        out_dir = DIR_OUTPUT_CSV,
        GZIP = T)

      rm(L105.Bal_new_all)

      ## *FBSH and CB ----
      FBSH_CBH_wide %>%
        gather_years() %>%
        FAOSTAT_AREA_RM_NONEXIST() %>%
        select(-element_code) %>%
        # merge Sudan and South Sudan
        FAO_AREA_DISAGGREGATE_HIST_DISSOLUTION_ALL(SUDAN2012_MERGE = T) ->
        FBSH_CB

      FBSH_CB %>%
        mutate(unit = "1000 tonnes", value = value) %>%
        filter(year <= min(FAOSTAT_Hist_Year_FBS) - 1)  %>%
        filter(!is.na(year)) %>%
        spread(year, value) %>%
        add_title("GCAMFAOSTAT_FBSH_CB") %>%
        add_units("1000 tonnes") %>%
        add_comments("gcamfaostat Export CSV") %>%
        add_precursors("FBSH_CBH_wide",
                       "yfaostat_GCAM_GCAMFAOSTAT_CSV") ->
        GCAMFAOSTAT_FBSH_CB

      add_to_output_meta(.df = GCAMFAOSTAT_FBSH_CB, "GCAMFAOSTAT_FBSH_CB")

      output_csv_data(
        gcam_dataset = GCAMFAOSTAT_FBSH_CB,
        out_filename ="GCAMFAOSTAT_FBSH_CB",
        col_type_nonyear = "iicccc",
        title = "Old FAO food balance sheet in primary equilvalent in 1973 to 2009",
        unit = "1000 tonnes",
        code = "FBSH",
        description = "Data is compiled and generated by gcamfaostat. FBSH and CB include old food and nonfood balances.",
        out_dir = DIR_OUTPUT_CSV,
        GZIP = T
      )

      rm(FBSH_CBH_wide, FBSH_CB)

      # Production and Area harvested ----


      for (.DF in c("L102.QCL_CROP_PRIMARY", "L104.QCL_FODDERCROP", "L102.QCL_PROD")) {
        get(.DF) %>%
          # merge Sudan and South Sudan
          FAO_AREA_DISAGGREGATE_HIST_DISSOLUTION_ALL(SUDAN2012_MERGE = T) %>%
          assign(x = .DF, envir = Curr_Envir)
      }

      ## *NonFodderProdArea ----
      L102.QCL_CROP_PRIMARY %>%
        filter(element == "Area harvested") %>%
        mutate(item_set = "QCL_COMM_CROP_PRIMARY") %>%
        bind_rows(L102.QCL_PROD %>%
                    filter(!is.na(year))) %>%
        select(-element_code) %>%
        mutate(value = value / 1000,
               unit = if_else(unit == "tonnes", "1000 tonnes", "1000 ha")) %>%
        spread(year, value) %>%
        add_title("GCAMFAOSTAT_NonFodderProdArea") %>%
        add_units("1000 tonnes or 1000 ha") %>%
        add_comments("gcamfaostat Export CSV") %>%
        add_precursors("L102.QCL_CROP_PRIMARY",
                       "L102.QCL_PROD",
                       "yfaostat_GCAM_GCAMFAOSTAT_CSV") ->
        GCAMFAOSTAT_NonFodderProdArea

      add_to_output_meta(.df = GCAMFAOSTAT_NonFodderProdArea, "GCAMFAOSTAT_NonFodderProdArea")

      output_csv_data(gcam_dataset = GCAMFAOSTAT_NonFodderProdArea,
        out_filename = "GCAMFAOSTAT_NonFodderProdArea",
        col_type_nonyear = "iiccccc",
        title = "Production and harvested area for FAO items",
        unit = "1000 tonnes or 1000 ha",
        code = "QCL",
        description = "Data is compiled and generated by gcamfaostat. Data is cleaned mainly based on QCL from FAOSTAT.",
        out_dir = DIR_OUTPUT_CSV,
        GZIP = T
      )

      ## *FodderProdArea ----
      L104.QCL_FODDERCROP %>%
        filter(!is.na(year)) %>%
        select(-element_code) %>%
        mutate(value = value / 1000,
               unit = if_else(unit == "tonnes", "1000 tonnes", "1000 ha")) %>%
        spread(year, value) %>%
        add_title("GCAMFAOSTAT_FodderProdArea") %>%
        add_units("1000 tonnes or 1000 ha") %>%
        add_comments("gcamfaostat Export CSV") %>%
        add_precursors("L104.QCL_FODDERCROP",
                       "yfaostat_GCAM_GCAMFAOSTAT_CSV") ->
        GCAMFAOSTAT_FodderProdArea

      add_to_output_meta(.df = GCAMFAOSTAT_FodderProdArea, "GCAMFAOSTAT_FodderProdArea")

      output_csv_data(gcam_dataset = GCAMFAOSTAT_FodderProdArea,
        out_filename = "GCAMFAOSTAT_FodderProdArea",
        col_type_nonyear = "iicccc",
        title = "Production and harvested area for fodder crops in 1973 to 2020",
        unit = "1000 tonnes or 1000 ha",
        code = "QCL",
        description = "Data is compiled and generated by gcamfaostat. Data is extrapolated based on old FAOSTAT data (to 2012) used in gcamdata.",
        out_dir = DIR_OUTPUT_CSV,
        GZIP = T
      )

      rm(L102.QCL_CROP_PRIMARY, L104.QCL_FODDERCROP, L102.QCL_PROD)

      ##*AnimalStocks ----
      # Laying is not needed for now; live animal stocks and milk animals are used for water demand calculation in GCAM
      # 1171 "Animals live nes" and 1083 "Pigeons, other birds" are not available


      L102.QCL_AN_LIVEANIMAL %>% filter(element == "Stocks") %>%
        spread(year, value) -> FAO_an_Stocks

      L102.QCL_AN_PRIMARY_MILK %>% filter(element == "Milk Animals") %>%
        spread(year, value) -> FAO_an_Dairy_Stocks

      FAO_an_Stocks %>%
        bind_rows(FAO_an_Dairy_Stocks) %>%
        add_title("GCAMFAOSTAT_AnimalStock") %>%
        add_units("Head, 1000 Head or No") %>%
        add_comments("gcamfaostat Export CSV") %>%
        add_precursors("FAO_an_Stocks",
                       "FAO_an_Dairy_Stocks",
                       "yfaostat_GCAM_GCAMFAOSTAT_CSV") ->
        GCAMFAOSTAT_AnimalStock

      add_to_output_meta(.df = GCAMFAOSTAT_AnimalStock, "GCAMFAOSTAT_AnimalStock")

      output_csv_data(
        gcam_dataset = GCAMFAOSTAT_AnimalStock,
        out_filename = "GCAMFAOSTAT_AnimalStock",
        col_type_nonyear = "icicicc",
        title = "FAO all meat animal stock and milk animal stock by area_item_year",
        unit = "Head, 1000 Head or No",
        code = "QCL",
        description = "Data is preprocessed and generated by gcamfaostat",
        out_dir = DIR_OUTPUT_CSV,
        GZIP = T
      )

      # Producer prices in quantity and value ----
      ##*Producer Price ----

      L103.QCL_PRIMARY_PROD_PV %>%
        filter(year >= MIN_HIST_PP_YEAR) %>%
        spread(year, value) %>%
        add_title("GCAMFAOSTAT_ProdPrice") %>%
        add_units("Quantity in tonne; Value in US$") %>%
        add_comments("gcamfaostat Export CSV") %>%
        add_precursors("L103.QCL_PRIMARY_PROD_PV",
                       "yfaostat_GCAM_GCAMFAOSTAT_CSV") ->
        GCAMFAOSTAT_ProdPrice

      add_to_output_meta(.df = GCAMFAOSTAT_ProdPrice, "GCAMFAOSTAT_ProdPrice")

      output_csv_data(
        gcam_dataset = GCAMFAOSTAT_ProdPrice,
        out_filename = "GCAMFAOSTAT_ProdPrice",
        col_type_nonyear = "iciccc",
        title = "FAO primary Ag and An production quantity and value by area_item_year for prices derivation",
        unit = "Quantity in tonne; Value in US$",
        code = "PP",
        description = "Data is preprocessed and generated by gcamfaostat. Processed QCL was also used. Missing values filled in.",
        out_dir = DIR_OUTPUT_CSV,
        GZIP = T
      )
      rm(L103.QCL_PRIMARY_PROD_PV)


      ##*GDP deflators ----

      FAO_GDP_Deflators <-
        PD %>% FAOSTAT_AREA_RM_NONEXIST(RM_AREA_CODE = NULL) %>%
        spread(year, value) %>%
        add_title("FAO_GDP_Deflators") %>%
        add_units("Unitless") %>%
        add_comments("gcamfaostat Export CSV") %>%
        add_precursors("FAO_GDP_Deflators",
                       "yfaostat_GCAM_GCAMFAOSTAT_CSV")

      add_to_output_meta(.df = FAO_GDP_Deflators, "FAO_GDP_Deflators")

      output_csv_data(
        gcam_dataset = FAO_GDP_Deflators,
        out_filename = "FAO_GDP_Deflators",
        col_type_nonyear = "cicici",
        title = "FAO GDP deflators by country (2015 = 100)",
        unit = "Unitless",
        code = "PD",
        description = "FAOSTAT (domain:PD) & Taiwan Statistics (access:4-12-2021)",
        out_dir = DIR_OUTPUT_CSV
      )

      # Macronutrient ----

      ## *MacroNutrientRate----


      L106.SUA_food_macronutrient_rate ->
        GCAMFAOSTAT_MacroNutrientRate


      # Fix Sudan with code 206 and 276 after 2012
      if (206 %in% GCAMFAOSTAT_MacroNutrientRate$area_code == F) {
        GCAMFAOSTAT_MacroNutrientRate %>%
          filter(area_code  == 276) %>% mutate(area_code  = 206) %>%
          bind_rows(GCAMFAOSTAT_MacroNutrientRate) ->
          GCAMFAOSTAT_MacroNutrientRate
      }

      GCAMFAOSTAT_MacroNutrientRate %>%
        add_title("GCAMFAOSTAT_MacroNutrientRate") %>%
        add_units("calories per g or (fat or protein) percentage") %>%
        add_comments("gcamfaostat Export CSV") %>%
        add_precursors("L106.SUA_food_macronutrient_rate",
                       "yfaostat_GCAM_GCAMFAOSTAT_CSV") ->
        GCAMFAOSTAT_MacroNutrientRate

      add_to_output_meta(.df = GCAMFAOSTAT_MacroNutrientRate, "GCAMFAOSTAT_MacroNutrientRate")

      output_csv_data(
        gcam_dataset = GCAMFAOSTAT_MacroNutrientRate,
        out_filename = "GCAMFAOSTAT_MacroNutrientRate",
        col_type_nonyear = "iicnnn",
        title = "Macronutrient conversion factor for food items, mean values of Hist_MEAN_Year_NUTRIENT_MASS_CONV",
        unit = "calories per g or (fat or protein) percentage",
        code = "SCL",
        description = "Data is compiled and generated by gcamfaostat. Macronutrient conversion factor for all available FAO food items.",
        out_dir = DIR_OUTPUT_CSV,
        GZIP = T
      )

      # Forestry data output ----

      ##*ForProdTrade ----
      L201.For_Balance %>%
        filter(element != "Demand") %>%
        spread(year, value) %>%
        add_title("GCAMFAOSTAT_ForProdTrade for forest products") %>%
        add_units("m3") %>%
        add_comments("gcamfaostat Export CSV") %>%
        add_precursors("L201.For_Balance",
                       "yfaostat_GCAM_GCAMFAOSTAT_CSV") ->
        GCAMFAOSTAT_ForProdTrade

      add_to_output_meta(.df = GCAMFAOSTAT_ForProdTrade, "GCAMFAOSTAT_ForProdTrade")

      output_csv_data(
        gcam_dataset = GCAMFAOSTAT_ForProdTrade,
        out_filename = "GCAMFAOSTAT_ForProdTrade",
        col_type_nonyear = "iciccc",
        title = "FAO forestry production, export, and import (forest products) by country_year",
        unit = "m3",
        code = "FO",
        description = "Data is preprocessed and generated by gcamfaostat. Gross trade is balanced",
        out_dir = DIR_OUTPUT_CSV
      )

      ##*ForExpPrice ----
      L201.FO_RoundwoodProducts_Export_Q_V %>%
        spread(year, value) %>%
        add_title("GCAMFAOSTAT_ForExpPrice") %>%
        add_units("Export Quantity in m3; Export Value in 1000 US$") %>%
        add_comments("gcamfaostat Export CSV") %>%
        add_precursors("L201.FO_RoundwoodProducts_Export_Q_V",
                       "yfaostat_GCAM_GCAMFAOSTAT_CSV") ->
        GCAMFAOSTAT_ForExpPrice

      add_to_output_meta(.df = GCAMFAOSTAT_ForExpPrice, "GCAMFAOSTAT_ForExpPrice")

      output_csv_data(
        gcam_dataset = GCAMFAOSTAT_ForExpPrice,
        out_filename = "GCAMFAOSTAT_ForExpPrice",
        col_type_nonyear = "iciccic",
        title = "FAO forests export qunatity and export value by country_year",
        unit = "Export Quantity in m3; Export Value in 1000 US$",
        description = "Data is generated by gcamfaostat for deriving export prices",
        code = "FO",
        out_dir = DIR_OUTPUT_CSV
      )


      # Land Cover data output ----

      ##*LandCover ----
      L301.RL_LandCover %>%
        spread(year, value) %>%
        add_title("GCAMFAOSTAT_LandCover") %>%
        add_units("1000 Ha") %>%
        add_comments("gcamfaostat Export CSV") %>%
        add_precursors("L301.RL_LandCover",
                       "yfaostat_GCAM_GCAMFAOSTAT_CSV") ->
        GCAMFAOSTAT_LandCover

      add_to_output_meta(.df = GCAMFAOSTAT_LandCover, "GCAMFAOSTAT_LandCover")

      output_csv_data(
        gcam_dataset = GCAMFAOSTAT_LandCover,
        out_filename = "GCAMFAOSTAT_LandCover",
        col_type_nonyear = "icicicc",
        title = "FAO cropland, Temporary Crop (harvested cropland), and fallow area by country_year",
        unit = "1000 Ha",
        description = "Data is cleaned and generated by gcamfaostat",
        code = "RL",
        out_dir = DIR_OUTPUT_CSV)



      # Fertilizer data output----

      ##* N Fertilizer ----
      L401.RFN_ProdDemand %>%
        spread(year, value) %>%
        add_title("GCAMFAOSTAT_NFertilizer") %>%
        add_units("tonnes N") %>%
        add_comments("gcamfaostat Export CSV") %>%
        add_precursors("L401.RFN_ProdDemand",
                       "yfaostat_GCAM_GCAMFAOSTAT_CSV") ->
        GCAMFAOSTAT_NFertilizer

      add_to_output_meta(.df = GCAMFAOSTAT_NFertilizer, "GCAMFAOSTAT_NFertilizer")

      output_csv_data(
        gcam_dataset = GCAMFAOSTAT_NFertilizer,
        out_filename = "GCAMFAOSTAT_NFertilizer",
        col_type_nonyear = "icicicc",
        title = "FAO fertilizer production and consumption by country_year",
        unit = "tonnes N",
        description = "Data is preprocessed and generated by gcamfaostat",
        code = "RFN",
        out_dir = DIR_OUTPUT_CSV
      )


      # Macroeconomics output----
      ## *Capital stock ----
      CS %>%
        FAO_AREA_DISAGGREGATE_HIST_DISSOLUTION_ALL(SUDAN2012_MERGE = T) %>%
        spread(year, value) %>%
        add_title("GCAMFAOSTAT_CapitalStock") %>%
        add_units("2015 USD") %>%
        add_comments("gcamfaostat Export CSV") %>%
        add_precursors("CS",
                       "yfaostat_GCAM_GCAMFAOSTAT_CSV") ->
        GCAMFAOSTAT_CapitalStock

      assertthat::assert_that(grepl("2015", unique(GCAMFAOSTAT_CapitalStock$element)), msg = "Check FAO units")
      add_to_output_meta(.df = GCAMFAOSTAT_CapitalStock, "GCAMFAOSTAT_CapitalStock")

      output_csv_data(
        gcam_dataset = GCAMFAOSTAT_CapitalStock,
        out_filename = "GCAMFAOSTAT_CapitalStock",
        col_type_nonyear = "iiciccc",
        title = "FAO macroeconomic metrics of capital shocks by country_year",
        unit = "2015 USD",
        description = "Data is preprocessed and generated by gcamfaostat",
        code = "CS",
        out_dir = DIR_OUTPUT_CSV)


      ## *GDP ----

      MK %>%
        FAO_AREA_DISAGGREGATE_HIST_DISSOLUTION_ALL(SUDAN2012_MERGE = T) %>%
        # area_code_Belgium_Luxembourg had been separated in the data for before 2000
        # so remove na
        filter(!is.na(year)) %>%
        spread(year, value) %>%
        add_title("GCAMFAOSTAT_CapitalStock") %>%
        add_units("2015 USD or nominal") %>%
        add_comments("gcamfaostat Export CSV") %>%
        add_precursors("MK",
                       "yfaostat_GCAM_GCAMFAOSTAT_CSV") ->
        GCAMFAOSTAT_GDP

      assertthat::assert_that(grepl("2015", unique(GCAMFAOSTAT_GDP$element)) %>% any(), msg = "Check FAO units")
      add_to_output_meta(.df = GCAMFAOSTAT_GDP, "GCAMFAOSTAT_GDP")

      output_csv_data(
        gcam_dataset = GCAMFAOSTAT_GDP,
        out_filename = "GCAMFAOSTAT_GDP",
        col_type_nonyear = "iiciccc",
        title = "FAO real and nominal GDP since 1970",
        unit = "2015 million USD or nominal million USD",
        code = "MK",
        description = "Data is preprocessed and generated by gcamfaostat.",
        out_dir = DIR_OUTPUT_CSV,
        GZIP = F)


      # GCAMFAOSTAT_metadata ----
      output_csv_data(
        gcam_dataset = yfaostat_GCAM_GCAMFAOSTAT_CSV,
        out_filename = "GCAMFAOSTAT_metadata",
        col_type_nonyear = "ciic",
        title = "Information of dataset exported by gcamfaostat",
        unit = "NA",
        description = "Data is preprocessed and generated by gcamfaostat v1.1.0",
        code = "NA",
        out_dir = DIR_OUTPUT_CSV)

      }
    else {

        lapply(MODULE_OUTPUTS[MODULE_OUTPUTS %>% names() == "CSV"],
               function(output){
                 assign(output, empty_data() %>%
                          add_title(output) %>%
                          add_precursors("yfaostat_GCAM_GCAMFAOSTAT_CSV"), envir =  Curr_Envir)
               })
        }

    yfaostat_GCAM_GCAMFAOSTAT_CSV %>%
      add_title("Export CSV to DIR_OUTPUT_CSV") %>%
      add_units("NA") %>%
      add_comments("Export CSV") %>%
      add_precursors(MODULE_INPUTS) ->
      yfaostat_GCAM_GCAMFAOSTAT_CSV


    return_data(MODULE_OUTPUTS)

  } else {
    stop("Unknown command")
  }
}
