# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_aglu_L100_FAOSTAT_SUA_PrimaryEquivalent
#'
#' Generate supply utilization balance in primary equivalent
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs, a vector of output names, or (if
#'   \code{command} is "MAKE") all the generated outputs: \code{GCAM_AgLU_SUA_APE_2010_2019},
#'   \code{FAO_AgProd_Kt_All},\code{FAO_AgArea_Kha_All},\code{FAO_Food_Macronutrient_All_2010_2019},
#'   \code{FAO_Food_MacronutrientRate_2010_2019_MaxValue}
#' @details This chunk compiles balanced supply utilization data in primary equivalent in GCAM region and commodities.
#' A method to generate primary equivalent is created for the new FAOSTAT supply utilization data (2010 to 2019).
#' New SUA balance is connected to the old one (before 2010). Production and harvested area data with FAO region and item
#' for primary production are provided. For FAO food items, macronutrient values are calculated at SUA item level.
#' Data processing was consistent across scales. Note that GCAM regions and commodities in aggregation mapping can
#' be changed in corresponding mappings. The output data is not averaged over time.
#' @importFrom assertthat assert_that
#' @importFrom dplyr bind_rows filter if_else inner_join left_join mutate rename select n group_by_at vars case_when arrange
#' @importFrom tidyr complete drop_na gather nesting spread replace_na
#' @importFrom tibble tibble
#' @author XZ 2022
module_aglu_L100_FAOSTAT_SUA_PrimaryEquivalent <- function(command, ...) {

  MODULE_INPUTS <-
    c(FILE = "aglu/AGLU_ctry",
      FILE = "common/iso_GCAM_regID",
      FILE = "common/GCAM_region_names",
      FILE = "aglu/FAO/GCAMDATA_FAOSTAT_SUA_195Regs_530Items_2010to2019",
      FILE = "aglu/FAO/GCAMDATA_FAOSTAT_BiTrade_194Regs_400Items_2010to2020",
      FILE = "aglu/FAO/Mapping_SUA_PrimaryEquivalent",
      FILE = "aglu/FAO/GCAMDATA_FAOSTAT_ProdArea_195Regs_271Prod160AreaItems_1973to2020",
      FILE = "aglu/FAO/GCAMDATA_FAOSTAT_FBSH_CB_173Regs_118Items_1973to2009",
      FILE = "aglu/FAO/Mapping_item_FBS_GCAM",
      FILE = "aglu/FAO/FAO_ag_items_PRODSTAT",
      FILE = "aglu/FAO/FAO_an_items_PRODSTAT",
      FILE = "aglu/FAO/GCAMDATA_FAOSTAT_MacroNutrientRate_179Regs_426Items_2010to2019Mean"
    )

  MODULE_OUTPUTS <-
    c("GCAM_AgLU_SUA_APE_2010_2019",
      "FAO_AgProd_Kt_All",
      "FAO_AgArea_Kha_All",
      "FAO_Food_Macronutrient_All_2010_2019",
      "FAO_Food_MacronutrientRate_2010_2019_MaxValue")

  if(command == driver.DECLARE_INPUTS) {
    return(MODULE_INPUTS)
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(MODULE_OUTPUTS)
  } else if(command == driver.MAKE) {

    year <- value <- Year <- Value <- FAO_country <- iso <- NULL    # silence package check.

    all_data <- list(...)[[1]]

    # Load required inputs ----

    lapply(MODULE_INPUTS, function(d){
      # get name as the char after last /
      nm <- tail(strsplit(d, "/")[[1]], n = 1)
      # get data and assign
      assign(nm, get_data(all_data, d, strip_attributes = T),
             envir = parent.env(environment()))  })

    # Section1: [2010-2019] Region aggregation of supply-utilization-accounting data ----

    # 1.1. Helper functions for SUA regional aggregation ----

    #' Adjust gross trade in SUA data to ensure regional export is smaller than production for an SUA item
    #' @description The function is used as an option in SUA_REG_Agg
    #' @param .DF input supply-utilization accounting data frame
    #' @return Trade-adjusted supply-utilization accounting data frame

    RM_GTOSS_TRADE_TRIANGLE_INEQUALITY <- function(.DF){

      # need to remove gross trade when export > production
      # to maintain triangle the inequality rule
      .DF %>% group_by(area, year, item) %>%
        # Calculate GrossTradeRM when prod - export < 0 and element in export and import
        # Note that when import < export, e.g., export from stock or discrepancy, import is used for adjustments
        mutate(GrossTradeRM = pmax(value[element == "Production"] - value[element == "Export"],
                                   -value[element == "Import"])) %>%
        mutate(GrossTradeRM = ifelse(GrossTradeRM < 0 & element %in% c("Export", "Import"),
                                     GrossTradeRM, 0)) %>%
        # Remove both import and export
        mutate(value = value + GrossTradeRM) %>% select(-GrossTradeRM) %>%
        ungroup()
    }


    #' Regional SUA aggregation (e.g., 32 in GCAM) with an option of removing intra-regional trade
    #' @description intra-regional trade, by default, removed only for the SUA items with bilateral trade data available.
    #' E.g., for 32 regions, if removed, global trade becomes inter-32-region trade.
    #' @param .DF_SUA input supply-utilization accounting data frame
    #' @param .RM_IntraRegTrade If TRUE, intra-regional trade is removed after aggregation
    #' @param .TM_Bilateral A data frame of bilateral trade data
    #' @param .RM_TRIANGLE_INEQUALITY If TRUE, adjust gross trade in SUA data to ensure regional export is smaller than production for an SUA item
    #' @return

    SUA_REG_Agg <- function(.DF_SUA,
                            .RM_IntraRegTrade = TRUE,
                            .TM_Bilateral,
                            .RM_TRIANGLE_INEQUALITY = TRUE) {
      # Assertion
      # iso_GCAM_regID specified regional aggregation mapping
      # Need to get intra-regional area-source pairs based on the info
      assertthat::assert_that(is.data.frame(AGLU_ctry))
      assertthat::assert_that(is.data.frame(iso_GCAM_regID))
      assertthat::assert_that(is.data.frame(GCAM_region_names))
      if (.RM_IntraRegTrade == TRUE) {
        assertthat::assert_that(is.data.frame(.TM_Bilateral))
      }
      if (.RM_TRIANGLE_INEQUALITY == TRUE) {
        assertthat::assert_that(is.function(RM_GTOSS_TRADE_TRIANGLE_INEQUALITY))
      }


      # Aggregation based on region mapping ----
      .DF_SUA %>%
        # Join GCAM region mappings
        gcamdata::left_join_error_no_match(AGLU_ctry %>% select(area = FAO_country, iso), by = "area") %>%
        gcamdata::left_join_error_no_match(iso_GCAM_regID %>% select(iso, GCAM_region_ID), by = "iso") %>%
        gcamdata::left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
        dplyr::group_by_at(vars(area = region, year, item, element)) %>%
        summarise(value = sum(value), .groups = "drop") ->
        DF_SUA_Agg

      if (.RM_IntraRegTrade == TRUE) {
        ## If removing intra-regional trade after aggregation ----

        ### Get bilateral pairs that need subtraction  ----
        .TM_Bilateral %>% distinct(area, source) %>%
          gcamdata::left_join_error_no_match(AGLU_ctry %>% select(area = FAO_country, iso), by = "area") %>%
          gcamdata::left_join_error_no_match(iso_GCAM_regID %>% select(iso, GCAM_region_ID), by = "iso") %>%
          select(-iso) %>%
          gcamdata::left_join_error_no_match(AGLU_ctry %>% select(source = FAO_country, iso), by = "source") %>%
          gcamdata::left_join_error_no_match(iso_GCAM_regID %>% select(iso, Source_GCAM_region_ID = GCAM_region_ID),
                                             by = "iso") %>%
          filter(GCAM_region_ID == Source_GCAM_region_ID) %>%
          select(-GCAM_region_ID, -Source_GCAM_region_ID, -iso) ->
          TM_intra_REG_pair

        ### Get trade flow and aggregate to gross and aggregated region----
        .TM_Bilateral %>% inner_join(TM_intra_REG_pair, by = c("area", "source")) -> TM_bilateral_rm

        # gross trade agg.
        TM_bilateral_rm %>% # Trade is balanced if aggregating bilateral
          mutate(element = "Import") %>%
          group_by(area_code, area, item_code, item, element, year) %>%
          summarise(TCL = sum(value, na.rm = T), .groups = "drop") %>%
          bind_rows(
            TM_bilateral_rm %>%
              mutate(element = "Export") %>%
              group_by(
                area_code = source_code,
                area = source,
                item_code,
                item,
                element,
                year
              ) %>%
              summarise(TCL = sum(value, na.rm = T), .groups = "drop")
          ) %>%
          # regional agg.
          gcamdata::left_join_error_no_match(AGLU_ctry %>% select(area = FAO_country, iso), by = "area") %>%
          gcamdata::left_join_error_no_match(iso_GCAM_regID %>% select(iso, GCAM_region_ID), by = "iso") %>%
          left_join(GCAM_region_names, by = "GCAM_region_ID") %>%
          dplyr::group_by_at(vars(area = region, year, item, element)) %>%
          summarise(TCL = sum(TCL), .groups = "drop") ->
          TCL_rm

        ### Join aggregated data and subtract intra-regional trade ----
        ## be careful with the unit
        DF_SUA_Agg %>% left_join(TCL_rm, by = c("area", "year", "item", "element")) %>%
          mutate(value = if_else(is.na(TCL), value, value - TCL / 1000)) %>%
          select(-TCL) ->
          DF_SUA_Agg
      }

      if (.RM_TRIANGLE_INEQUALITY == TRUE) {
        ### Remove gross trade to comply with TRIANGLE_INEQUALITY ----
        ## Remove gross trade to ensure prod > export
        DF_SUA_Agg %>%
          RM_GTOSS_TRADE_TRIANGLE_INEQUALITY ->
          DF_SUA_Agg1
        return(DF_SUA_Agg1)
      } else {
        return(DF_SUA_Agg)
      }
    }


    # 1.2. Execution: regional aggregation ----

    # Get SUA data ready
    FAO_SUA_Kt_2010to2019 <-
      GCAMDATA_FAOSTAT_SUA_195Regs_530Items_2010to2019 %>%
      gather_years() %>%
      filter(!is.na(value)) # filter out nonexist regions years due to gather e.g., USSR after 1991
    # Get bilateral trade data ready
    FAO_BiTrade_Kt_2010to2019 <-
      GCAMDATA_FAOSTAT_BiTrade_194Regs_400Items_2010to2020 %>%
      gather_years() %>%
      filter(!is.na(value))
    ## Aggregate to *FAO_SUA_Kt_2010to2019_R ----
    FAO_SUA_Kt_2010to2019_R <-
      SUA_REG_Agg(
        .DF_SUA =  FAO_SUA_Kt_2010to2019,
        .RM_IntraRegTrade = TRUE,
        .TM_Bilateral = FAO_BiTrade_Kt_2010to2019
      )
    ## Clean up
    rm(FAO_BiTrade_Kt_2010to2019)
    ## Done Section1 ----
    #****************************----

    # Section2: [2010-2019] Primary equivalent aggregation to GCAM commodities ----

    # 2.1 Helper functions for SUA primary equivalent aggregation ----

    #' Check and assert SUA data frame structure
    #' @param .DF Input supply-utilization accounting data frame
    #' @return Logical T or F

    is.SUA <- function(.DF){
      # Generate SUA class

      assertthat::assert_that(is.data.frame(.DF))

      # Data col names check
      assertthat::assert_that(all( c("element", "value")%in% names(.DF)))
      assertthat::assert_that(any( c("item", "item_code", "GCAM_commodity")%in% names(.DF)))

      # Has all elements
      c("Opening stocks", "Production", "Import",
        "Export", "Processed", "Food", "Feed", "Seed", "Other uses", "Loss", "Closing stocks",
        "Stock Variation") ->
        Bal_element

      assertthat::assert_that( all(Bal_element %in% unique(.DF$element)))

    }

    #' Get extraction rate
    #' @description Gross extraction rate is calculated for domestic, traded, and lagged values.
    #' By gross, it means sink items are aggregated.
    #' The function is used in Proc_primarize.
    #' @param .DF Input supply-utilization accounting data frame with one tier of processing
    #' @param .source_item Source items or primary items in the processing
    #' @param .sink_item Sink items or processed items in the processing
    #' @return A data frame including regional, traded, and world extraction rates of a processing

    Get_GROSS_EXTRACTION_RATE <- function(.DF, .source_item, .sink_item){

      assertthat::assert_that(is.SUA(.DF))
      assertthat::assert_that(is.data.frame(Mapping_SUA_PrimaryEquivalent))

      Mapping_SUA_PrimaryEquivalent %>% filter(source_item %in% .source_item) %>%
        distinct(extraction_rate_Q25, Q25asMin) %>%
        mutate(minimium_extraction_rate = if_else(Q25asMin == T, extraction_rate_Q25, 0)) %>%
        pull(minimium_extraction_rate) -> MIN_EXTRACTION_RATE

      .DF %>%
        #Prepare data to calculate regional, traded, and world average extraction rates
        filter((element %in% c("Production", "Export") & item %in% .sink_item) |
                 (element == "Processed" & item %in% .source_item)) %>%
        group_by(area, year, element) %>%
        # sum across year and also sink or source items
        summarise(value = sum(value), .groups = "drop") %>%
        spread(element, value) %>%
        group_by(year) %>%
        mutate(extraction_rate_world = sum(Production) / sum(Processed)) %>%
        # in case sum(Processed) or sum(Production) == 0
        mutate(extraction_rate_world = if_else(extraction_rate_world != 0 & is.finite(extraction_rate_world),
                                               extraction_rate_world, 1)) %>%
        # Regional extraction rate = prod of an aggregated processed item  / Processed use of an aggregated primary item
        # Use world average to fill in NA or zero
        mutate(extraction_rate = if_else(is.na(Production / Processed), extraction_rate_world, Production / Processed)) %>%
        mutate(extraction_rate = if_else(extraction_rate == 0, extraction_rate_world, extraction_rate)) %>%
        # Using minimum extraction rate here
        mutate(minimium_extraction_rate = MIN_EXTRACTION_RATE) %>%
        mutate(extraction_rate = pmax(MIN_EXTRACTION_RATE, extraction_rate)) %>%
        # Calculate traded extraction rate
        mutate(extraction_rate_trade = sum(Export) / sum(Export / extraction_rate),
               extraction_rate_trade = if_else(is.na(extraction_rate_trade), extraction_rate, extraction_rate_trade),
               # both processed and production > 0
               positive_prod = if_else(Production >0 & Processed > 0 , T, F) ) %>%
        ungroup() %>%
        # Calculate lagged extraction_rate but replace NA with current rate (first period)
        group_by(area) %>%
        mutate(extraction_rate_lag = lag(extraction_rate)) %>%
        mutate(extraction_rate_lag = if_else(is.na(extraction_rate_lag), extraction_rate, extraction_rate_lag)) %>%
        ungroup() %>%
        select(area, year, extraction_rate, extraction_rate_lag, extraction_rate_trade, extraction_rate_world, positive_prod)
    }


    #' Separate the SUA balance into domestic and imported balanced for sink_item
    #' @description The function is used in Proc_primarize
    #' @param .DF Input supply-utilization accounting data frame with one tier of processing
    #' @param .SINK_ITEM Sink items or processed items in the processing
    #' @return SUA DF

    Get_ARMINGTON_BALANCE <- function(.DF, .SINK_ITEM){

      assertthat::assert_that(is.SUA(.DF))
      Import_Demand_Item <- c("Food", "Feed", "Processed", "Other uses", "Seed", "Loss")

      .DF %>% filter(item %in% .SINK_ITEM) %>%
        group_by(area, year, item) %>%
        # Calculate imported consumption share
        # The assumption is that a portion of Import_Demand_Items was imported
        # so they need to be scaled by an international extraction rate
        # Note that stock variation is not included in import consumption to maintain stock balance
        # so additional adjustment may be needed
        mutate(Import_Demand_Share = value[element == "Import"] /
                 sum(value[element %in% Import_Demand_Item])) %>%
        # replace NA and inf
        mutate(Import_Demand_Share = if_else(!is.finite(Import_Demand_Share), 0,
                                             Import_Demand_Share)) %>%
        # The share should be small than 1 though outlier regions may import for storage
        mutate(Import_Demand_Share = pmin(Import_Demand_Share, 1)) %>%
        # when Import_Demand_Item consumption < Import they are used to share out Import consumptions
        # otherwise, Residuals is used for adjustments
        mutate(bal_import = case_when(
          element %in% c("Import") ~ value,
          element %in% Import_Demand_Item ~ Import_Demand_Share * value,
          TRUE ~ 0) ) %>%
        mutate(bal_import = if_else(element %in% c("Residuals"),
                                    bal_import[element == "Import"] - sum(bal_import[element %in% Import_Demand_Item]),
                                    bal_import )) %>%
        select(-Import_Demand_Share) %>%
        # Calculate domestic balance
        mutate(bal_domestic = value - bal_import) %>%
        select(-value) %>% ungroup() %>%
        tidyr::gather(bal_source, value, bal_import, bal_domestic) %>%
        # Clean the bal items
        spread(element, value) %>%
        mutate(`Regional supply` = `Opening stocks` + Production + `Import`,
               `Regional demand` = `Export` + Feed + Food + Loss + Processed + Seed + `Other uses` +`Closing stocks`,
               Residuals = `Regional supply` -  `Regional demand`) %>%
        tidyr::gather(element, value, -area, -item, -year, -bal_source)

    }


    #' Separate the domestic SUA balance into current and lagged balanced for sink_item
    #' @description The function is used in Proc_primarize
    #' @param .DF Output from Get_ARMINGTON_BALANCE. Input supply-utilization accounting data frame with one tier of processing and
    #' @param .SINK_ITEM Sink items or processed items in the processing
    #' @return SUA DF

    Get_STOCK_BALANCE <- function(.DF, .SINK_ITEM){

      assertthat::assert_that(is.SUA(.DF))
      Opening_Stock_Item <- c("Food", "Feed", "Processed", "Other uses", "Seed", "Loss")

      .DF %>% filter(item %in% .SINK_ITEM,
                     bal_source == "bal_domestic") %>%
        select(-bal_source) %>%
        group_by(area, year, item) %>%
        mutate(Ostock = value[element == "Opening stocks"] ) %>%
        mutate(Opening_Stock_Demand_Share = Ostock /
                 sum(value[element %in% Opening_Stock_Item]))  %>%
        mutate(Opening_Stock_Demand_Share = if_else(Ostock == 0, 0,
                                                    Opening_Stock_Demand_Share)) %>%
        replace_na(list(Opening_Stock_Demand_Share = 1)) %>%
        # The share should be small than 1
        # Other elements will be adjusted if not
        mutate(Opening_Stock_Demand_Share = pmin(Opening_Stock_Demand_Share, 1)) %>%
        mutate(bal_domestic_lag = case_when(
          element %in% c("Opening stocks") ~ value,
          element %in% Opening_Stock_Item ~ Opening_Stock_Demand_Share * value,
          TRUE ~ 0) ) %>%
        mutate(bal_domestic_lag = if_else(element %in% c("Residuals"),
                                          bal_domestic_lag[element == "Opening stocks"] - sum(bal_domestic_lag[element %in% Opening_Stock_Item]),
                                          bal_domestic_lag )) %>%

        select(-Opening_Stock_Demand_Share, -Ostock) %>%
        # Calculate domestic balance
        mutate(bal_domestic_current = value - bal_domestic_lag) %>%
        select(-value) %>% ungroup() %>%
        tidyr::gather(bal_source, value, bal_domestic_lag, bal_domestic_current) %>%
        # Clean the bal items
        spread(element, value) %>%
        mutate(`Regional supply` = `Opening stocks` + Production + `Import`,
               `Regional demand` = `Export` + Feed + Food + Loss + Processed + Seed + `Other uses` +`Closing stocks`,
               `Stock Variation` = `Closing stocks` - `Opening stocks`,
               Residuals = `Regional supply` -  `Regional demand`) %>%
        tidyr::gather(element, value, -area, -item, -year, -bal_source) %>%
        bind_rows(
          .DF %>% filter(item %in% .SINK_ITEM,
                         bal_source != "bal_domestic")  )
    }


    #' Primary equivalent aggregation
    #' @param .DF Input supply-utilization accounting data frame with one tier of processing
    #' @param .SOURCE_ITEM Source items or primary items in the processing
    #' @param .SINK_ITEM Sink items or processed items in the processing
    #' @param .OUTPUT_SPECIFIC_EXTRACTION_RATE A data frame with item and output_specific_extraction_rate.
    #' # In some cases, prescale sink item SUA using output_specific_extraction_rate can improve the processing.
    #' # e.g., when coproduction shares are not fixed.
    #' @return A supply-utilization accounting data frame of a data tier in source item equivalent

    Proc_primarize <- function(.DF, .SOURCE_ITEM, .SINK_ITEM,
                               .OUTPUT_SPECIFIC_EXTRACTION_RATE = NA){

      ## Assertion
      assertthat::assert_that(is.SUA(.DF))
      assertthat::assert_that(is.character(.SOURCE_ITEM))
      assertthat::assert_that(is.character(.SINK_ITEM))
      # .DF has all items needed
      assertthat::assert_that(all(c(.SOURCE_ITEM, .SINK_ITEM) %in% c(.DF %>% ungroup() %>%  distinct(item) %>% pull)))
      assertthat::assert_that(is.function(Get_ARMINGTON_BALANCE))
      assertthat::assert_that(is.function(Get_STOCK_BALANCE))
      assertthat::assert_that(is.function(Get_GROSS_EXTRACTION_RATE))

      ## a. Pre-scale sink item data when .OUTPUT_SPECIFIC_EXTRACTION_RATE is available ----
      if (is.data.frame(.OUTPUT_SPECIFIC_EXTRACTION_RATE)) {
        .DF %>% left_join(.OUTPUT_SPECIFIC_EXTRACTION_RATE, by = "item") %>%
          replace_na(list(output_specific_extraction_rate = 1)) %>%
          mutate(value = value / output_specific_extraction_rate) %>%
          select(-output_specific_extraction_rate) ->
          .DF
      }

      ## b. For the sink items of the tier, separate balance into domestic and imported ----
      # Note that the method here relies on Get_GROSS_EXTRACTION_RATE and Get_ARMINGTON_BALANCE
      .DF %>% Get_ARMINGTON_BALANCE(.SINK_ITEM) %>%
        Get_STOCK_BALANCE(.SINK_ITEM) %>%
        # Join extraction rate for the tier
        gcamdata::left_join_error_no_match(
          # Get extraction rate for domestic and traded
          # Note that extraction rates are mean values across time
          # Note that regional extraction rate could be inf
          # It is likely due to data inconsistency, e.g., zero processed in source but positive sink
          # No adjustments were made since 1/inf become zero in the scaling process, preserving primary balance
          .DF %>% Get_GROSS_EXTRACTION_RATE(.SOURCE_ITEM, .SINK_ITEM) %>%
            select(area, year,
                   bal_domestic_current = extraction_rate,
                   bal_domestic_lag = extraction_rate_lag,
                   bal_import = extraction_rate_trade) %>%
            tidyr::gather(bal_source, extraction_rate, bal_domestic_current, bal_domestic_lag, bal_import),
          by = c("area", "year", "bal_source")
        ) %>%
        # Scale sink items to get source item equivalent
        mutate(value = if_else(item %in% .SINK_ITEM, value / extraction_rate, value)) %>%
        select(-extraction_rate) -> .df1

      ## c. Aggregate sink_items are aggregated into "sink_item" ----
      # And production & processed are adjusted for primary aggregation
      # Bind source items as well
      .df1 %>% mutate(item = replace(item, item %in% .SINK_ITEM, "sink_item")) %>%
        group_by(area, year, item, element) %>%
        summarise(value = sum(value), .groups = "drop") %>%
        # Move production to negative processed for aggregation
        group_by(area, year, item) %>%
        mutate(value = if_else(item == "sink_item" & element %in% c("Production", "Processed"),
                               value - value[element == "Production"], value)) %>%
        ungroup() %>%
        # Bind source items and other items not used
        bind_rows(.DF %>% filter(!item %in% .SINK_ITEM))-> .df2

      ## d. Merge sink SUA into source items SUA  ----
      # Note that with multiple source items, sinks are aggregated into sources based on average processed shares across sources
      # Prepare data to calculate world average source share
      .df2 %>% filter(element == "Processed", item %in% .SOURCE_ITEM)  %>%
        group_by(item) %>% mutate(value = sum(value)) %>% ungroup() %>%
        group_by(area, year) %>%
        mutate(share = value/sum(value)) %>%
        # in case of inf. use simple share
        mutate(share = if_else(is.finite(share), share, dplyr::n()/sum(dplyr::n()))) %>%
        ungroup() %>%
        select(-element, - value) %>%
        # full join is needed since item (left) and element(right) are needed
        full_join(.df2 %>% filter(item == "sink_item") %>%
                    select(-item), by = c("area", "year")) %>%
        mutate(value = share * value) %>% select(-share) %>%
        # Bind source item and aggregated across source & sink items based on primary equivalent
        bind_rows(.df2 %>% filter(item != "sink_item")) %>%
        group_by(area, year, item, element) %>%
        summarise(value = sum(value), .groups = "drop") -> .df3

      .df3 %>%
        spread(element, value) %>%
        mutate(`Regional supply` = `Opening stocks` + Production + `Import`,
               `Regional demand` = `Export` + Feed + Food + Loss + Processed + Seed + `Other uses` +`Closing stocks`,
               Residuals = `Regional supply` -  `Regional demand`) %>%
        tidyr::gather(element, value, -area, -item, -year) ->
        .df4

      ## e. Return data
      return(.df4)

      ## Done Proc_primarize function

    }


    ### FN for SUA primary equivalent aggregation

    #' Recursively aggregate nest into primary equivalent using from bottom to top
    #' @description The function is a recursive wrapper of Proc_primarize
    #' @param .COMM Primary commodity in the FAO item mapping (APE_comm).
    #' @param .MAPPING Mapping of SUA items to primary products by level of nestings. Mapping_SUA_PrimaryEquivalent was constructed.
    #' @param .DF_SUA Input SUA data frame
    #' @param .ITEM_SUFFIX Adding a suffix (_APE) for item in primary equivalent in output. Note that an aggregated primary commodity is also kept.
    #' @param .RECUR_TOP_NEST Bottom nest_level in mapping. If 1, all the way through APE but if 2 only run to top but 1 nest
    #' @return SUA data frame in primary equivalent

    recursively_primarize <- function(.COMM,
                                      .MAPPING = Mapping_SUA_PrimaryEquivalent,
                                      .DF_SUA = Bal_new_all,
                                      .ITEM_SUFFIX = "_APE",
                                      .RECUR_TOP_NEST = 1
    ) {


      ## Assertion
      # Need Proc_primarize exist
      assertthat::assert_that(is.function(Proc_primarize))
      assertthat::assert_that(is.character(.COMM))
      # Required format for .MAPPING
      assertthat::assert_that(is.data.frame(.MAPPING))
      assertthat::assert_that(all(c("APE_comm", "nest_level",
                                    "source_item", "sink_item",
                                    "output_specific_extraction_rate") %in% names(.MAPPING)))

      ## a. Get data for all commodities, both primary and processed, needed ----
      COMM_ALL <-
        .MAPPING %>%
        filter(APE_comm == .COMM) %>%
        select(source_item, sink_item) %>% unlist() %>%
        unique()
      assertthat::assert_that(length(COMM_ALL) >0, msg = "Item not exist; check mapping")
      # Subset of relevant data only and assert all item need exist
      assertthat::assert_that(is.data.frame(.DF_SUA))
      DF_BAL <-
        .DF_SUA %>% filter(item %in% COMM_ALL)
      assertthat::assert_that(length(setdiff(COMM_ALL, unique(DF_BAL$item))) == 0)


      ## b. Get total number of nests based on nest_level in .MAPPING ----
      .MAPPING %>%
        filter(APE_comm == .COMM) %>% pull(nest_level) %>% max ->
        nNests

      ## c. Get primary commodities set ----
      .MAPPING %>% filter(source_primary == TRUE) %>%
        distinct(source_item) %>% pull  %>%
        intersect(COMM_ALL) ->
        COMM_primary

      ## d. Wrapper function of Proc_primarize to use nest_level and mapping inputs----
      Proc_primarize1 <- function(.DF, .COMM, .NEST_LEVEL){

        .MAPPING %>%
          filter(APE_comm == .COMM, nest_level == .NEST_LEVEL) ->
          .SUA

        ### Get OUTPUT_SPECIFIC_EXTRACTION_RATE ----
        .SUA %>%
          filter(!is.na(output_specific_extraction_rate)) %>%
          select(item = sink_item, output_specific_extraction_rate) ->
          OUTPUT_SPECIFIC_EXTRACTION_RATE

        .DF %>% Proc_primarize(.SOURCE_ITEM = unique(.SUA$source_item),
                               .SINK_ITEM = unique(.SUA$sink_item),
                               .OUTPUT_SPECIFIC_EXTRACTION_RATE = OUTPUT_SPECIFIC_EXTRACTION_RATE)
      }

      ## e. A smart recursive function ----
      recursively_repeat <- function(.x, .reps, Proc_primarize1) {
        if (.reps == .RECUR_TOP_NEST - 1 ) {
          .x
        } else {
          recursively_repeat(Proc_primarize1(.x, .COMM,
                                             .NEST_LEVEL = .reps),
                             .reps - 1, Proc_primarize1)
        }
      }

      ## f. Execute the recursive function to realize recursively_primarize ----
      # through all nests
      recursively_repeat(DF_BAL, .reps = nNests, Proc_primarize1) -> DF_BAL_PE

      ## g. Return data ----
      # mutate APE item if .RECUR_TOP_NEST == 1 and
      # also bind primary commodity balance
      # otherwise, keep item names
      if (.RECUR_TOP_NEST == 1) {
        return(DF_BAL_PE %>%
                 # Update APE item name
                 mutate(item = paste0(.COMM, .ITEM_SUFFIX)) %>%
                 # Aggregate APE items when needed
                 group_by(area, year, item, element) %>%
                 summarise(value = sum(value), .groups = "drop") %>%
                 ungroup() %>%
                 # Keep an aggregated primary commodity
                 bind_rows(DF_BAL %>%
                             filter(item %in% COMM_primary) %>%
                             mutate(item = .COMM) %>%
                             group_by(area, year, item, element) %>%
                             summarise(value = sum(value), .groups = "drop") %>%
                             ungroup())  )
      } else {
        return(DF_BAL_PE)
      }
      ## Done recursively_primarize function
    }

    # 2.2. Execution: process data into APE ----


    ## Loop through all GCAM_commodity with available data ----
    Mapping_SUA_PrimaryEquivalent %>% distinct(GCAM_commodity) %>% pull ->
      GCAM_commodity

    lapply(GCAM_commodity, function(.GCAM_COMM){

      ### Assert that items needed included in data
      assertthat::assert_that(
        Mapping_SUA_PrimaryEquivalent %>%
          filter(GCAM_commodity == .GCAM_COMM) %>%
          distinct(sink_item) %>% pull %>%
          setdiff(FAO_SUA_Kt_2010to2019_R %>% pull(item)) %>% length() == 0
      )
      ### Loop and bind
      Mapping_SUA_PrimaryEquivalent %>%
        filter(GCAM_commodity == .GCAM_COMM) %>%
        distinct(APE_comm) %>% pull %>%
        # Get the APE_comm and enter them into recursively_primarize
        lapply(FUN = recursively_primarize,
               .DF_SUA = FAO_SUA_Kt_2010to2019_R) %>%
        bind_rows()  %>%
        # Aggregate APE_comm into GCAM_commodity
        mutate(GCAM_commodity = if_else(grepl("_APE", item), .GCAM_COMM,
                                        paste0(.GCAM_COMM, "_Primary"))) %>%
        # Keep APE balance only
        filter(grepl("_APE", item)) %>%
        group_by(region = area, year, GCAM_commodity, element) %>%
        summarise(value = sum(value), .groups = "drop")
    }) %>% bind_rows() ->
      GCAM_APE_after2010

    rm(FAO_SUA_Kt_2010to2019_R)

    ## Done Section2 ----
    #****************************----


    # Section4 [1970-2019] GCAM_APE SUA ----

    # 4.1. Helper functions ----
    Check_Balance_SUA <- function(.DF){

      assertthat::assert_that(all(c("element") %in% names(.DF)))
      assertthat::assert_that(all(c("Import", "Export", "Production",
                                    "Food", "Feed", "Other uses") %in%
                                    c(.DF %>% distinct(element) %>% pull)))
      # 0. Check NA
      if (isTRUE(.DF %>% filter(is.na(value)) %>% nrow == 0)) {
        message("Good! No NA in value") } else{
          warning("NA in value")
        }


      # 1. Positive value except stock variation and residues
      if (isTRUE(.DF %>% filter(!element %in% c("Stock Variation", "Other uses")) %>%
                 summarise(min = min(value, na.rm = T)) %>% pull(min) >= -0.001)) {
        message("Good! Signs checked") } else{
          warning("Negative values in key elements (not including stock variation and other uses)")
        }

      # 2. Trade balance in all year and items
      if (isTRUE(.DF %>% filter(element %in% c("Import", "Export")) %>%
                 group_by(year, GCAM_commodity, element) %>%
                 summarise(value = sum(value), .groups = "drop") %>%
                 spread(element, value) %>% filter(abs(Import - Export) > 0.0001) %>% nrow() == 0)) {
        message("Good! Gross trade in balance") } else{
          warning("Gross trade imbalance")
        }

      # 3. SUA balance check
      if (isTRUE(.DF %>%
                 spread(element, value) %>%
                 mutate(`Regional supply` = Production + `Import`,
                        `Regional demand` = `Export` + Feed + Food  + `Other uses`,
                        bal = abs(`Regional supply` -  `Regional demand`)) %>%
                 filter(bal > 0.0001) %>% nrow() == 0)) {
        message("Good! Regional supply = Regional demand + Residuals") } else{
          warning("Regional supply != Regional demand + Residuals")
        }

      # 4. Balanced in all dimensions
      assertthat::assert_that(.DF %>% nrow() ==
                                .DF %>% distinct(year) %>% nrow *
                                .DF %>% distinct(GCAM_commodity) %>% nrow *
                                .DF %>% distinct(element) %>% nrow *
                                .DF %>% distinct(region) %>% nrow)

    }

    # 4.2. Connect and bind data from two periods ----

    GCAM_AgLU_SUA_APE_2010_2019 <-
      GCAM_APE_after2010 %>%
      mutate(unit = "1000 tonnes") %>%
      # clean and aggregate elements not using
      filter(!element %in% c("Regional demand", "Regional supply",
                             "Opening stocks", "Closing stocks")) %>%
      mutate(element = replace(element,
                               element %in% c("Stock Variation", "Processed",
                                              "Seed", "Residuals", "Loss"),
                               "Other uses")) %>%
      dplyr::group_by_at(dplyr::vars(-value)) %>%
      summarise(value = sum(value), .groups = "drop")

    ## Check balance
    GCAM_AgLU_SUA_APE_2010_2019 %>% Check_Balance_SUA
    rm(GCAM_APE_after2010)


    ## Done Section4 ----
    #****************************----

    # Section5 [1970-2019] Connect production and area data ----

    # This section gets crop and livestock production before aggregation (FAO region and items)
    # For both before 2010 and after 2010
    # They are also aggregated to GCAM region and commodities to assert consistency
    # The processing includes all crops (including fodder crops) and livestock items

    # 5.1. Get all mapping straight ----

    Primary_Item_CROP <-
      FAO_ag_items_PRODSTAT %>%
        select(item, GCAM_commodity, GCAM_subsector) %>%
        filter(!is.na(item), !is.na(GCAM_commodity)) %>%
        # Fodder grass has a duplicate as it mapped to different GTAP crops
        distinct %>%
        mutate(CropMeat = if_else(GCAM_commodity %in% c("FodderGrass", "FodderHerb"),
                                  "Crop_Fodder", "Crop_NonFodder"))
    assertthat::assert_that(
      all(Primary_Item_CROP %>% filter(CropMeat == "Crop_NonFodder") %>%  pull(item) %in%
        c(Mapping_SUA_PrimaryEquivalent %>% filter(source_primary == T) %>%
          distinct(item = source_item) %>% pull)),
      msg = "Inconsistent mapping of primary crops between FAO_ag_items_PRODSTAT and Mapping_SUA_PrimaryEquivalent" )

    Primary_Item_MEAT <-
      Mapping_SUA_PrimaryEquivalent %>%
      # animal meat Eq since they are included as primary production after 2010
      filter(source_primary == T | grepl("MeatEq", APE_comm)) %>%
      distinct(GCAM_commodity, item = source_item) %>%
      filter(GCAM_commodity %in%
               c(FAO_an_items_PRODSTAT %>%
                   filter(!is.na(GCAM_commodity)) %>%
                   distinct(GCAM_commodity) %>% pull))%>%
      mutate(CropMeat = "Meat")

    # 5.2. Get primary production for all ----
    # Connecting, mapping, arrange, and assertion

    ## Bind production and area data for both fodder and nonfodder ----
    FAO_AgProd_Kt_Area_Kha <-
      GCAMDATA_FAOSTAT_ProdArea_195Regs_271Prod160AreaItems_1973to2020 %>%
      gather_years() %>%
      filter(!is.na(value))

    # Assert yield no inf.
    assertthat::assert_that(
      # only safeguard here as data was cleaned and area and prod are matched
      FAO_AgProd_Kt_Area_Kha %>%
        # filter only primary crop items (all crops with area)
        filter(item_set %in% c("QCL_COMM_CROP_PRIMARY",
                               "QCL_COMM_CROP_PRIMARY_FODDER")) %>%
        select(-unit) %>% spread(element, value) %>%
        filter(is.infinite(Production / `Area harvested`|
                             is.infinite(`Area harvested`/Production)) ) %>%
        nrow == 0,
      msg = "Check region/item for prod > 0 & area = 0 or prod = 0 & area > 0" )

    # Assert primary production in two sources area consistent
    assertthat::assert_that(
      FAO_SUA_Kt_2010to2019 %>% filter(element == "Production") %>%
        inner_join(FAO_AgProd_Kt_Area_Kha %>%
                     filter(item_set != "QCL_COMM_OTHERPROC") %>%
                     filter(element == "Production") %>% rename(value1 = value),
                   by = c("area_code", "item_code", "element", "item", "area", "year")) %>%
        mutate(diff = abs(value1 - value)) %>%
        filter(diff > 0.0001) %>% nrow() == 0,
      msg = "Primary production in SUA (FAO_SUA_Kt_2010to2019) and
      QCL (FAO_AgProd_Kt_Area_Kha) are inconsistent "
    )

    ## a. All production ----
    # Meat production is more than (QCL)FAO_AgProd_Kt_Area_Kha after 2010
    # Production in FAO_AgProd_Kt_Area_Kha before 2010 was used
    FAO_AgProd_Kt_Area_Kha %>%
      filter(element == "Production") %>%
      filter(year < min(FAO_SUA_Kt_2010to2019$year)) %>%
      select(names(FAO_SUA_Kt_2010to2019)) %>%
      bind_rows(
        # For after 2010
        # Note that not all meat items came from QCL_PROD (unlike primary crops)
        # E.g., meat Eq, offals (livers chicken), etc. were from derivation or SCL
        # But all items should exist in Bal_new_all
        # And Bal_new_all is identical to QCL_PROD for primary productions
        FAO_SUA_Kt_2010to2019 %>%
          filter(element == "Production") ) %>%
      bind_rows(
        # bind fodder crops for after 2010
        FAO_AgProd_Kt_Area_Kha %>%
          filter(item_set == "QCL_COMM_CROP_PRIMARY_FODDER",
                 element == "Production") %>%
          filter(year >= min(FAO_SUA_Kt_2010to2019$year)) %>%
          select(names(FAO_SUA_Kt_2010to2019))
      ) %>%
      # Inner join works as filter here
      # Keep subsector info for crops
      inner_join(Primary_Item_CROP %>%
                   bind_rows(Primary_Item_MEAT %>%
                               mutate(GCAM_subsector = GCAM_commodity)),
                 by = "item") %>%
      # add in iso and gcam regions ID
      left_join_error_no_match(AGLU_ctry %>% select(area = FAO_country, iso), by = "area") %>%
      left_join_error_no_match(iso_GCAM_regID %>% select(iso, GCAM_region_ID), by = "iso") ->
      FAO_AgProd_Kt_All

    FAO_AgProd_Kt_All %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      dplyr::group_by_at(vars(region, year, GCAM_commodity, element, CropMeat)) %>%
      summarise(value = sum(value), .groups = "drop") ->
      QCL_PROD_GCAM

    assertthat::assert_that(
      GCAM_AgLU_SUA_APE_2010_2019 %>%
        filter(element == "Production") %>%
        left_join_error_no_match(
          QCL_PROD_GCAM %>% filter(CropMeat != "Crop_Fodder") %>%
            select(-CropMeat) %>%
            rename(value1 = value) %>%
            complete(nesting(region, year, element), GCAM_commodity, fill = list(value1 = 0)),
          by = c("region", "GCAM_commodity", "year", "element")) %>%
        mutate(diff = abs(value1 - value)) %>%
        filter(diff > 0.0001) %>% nrow() == 0,
        msg = "Primary production from two sources
              (GCAM_AgLU_SUA_APE_2010_2019 and FAO_AgProd_Kt_Area_Kha) are inconsistent." )

    ## b. All area harvested ----


    FAO_AgProd_Kt_Area_Kha %>%
      filter(element == "Area harvested") %>%
      select(names(FAO_SUA_Kt_2010to2019)) %>%
      # Keep subsector info for crops
      inner_join(Primary_Item_CROP, by = "item") %>%
      # add in iso and gcam regions ID
      left_join_error_no_match(AGLU_ctry %>% select(area = FAO_country, iso), by = "area") %>%
      left_join_error_no_match(iso_GCAM_regID %>% select(iso, GCAM_region_ID), by = "iso") ->
      FAO_AgArea_Kha_All

    #****************************----
    #Section6 Connect food items and macronutrient rates ----

    # 6.1 Separate FAO food items into GCAM food items and NEC for macronutrient ----
    # GCAM included most of the food items
    # All food item with available macronutrient info from FAOSTAT are included

    # a. Get all GCAM SUA items from the mapping by binding both source and sink items
    # about 486 items (out of 530) used in GCAM

    Mapping_SUA_PrimaryEquivalent %>%
      select(GCAM_commodity, item = source_item) %>%
      bind_rows(Mapping_SUA_PrimaryEquivalent %>%
                  select(GCAM_commodity, item = sink_item)) %>%
      distinct() %>% arrange(GCAM_commodity) ->
      SUA_Items_GCAM

    assertthat::assert_that(
      SUA_Items_GCAM %>% distinct(item) %>% nrow() == SUA_Items_GCAM %>% nrow(),
      msg = "Check duplicates in Mapping_SUA_PrimaryEquivalent SUA items"
    )

    # highly processed products or other products are not included in GCAM
    # (e.g., wine, infant food, or other nonfood items etc.)

    FAO_SUA_Kt_2010to2019 %>% distinct(item) %>%
      filter(!item %in% unique(SUA_Items_GCAM$item)) -> SUA_Items_NonGCAM

    # b. There are 426 FAO food items, all included in FAO_SUA_Kt_2010to2019 (530 items)
    # SUA_Items_Food includes both GCAM and NonGCAM(NEC)
    FAO_SUA_Kt_2010to2019 %>% distinct(item, item_code) %>%
      filter(item %in% unique(GCAMDATA_FAOSTAT_MacroNutrientRate_179Regs_426Items_2010to2019Mean$item)) %>%
      left_join(SUA_Items_GCAM, by = "item") %>%
      # For NA GCAM_commodity: not elsewhere classified (NEC)
      # So we would know % of food calories not included in GCAM commodities
      mutate(GCAM_commodity = if_else(is.na(GCAM_commodity), "NEC", GCAM_commodity)) ->
      SUA_Items_Food


    # 6.2 Get macronutrient values ----

    ### a. Get world average macronutrient ----
    # For filling in missing values

    GCAMDATA_FAOSTAT_MacroNutrientRate_179Regs_426Items_2010to2019Mean %>%
      tidyr::gather(macronutrient, macronutrient_value, calperg:proteinperc) %>%
      group_by(item, item_code, macronutrient) %>%
      summarise(macronutrient_value_World = mean(macronutrient_value), .groups = "drop") %>%
      ungroup() ->
      SUA_food_macronutrient_rate_World


    ### b. Calculate SUA food Calories consumption by joining macronutrient rates and SUA food ----

    FAO_SUA_Kt_2010to2019 %>%
      filter(element == "Food", item_code %in% SUA_Items_Food$item_code) %>%
      rename(Food_Kt = value) %>%
      select(-element) %>%
      left_join_error_no_match(SUA_Items_Food, by = c("item_code", "item")) %>%
      repeat_add_columns(
        tibble(macronutrient = c("calperg", "fatperc", "proteinperc"))) %>%
      left_join(
        GCAMDATA_FAOSTAT_MacroNutrientRate_179Regs_426Items_2010to2019Mean %>%
          tidyr::gather(macronutrient, macronutrient_value, calperg:proteinperc),
        by = c("area_code", "item_code", "item", "macronutrient")
      ) %>%
      left_join_error_no_match(SUA_food_macronutrient_rate_World,
                               by = c("item_code", "item", "macronutrient")) %>%
      mutate(macronutrient_value = if_else(is.na(macronutrient_value),
                                           macronutrient_value_World,
                                           macronutrient_value)) %>%
      # calculate total Cal, protein and fat in food
      # value was in 1000 ton or 10^ 9 g
      mutate(value = macronutrient_value * Food_Kt/ 1000,
             value = if_else(macronutrient %in% c("fatperc", "proteinperc"),
                             value / 100, value)) %>%
      select(-macronutrient_value, -macronutrient_value_World, -Food_Kt) %>%
      # rename element with units
      mutate(macronutrient = case_when(
        macronutrient == "calperg" ~ "MKcal",
        macronutrient == "fatperc" ~ "MtFat",
        macronutrient == "proteinperc" ~ "MtProtein" )) %>%
      left_join_error_no_match(AGLU_ctry %>% select(area = FAO_country, iso), by = "area") %>%
      left_join_error_no_match(iso_GCAM_regID %>% select(iso, GCAM_region_ID), by = "iso") ->
      FAO_Food_Macronutrient_All_2010_2019

    ### c. Get the max values of macronutrient conversion rate (per GCAM_commodity) ----
    # This will be used later as an upper bound to improve the data
    GCAMDATA_FAOSTAT_MacroNutrientRate_179Regs_426Items_2010to2019Mean %>%
      tidyr::gather(macronutrient, macronutrient_value, calperg:proteinperc) %>%
      left_join_error_no_match(SUA_Items_Food,
                               by = c("item_code", "item")) %>%
      group_by(GCAM_commodity, macronutrient) %>%
      summarise(max_macronutrient_value = max(macronutrient_value), .groups = "drop") ->
      FAO_Food_MacronutrientRate_2010_2019_MaxValue


    #****************************----
    # Produce outputs ----
    #*******************************

    GCAM_AgLU_SUA_APE_2010_2019 %>%
      add_title("GCAM_AgLU_SUA_APE_2010_2019") %>%
      add_units("kton") %>%
      add_comments("Supply utilization balance for GCAM commodities and regions in primary equivalent") %>%
      add_precursors("aglu/AGLU_ctry",
                     "common/iso_GCAM_regID",
                     "common/GCAM_region_names",
                     "aglu/FAO/GCAMDATA_FAOSTAT_SUA_195Regs_530Items_2010to2019",
                     "aglu/FAO/GCAMDATA_FAOSTAT_BiTrade_194Regs_400Items_2010to2020",
                     "aglu/FAO/Mapping_SUA_PrimaryEquivalent",
                     "aglu/FAO/GCAMDATA_FAOSTAT_ProdArea_195Regs_271Prod160AreaItems_1973to2020",
                     "aglu/FAO/GCAMDATA_FAOSTAT_FBSH_CB_173Regs_118Items_1973to2009",
                     "aglu/FAO/Mapping_item_FBS_GCAM") ->
      GCAM_AgLU_SUA_APE_2010_2019

    FAO_AgProd_Kt_All %>%
      add_title("FAO_AgProd_Kt_All") %>%
      add_units("1000 tonnes") %>%
      add_comments("Supply utilization balance for GCAM commodities and regions in primary equivalent") %>%
      add_precursors("aglu/AGLU_ctry",
                     "common/iso_GCAM_regID",
                     "aglu/FAO/FAO_ag_items_PRODSTAT",
                     "aglu/FAO/FAO_an_items_PRODSTAT",
                     "aglu/FAO/Mapping_SUA_PrimaryEquivalent",
                     "aglu/FAO/GCAMDATA_FAOSTAT_ProdArea_195Regs_271Prod160AreaItems_1973to2020") ->
      FAO_AgProd_Kt_All

    FAO_AgArea_Kha_All %>%
      add_title("FAO_AgArea_Kha_All") %>%
      add_units("1000 ha") %>%
      add_comments("Harvested area") %>%
      add_precursors("aglu/AGLU_ctry",
                     "common/iso_GCAM_regID",
                     "aglu/FAO/FAO_ag_items_PRODSTAT",
                     "aglu/FAO/FAO_an_items_PRODSTAT",
                     "aglu/FAO/Mapping_SUA_PrimaryEquivalent",
                     "aglu/FAO/GCAMDATA_FAOSTAT_ProdArea_195Regs_271Prod160AreaItems_1973to2020") ->
      FAO_AgArea_Kha_All

    FAO_Food_Macronutrient_All_2010_2019 %>%
      add_title("FAO_Food_Macronutrient_All_2010_2019") %>%
      add_units("MKcal, MtFat, MtProtein") %>%
      add_comments("Macronutrient consumption values connected to food consumption in GCAM_AgLU_SUA_APE_1973_2019") %>%
      add_precursors("aglu/AGLU_ctry",
                     "common/iso_GCAM_regID",
                     "aglu/FAO/GCAMDATA_FAOSTAT_SUA_195Regs_530Items_2010to2019",
                     "aglu/FAO/GCAMDATA_FAOSTAT_MacroNutrientRate_179Regs_426Items_2010to2019Mean",
                     "aglu/FAO/Mapping_SUA_PrimaryEquivalent") ->
      FAO_Food_Macronutrient_All_2010_2019

    FAO_Food_MacronutrientRate_2010_2019_MaxValue %>%
      add_title("FAO_Food_MacronutrientRate_2010_2019_MaxValue") %>%
      add_units("cal per g, fat perc. , protein perc.") %>%
      add_comments("The max value of macronutrient conversion rate across region, year, and SUA items (per GCAM_commodity") %>%
      add_precursors("aglu/AGLU_ctry",
                     "common/iso_GCAM_regID",
                     "aglu/FAO/GCAMDATA_FAOSTAT_SUA_195Regs_530Items_2010to2019",
                     "aglu/FAO/GCAMDATA_FAOSTAT_MacroNutrientRate_179Regs_426Items_2010to2019Mean",
                     "aglu/FAO/Mapping_SUA_PrimaryEquivalent") ->
      FAO_Food_MacronutrientRate_2010_2019_MaxValue

    return_data(MODULE_OUTPUTS)


  } else {
    stop("Unknown command")
  }
}
