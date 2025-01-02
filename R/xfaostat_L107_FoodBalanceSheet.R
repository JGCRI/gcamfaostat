# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_xfaostat_L107_FoodBalanceSheet
#'
#' Generate FBS supply utilization balance in primary equivalent
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs, a vector of output names, or (if
#'   \code{command} is "MAKE") all the generated outputs: \code{L107.FAO_Food_Macronutrient_All},
#'   \code{FAO_Food_MacronutrientRate_2010_2019_MaxValue}
#' @details This chunk aggregates supply utilization accounts into food balance sheet items
#' @importFrom assertthat assert_that
#' @importFrom dplyr summarize bind_rows filter if_else inner_join left_join mutate rename select n group_by_at
#' first case_when vars
#' @importFrom tibble tibble is_tibble
#' @importFrom tidyr complete drop_na gather nesting spread replace_na
#' @author XZ 2024
module_xfaostat_L107_FoodBalanceSheet <- function(command, ...) {

  MODULE_INPUTS <-
    c(FILE = file.path(DIR_RAW_DATA_FAOSTAT, "Mapping_gcamdata_SUA_PrimaryEquivalent"),
      FILE = file.path(DIR_RAW_DATA_FAOSTAT, "Mapping_gcamdata_SUA_ItemCode"),
      FILE = file.path(DIR_RAW_DATA_FAOSTAT, "Mapping_gcamdata_FAO_iso_reg"),
      "TM_bilateral_wide",
      "L105.Bal_new_all",
      "L106.SUA_food_macronutrient_rate")

  MODULE_OUTPUTS <-
    c("L107.Traceable_FBS_PCe_2010Plus",
      "L107.Traceable_FBS_Food_Calorie_Macronutrient_2010Plus")

  if(command == driver.DECLARE_INPUTS) {
    return(MODULE_INPUTS)
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(MODULE_OUTPUTS)
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    Curr_Envir <- environment()

    # Load required inputs ----

    get_data_list(all_data, MODULE_INPUTS, strip_attributes = TRUE)


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
      transmute(area_code, item_code, source_code, year, value) ->
      FAO_BiTrade_Kt

    # GCAMFAOSTAT_SUA
    L105.Bal_new_all %>%
      filter(value != 0.0) %>%
      transmute(area_code, item_code, element, year, value) ->
      FAO_SUA_Kt


    # Fix Sudan with code 206 and 276 after 2012
    if (206 %in% L106.SUA_food_macronutrient_rate$area_code == F) {
      L106.SUA_food_macronutrient_rate %>%
        filter(area_code  == 276) %>% mutate(area_code  = 206) %>%
        bind_rows(L106.SUA_food_macronutrient_rate) ->
        L106.SUA_food_macronutrient_rate
    }




    # Get Supply-utilization account (SUA) elements and use as factor
    All_Bal_element <- levels(FAO_SUA_Kt$element)
    All_Bal_element <- factor(All_Bal_element, levels = All_Bal_element)

    # Bilateral trade item indicator is added to Mapping_gcamdata_SUA_ItemCode
    # filter FAO_BiTrade_Kt to only include bilateral trade item to be
    # consistent with FAO_SUA_Kt
    BilaterialTrade_ItemCode <- Mapping_gcamdata_SUA_ItemCode %>% filter(TM == TRUE) %>% distinct(item_code) %>% pull
    FAO_BiTrade_Kt %>%
      filter(item_code %in% BilaterialTrade_ItemCode) ->
      FAO_BiTrade_Kt
    Mapping_gcamdata_SUA_ItemCode %>% select(item, item_code) -> Mapping_gcamdata_SUA_ItemCode

    # Section1: [2010-2019] Region aggregation of supply-utilization-accounting data ----

    # Note: the volume of data in this processing is quite large.  Therefore we took
    # extra care to be cognizant of processing speed and memory usage through section 1 and 2.
    # In particular we rely on ID codes and factors are much as possible to speed up joins.
    # In addition, we have filtered zero rows from the raw data to significantly reduce
    # the overall volume.  Unfortunately, this change makes the processing riddled with
    # trap doors where we need to be extra careful to complete / refill zeros or risk loosing
    # rows of legitimate data.

    # create a complete area / iso  region mapping
    FAO_SUA_Kt %>%
      distinct(area_code) %>%
      left_join_error_no_match(Mapping_gcamdata_FAO_iso_reg %>% distinct(area_code, iso, region_ID), by = c("area_code")) ->
      Area_Region_Map

    # 1.1 Regional aggregation for SUA ----
    # [this section is not doing anything if no regional aggregation is needed]

    # Aggregate SUA regions if needed
    # Intra regional trade is removed when bilateral trade data is available

    FAO_SUA_Kt %>%
      left_join_error_no_match(
        Area_Region_Map %>% select(area_code, region_ID), by="area_code") %>%
      group_by(region_ID, item_code, element, year) %>%
      summarize(value = sum(value), .groups = "drop") %>%
      ungroup() ->
      DF_SUA_Agg

    # Calculate intra regional trade
    FAO_BiTrade_Kt %>%
      left_join_error_no_match(Area_Region_Map %>% select(area_code, region_ID), by="area_code") %>%
      left_join_error_no_match(Area_Region_Map %>% select(source_code = area_code, source_region_ID = region_ID), by="source_code") %>%
      filter(region_ID == source_region_ID) %>%
      group_by(region_ID, item_code, year) %>%
      summarize(value = sum(value), .groups = "drop") %>%
      ungroup() %>%
      mutate(value = -value) ->
      DF_INTRA_REG_TRADE

    # SUA has fewer items and years than the bilateral data set and in addition
    # there are some small discrepancies zero import/export in SUA vs tiny amounts of trade
    # in the bilateral.  Doing a left_join here will drop these dependencies which is
    # what we want.
    bind_rows(DF_INTRA_REG_TRADE %>% mutate(element = All_Bal_element[All_Bal_element == "Export"]),
              DF_INTRA_REG_TRADE %>% mutate(element = All_Bal_element[All_Bal_element == "Import"])) %>%
      rename(TCL = value) %>%
      right_join(DF_SUA_Agg, by = c("region_ID", "item_code", "year", "element")) %>%
      mutate(value = if_else(is.na(TCL), value, value + TCL)) %>%
      select(-TCL) %>%
      filter(value != 0.0) ->
      DF_SUA_Agg_TradeAdj


    # need to remove gross trade when export > production
    # to maintain triangle the inequality rule
    # Note that Prod < export is still possible due to "residuals"
    DF_SUA_Agg_TradeAdj %>%
      filter(element %in% c("Production", "Import", "Export")) %>%
      spread(element, value, fill=0.0) %>%
      mutate(value = pmax(Production - Export, -Import)) %>%
      filter(value < 0) %>%
      select(-Production, -Import, -Export) ->
      GrossTradeRM

    bind_rows(GrossTradeRM %>% mutate(element = All_Bal_element[All_Bal_element == "Export"]),
              GrossTradeRM %>% mutate(element = All_Bal_element[All_Bal_element == "Import"]),
              DF_SUA_Agg_TradeAdj) %>%
      group_by(region_ID, item_code, element, year) %>%
      summarize(value = sum(value), .groups = "drop") %>%
      ungroup() ->
      FAO_SUA_Kt_R

    # complete years and fill in zeros
    FAO_SUA_Kt_R %>%
      complete(nesting(region_ID, item_code, element), year) %>%
      replace_na(list(value = 0)) ->
      FAO_SUA_Kt_R

    ## Clean up
    rm(FAO_BiTrade_Kt)
    ## Done Section1 ----
    #****************************----

    # Section2: [2010-2019] Primary equivalent aggregation to PCe commodities ----

    Mapping_gcamdata_SUA_PrimaryEquivalent %>%
      left_join_error_no_match(Mapping_gcamdata_SUA_ItemCode %>% rename(sink_item_code = item_code), by=c("sink_item" = "item")) %>%
      left_join_error_no_match(Mapping_gcamdata_SUA_ItemCode %>% rename(source_item_code = item_code), by=c("source_item" = "item")) %>%
      mutate(APE_comm = as.factor(APE_comm)) ->
      Mapping_gcamdata_SUA_PrimaryEquivalent_ID

    #Mapping_gcamdata_SUA_PrimaryEquivalent_ID[Mapping_gcamdata_SUA_PrimaryEquivalent_ID$sink_item_code == 235, "source_primary"] = FALSE

    Mapping_gcamdata_SUA_PrimaryEquivalent_ID %>%
      select(item_code = sink_item_code, output_specific_extraction_rate) %>%
      filter(!is.na(output_specific_extraction_rate)) ->
      OUTPUT_SPECIFIC_EXTRACTION_RATE

    # 2.1 Helper functions for SUA primary equivalent aggregation ----


    # Get extraction rate
    # @description Gross extraction rate is calculated for domestic, traded, and lagged values.
    # By gross, it means sink items are aggregated.
    # The function is used in Proc_primarize.
    # @param DF_CURR_NEST Input supply-utilization accounting data frame with one tier of processing
    # @param DF_ALL Input supply-utilization accounting data frame with ALL the data
    # @return A data frame including regional, traded, and world extraction rates of a processing

    Get_GROSS_EXTRACTION_RATE <- function(DF_CURR_NEST, DF_ALL) {

      curr_sink_items = unique(DF_CURR_NEST$item_code)
      Mapping_gcamdata_SUA_PrimaryEquivalent_ID %>%
        filter(sink_item_code %in% curr_sink_items) ->
        Curr_Sink_Mapping
      curr_source_items = unique(Curr_Sink_Mapping$source_item_code)
      Mapping_gcamdata_SUA_PrimaryEquivalent_ID %>%
        filter(source_item_code %in% curr_source_items) ->
        Curr_Source_Mapping
      Curr_Source_Mapping %>%
        group_by(APE_comm) %>%
        mutate(minimium_extraction_rate = if_else(Q25asMin, extraction_rate_Q25, 0)) %>%
        select(APE_comm, minimium_extraction_rate) %>%
        distinct() ->
        MIN_EXTRACTION_RATE

      DF_ALL %>%
        #Prepare data to calculate regional, traded, and world average extraction rates
        tidyr::unnest(c(data)) %>%
        filter(element == "Processed", item_code %in% curr_source_items) %>%
        select(-nest_level) %>%
        bind_rows(DF_CURR_NEST %>% filter(element == "Production" | element == "Export")) %>%
        dplyr::group_by_at(vars(-item_code, -value)) %>%
        summarize(value=sum(value), .groups = "drop") %>%
        ungroup() %>%
        complete(region_ID = unique(Area_Region_Map$region_ID), nesting(element, year, APE_comm), fill=list(value=0)) %>%
        spread(element, value, fill = 0.0) %>%
        left_join_error_no_match(MIN_EXTRACTION_RATE, by=c("APE_comm")) %>%
        group_by(APE_comm, year) %>%
        mutate(extraction_rate_world = sum(Production) / sum(Processed),
               # in case sum(Processed) or sum(Production) == 0
               extraction_rate_world = if_else(extraction_rate_world != 0 & is.finite(extraction_rate_world),
                                               extraction_rate_world, 1),
               extraction_rate = Production / Processed,
               # Regional extraction rate = prod of an aggregated processed item  / Processed use of an aggregated primary item
               # Use world average to fill in NA or zero
               extraction_rate = if_else(is.na(extraction_rate) | extraction_rate == 0, extraction_rate_world, extraction_rate),
               # Using minimum extraction rate here as an upper threshold to avoid extremely large scaling later
               extraction_rate = pmax(extraction_rate, minimium_extraction_rate),
               extraction_rate_trade = sum(Export) / sum(Export / extraction_rate),
               extraction_rate_trade = if_else(is.na(extraction_rate_trade), extraction_rate, extraction_rate_trade),
               # Set rate to Inf when Processed == 0
               PositiveProd_ZeroProc = Production > 0 & Processed == 0,
               extraction_rate = if_else(PositiveProd_ZeroProc == TRUE, Inf, extraction_rate)
               ) %>%
        ungroup() %>%
        group_by(APE_comm, region_ID) %>%
        # Calculate lagged extraction_rate but replace NA with current rate (first period)
        mutate(extraction_rate_lag = lag(extraction_rate, default=extraction_rate[1])) %>%
        ungroup() %>%
        select(APE_comm, region_ID, year, bal_import = extraction_rate_trade, bal_domestic_lag = extraction_rate_lag, bal_domestic_current = extraction_rate) %>%
        gather(bal_source, extraction_rate, bal_import, bal_domestic_lag, bal_domestic_current, factor_key = TRUE)
    }


    # Separate the SUA balance into domestic and imported balanced for sink_item
    # @description The function is used in Proc_primarize
    # @param DF_CURR_NEST Input supply-utilization accounting data frame with one tier of processing
    # @return SUA DF

    Get_ARMINGTON_BALANCE <- function(DF_CURR_NEST) {
      Import_Demand_Item <- factor(c("Food", "Feed", "Processed", "Other uses", "Seed", "Loss"), levels=All_Bal_element)

      DF_CURR_NEST %>%
        # Calculate imported consumption share
        # The assumption is that a portion of Import_Demand_Items was imported
        # so they need to be scaled by an international extraction rate
        # Note that stock variation is not included in import consumption to maintain stock balance
        # so additional adjustment may be needed

        # We will map/allocate Import to Import_Demand_Item
        # But need to ensure that Import_Demand_Item consumed more than Import; otherwise, using residue to adjust
        # that is more import and more residue
        # We calculate Import_Demand_Share = import / import_demand
        # import_demand is indeed the demand values that could source from import
      # and use this share to share our import for all Import_Demand_Item
        filter(element == "Import" | element %in% Import_Demand_Item) %>%
        mutate(is_import = element == "Import") %>%
        spread(is_import, value, fill=0.0) %>%
        group_by(APE_comm, region_ID, year, item_code) %>%
        summarize(import = sum(`TRUE`),
                  import_demand = sum(`FALSE`), .groups = "drop") %>%
        ungroup() %>%
        mutate(Import_Demand_Share = import / import_demand,
               # replace NA and inf
               Import_Demand_Share = if_else(is.finite(Import_Demand_Share), Import_Demand_Share, 0),
               # The share should be small than 1 though outlier regions may import for storage
               Import_Demand_Share = pmin(Import_Demand_Share, 1),
               residual = import - import_demand * Import_Demand_Share) %>%
        ungroup() %>%
        select(APE_comm, region_ID, item_code, year, Import_Demand_Share, residual) %>%
        left_join(DF_CURR_NEST, ., by=c("APE_comm", "region_ID", "item_code", "year")) %>%
        # when Import_Demand_Item consumption < Import they are used to share out Import consumptions
        # otherwise, Residuals is used for adjustments
        mutate(bal_import = case_when(element == "Import" ~ value,
                                      element %in% Import_Demand_Item ~ value * Import_Demand_Share,
                                      element == "Residuals" ~ residual,
                                      TRUE ~ 0),
               # Calculate domestic balance
               bal_domestic = value - bal_import) %>%
        select(-value, -Import_Demand_Share, -residual) %>%
        gather(bal_source, value, bal_import, bal_domestic, factor_key = TRUE) ->
        TradeBal_Data

      # adding back "Regional supply" and "Regional demand"

      Regional_supply_elements <- factor(c("Opening stocks", "Production", "Import"), levels=All_Bal_element)
      Regional_demand_elements <- factor(c("Export", "Feed", "Food", "Loss", "Processed", "Seed", "Other uses", "Closing stocks"), levels=All_Bal_element)
      TradeBal_Data %>%
        mutate(is_supply = element %in% Regional_supply_elements,
               is_demand = element %in% Regional_demand_elements) %>%
        filter(is_supply | is_demand) %>%
        group_by(APE_comm, region_ID, year, item_code, bal_source) %>%
        # Clean the bal items
        summarize(`Regional supply` = sum(value[is_supply]),
                  `Regional demand` = sum(value[is_demand]), .groups = "drop") %>%
        ungroup() %>%
        mutate(`Residuals` = `Regional supply` - `Regional demand`) %>%
        gather(element, value, `Regional supply`, `Regional demand`, `Residuals`) %>%
        mutate(element = factor(element, levels=All_Bal_element)) %>%
        bind_rows(TradeBal_Data %>% filter(!element %in% c("Regional supply", "Regional demand", "Residuals")), .)
    }


    # Separate the domestic SUA balance into current and lagged balanced for sink_item
    # @description The function is used in Proc_primarize
    # @param DF_CURR_NEST_TradeAdj Output from Get_ARMINGTON_BALANCE. Input supply-utilization accounting data frame with one tier of processing and
    # @param .SINK_ITEM Sink items or processed items in the processing
    # @return SUA DF

    Get_STOCK_BALANCE <- function(DF_CURR_NEST_TradeAdj) {
      Opening_Stock_Item <- factor(c("Food", "Feed", "Processed", "Other uses", "Seed", "Loss"), levels=All_Bal_element)

      get_bal_source_data <- function(data, bal_source_key) {
        data[data$bal_source == bal_source_key, "data", drop = TRUE][[1]]
      }

      DF_CURR_NEST_TradeAdj %>%
        tidyr::nest(data = -bal_source) ->
        StockCalcNested

      StockCalcNested %>%
        get_bal_source_data("bal_domestic") %>%
        filter(element == "Opening stocks" | element %in% Opening_Stock_Item) %>%
        mutate(is_opening = element == "Opening stocks") %>%
        spread(is_opening, value, fill=0.0) %>%
        group_by(APE_comm, region_ID, year, item_code) %>%
        summarize(Ostock = sum(`TRUE`),
                  Ostock_demand = sum(`FALSE`), .groups = "drop") %>%
        ungroup() %>%
        mutate(Ostock_Demand_Share = Ostock / Ostock_demand,
               # The share should be small than 1
               # Other elements will be adjusted if not
               Ostock_Demand_Share = if_else(is.finite(Ostock_Demand_Share), Ostock_Demand_Share, 0),
               Ostock_Demand_Share = pmin(Ostock_Demand_Share, 1),
               residual = Ostock - Ostock_demand * Ostock_Demand_Share) %>%
        ungroup() %>%
        select(APE_comm, region_ID, item_code, year, Ostock_Demand_Share, residual) %>%
        left_join(StockCalcNested %>% get_bal_source_data("bal_domestic"), ., by=c("APE_comm", "region_ID", "item_code", "year")) %>%
        mutate(bal_domestic_lag = case_when(element == "Opening stocks" ~ value,
                                            element %in% Opening_Stock_Item ~ value * Ostock_Demand_Share,
                                            element == "Residuals" ~ residual,
                                            TRUE ~ 0),
               # Calculate domestic balance
               bal_domestic_current = value - bal_domestic_lag) %>%
        select(-value, -Ostock_Demand_Share, -residual) %>%
        gather(bal_source, value, bal_domestic_lag, bal_domestic_current, factor_key = TRUE) ->
        StockBal_Data

      Regional_supply_elements <- factor(c("Opening stocks", "Production", "Import"), levels=All_Bal_element)
      Regional_demand_elements <- factor(c("Export", "Feed", "Food", "Loss", "Processed", "Seed", "Other uses", "Closing stocks"), levels=All_Bal_element)
      Bal_types = c("bal_import", "bal_domestic_lag", "bal_domestic_current")
      Bal_types = factor(Bal_types, levels=Bal_types)
      StockBal_Data %>%
        complete(year = unique(StockBal_Data$year), nesting(region_ID, item_code, element, APE_comm, bal_source), fill=list(value=0)) %>%
        mutate(is_supply = element %in% Regional_supply_elements,
               is_demand = element %in% Regional_demand_elements) %>%
        group_by(APE_comm, region_ID, year, item_code, bal_source) %>%
        # Clean the bal items
        summarize(`Regional supply` = sum(value[is_supply]),
                  `Regional demand` = sum(value[is_demand]),
                  # using max to guard against missing Closing stocks row
                  `Stock Variation` = max(value[element == "Closing stocks"], 0) - max(value[element == "Opening stocks"], 0),
                  .groups = "drop") %>%
        ungroup() %>%
        mutate(`Residuals` = `Regional supply` - `Regional demand`) %>%
        gather(element, value, `Regional supply`, `Regional demand`, `Stock Variation`, `Residuals`) %>%
        mutate(element = factor(element, levels=All_Bal_element)) %>%
        bind_rows(StockBal_Data %>% filter(!element %in% c("Regional supply", "Regional demand", "Stock Variation", "Residuals")), .) %>%
        tidyr::nest(data = - bal_source) %>%
        mutate(bal_source = factor(bal_source, levels=Bal_types)) %>%
        bind_rows(StockCalcNested %>% filter(bal_source == "bal_import") %>% mutate(bal_source = factor(bal_source, levels=Bal_types))) %>%
        tidyr::unnest(c("data"))
    }


    # Primary equivalent aggregation
    # @param DF_ALL Input supply-utilization accounting data frame with all levels of data nested which need to be primarized
    # @return A supply-utilization accounting data frame with all levels processed and aggregated to APE_comm_Agg

    Proc_primarize <- function(DF_ALL){

      MaxNest = max(DF_ALL$nest_level)
      MinNest = 1

      for(curr_nest in MaxNest:MinNest) {
        # get the current tier to process
        DF_ALL %>%
          filter(nest_level == curr_nest) %>%
          pull(data) %>%
          first() ->
          DF_CURR_NEST

        # Sink items or processed items in the processing
        curr_sink_items = unique(DF_CURR_NEST$item_code)
        Mapping_gcamdata_SUA_PrimaryEquivalent_ID %>%
          filter(sink_item_code %in% curr_sink_items) ->
          Curr_Sink_Mapping
        # Source items or primary items in the processing
        curr_source_items = unique(Curr_Sink_Mapping$source_item_code)
        Mapping_gcamdata_SUA_PrimaryEquivalent_ID %>%
          filter(source_item_code %in% curr_source_items) ->
          Curr_Source_Mapping

        # OUTPUT_SPECIFIC_EXTRACTION_RATE A data frame with item and output_specific_extraction_rate.
        #' # In some cases, prescale sink item SUA using output_specific_extraction_rate can improve the processing.
        #' # e.g., when coproduction shares are not fixed.
        if(nrow(OUTPUT_SPECIFIC_EXTRACTION_RATE %>% filter(item_code %in% curr_sink_items)) > 0) {
          ## a. Pre-scale sink item data when .OUTPUT_SPECIFIC_EXTRACTION_RATE is available ----
          DF_CURR_NEST %>%
            left_join(OUTPUT_SPECIFIC_EXTRACTION_RATE, by=c("item_code")) %>%
            replace_na(list(output_specific_extraction_rate = 1)) %>%
            mutate(value = value / output_specific_extraction_rate) %>%
            select(-output_specific_extraction_rate) ->
            DF_CURR_NEST
        }

        ## b. For the sink items of the tier, separate balance into domestic and imported ----
        # Note that the method here relies on Get_GROSS_EXTRACTION_RATE and Get_ARMINGTON_BALANCE
        # Aggregate sink_items are aggregated into "sink_item"
        # And production & processed are adjusted for primary aggregation
        DF_CURR_NEST %>%
          # adding bal_source including bal_import and bal_domestic
          # map/allocate import to consumption
          Get_ARMINGTON_BALANCE() %>%
          Get_STOCK_BALANCE() %>%
          # Get extraction rate for domestic and traded
          # Note that extraction rates are mean values across time
          # Note that regional extraction rate could be inf
          # It is likely due to data inconsistency, e.g., zero processed in source but positive sink
          # No adjustments were made since 1/inf become zero in the scaling process, preserving primary balance
          left_join(Get_GROSS_EXTRACTION_RATE(DF_CURR_NEST, DF_ALL), by=c("APE_comm", "region_ID", "year", "bal_source")) %>%
          # Scale sink items to get source item equivalent
          mutate(value = value / extraction_rate) %>%
          select(-extraction_rate) ->
          .df1

        ## c. Calculate source item shares ----
        # in case that there are multiple source items, we need to do vertical aggregation carefully to avoid double accounting
        # here we calculate the source shared by their processed uses
        DF_ALL %>%
          filter(nest_level <= curr_nest) %>%
          tidyr::unnest(c("data")) %>%
          filter(item_code %in% curr_source_items, element == "Processed") %>%
          select(-element) ->
          .df2

        .df2 %>%
          complete(region_ID = unique(Area_Region_Map$region_ID), nesting(APE_comm, item_code, nest_level, year), fill=list(value=0)) %>%
          complete(.df2 %>% distinct(APE_comm, item_code, nest_level), nesting(region_ID, year), fill=list(value=0)) ->
          .df2_1

        .df2_1 %>%
          # Use global value (year is specified now!)
          group_by(APE_comm, item_code, year) %>%
          mutate(value = sum(value)) %>%
          ungroup() %>%
          group_by(APE_comm, region_ID, year) %>%
          mutate(share = value/ sum(value),
                 share = if_else(is.finite(share), share, dplyr::n()/sum(dplyr::n()))) %>%
          ungroup() %>%
          select(-value, source_item_code = item_code) ->
          source_share_global

        .df2_1 %>%
          group_by(APE_comm, item_code, year) %>%
          mutate(value = sum(value)) %>%
          ungroup() %>%
          group_by(APE_comm, region_ID, year) %>%
          mutate(share = value/ sum(value),
                 share = if_else(is.finite(share), share, dplyr::n()/sum(dplyr::n()))) %>%
          ungroup() %>%
          select(-value, source_item_code = item_code) ->
          source_share_regional

        source_share_regional %>% mutate(bal_source = "bal_domestic_lag") %>%
          bind_rows(source_share_regional %>% mutate(bal_source = "bal_domestic_current")) %>%
          bind_rows(source_share_global %>% mutate(bal_source = "bal_import")) ->
          source_share

        # Note that the treatment of this share (when many-to-one processing) could cause negative values in processed use
        # we will make adjustments right away

        ## d. Merge sink SUA into source items SUA  ----
        # Note that with multiple source items, sinks are aggregated into sources based on average processed shares across sources
        # Prepare data to calculate world average source share

        # prepare sink item SUAs and map them to source item(s)
        # multiple source items are possible but source item will be shared out
        .df1 %>%
          left_join(source_share, by = c("bal_source", "region_ID", "year", "APE_comm")) %>%
          filter(!is.na(share)) %>%
          mutate(value = value * share,
                 item_code = source_item_code) %>%
          select(-share) %>%
          group_by(nest_level, APE_comm, region_ID, year, item_code, element) %>%
          summarize(value = sum(value), .groups = "drop") %>%
          ungroup() %>%
          complete(element=All_Bal_element[All_Bal_element %in% c("Prodution", "Processed")], nesting(nest_level, APE_comm, region_ID, year, item_code), fill=list(value=0)) %>%
          group_by(nest_level, APE_comm, region_ID, year, item_code) %>%
          # get data ready to merger upper nest and move prod to negative processed
          mutate(value = if_else(element == "Production" | element == "Processed", value - value[element == "Production"], value)) %>%
          ungroup() ->
          .df3

        # Bind source item and aggregated across source & sink items based on primary equivalent
        # Note we will bind and aggregate by nest, ultimately it seems unlikely nesting and provided
        # any performance boost, but certainly didn't hurt either.
        .df3 %>%
          tidyr::nest(data = -nest_level) ->
          df3_nested

        # merge df3_nested into DF_ALL and aggregate
        for(nest_i in df3_nested$nest_level) {

          DF_ALL %>%
            filter(nest_level == nest_i) %>% pull(data) %>% first %>%
            bind_rows(
              df3_nested %>% filter(nest_level == nest_i) %>% pull(data) %>% first
            ) %>%
            group_by(APE_comm, region_ID, year, item_code, element) %>%
            summarize(value = sum(value), .groups = "drop") %>%
            ungroup() ->
            AGG
          # There could be negative "processed" use due to the above many-to-one processing
          # but should be small; we will adjust them later since they may aggregate
          DF_ALL %>%
            filter(nest_level != nest_i) %>%
            bind_rows(tibble(nest_level = nest_i, data = list(AGG))) ->
            DF_ALL
        }
        # drop the processed tier as the data has now been aggregated and thus
        # no longer needed
        DF_ALL %>% filter(nest_level != curr_nest) ->
          DF_ALL
      }

      # Combine the remaining items by APE_comm
      DF_ALL %>%
        tidyr::unnest(c("data")) %>%
        group_by(region_ID, APE_comm, element, year) %>%
        summarize(value = sum(value), .groups = "drop") %>%
        ungroup() %>%
        spread(element, value, fill = 0.0) %>%
        # Do a final balance cleaning
        # starting with stocks; the adj here is generaly very small
        group_by(region_ID, APE_comm) %>% dplyr::arrange(-year) %>%
        mutate(`Stock Variation` = `Closing stocks` - `Opening stocks`,
               cum_StockVariation = cumsum(`Stock Variation`) - first(`Stock Variation`),
               `Opening stocks1` = first(`Opening stocks`) - cum_StockVariation) %>%
        select(-cum_StockVariation, -`Opening stocks`) %>%
        rename(`Opening stocks` = `Opening stocks1`) %>%
        mutate(`Closing stocks` = `Opening stocks` + `Stock Variation`) %>%
        mutate(Stockshifter = if_else(`Closing stocks` < 0, abs(`Closing stocks`), 0)) %>%
        mutate(Stockshifter = if_else(`Opening stocks` < 0 & `Opening stocks` < `Closing stocks`,
                                      abs(`Opening stocks`), Stockshifter)) %>%
        mutate(Stockshifter = max(Stockshifter),
               `Opening stocks` = `Opening stocks` + Stockshifter,
               `Closing stocks` = `Opening stocks` + `Stock Variation`) %>%
        select(-Stockshifter) %>%
        ungroup() %>%
        # recalculate residuals
        mutate(`Regional supply` = `Opening stocks` + Production + `Import`,
               # ignore negative "processed" due to the above many-to-one processing above (mostly small)
               Processed = if_else(Processed < 0, 0, Processed),
               Food = if_else(Food < 0, 0, Food),
               `Other uses` = if_else(`Other uses` < 0, 0, `Other uses`),
               `Regional demand` = `Export` + Feed + Food + Loss + Processed + Seed + `Other uses` +`Closing stocks`,
               Residuals = `Regional supply` -  `Regional demand`) %>%
        gather(element, value, -region_ID, -APE_comm, -year) ->
        APE_AGG

      # Aggregate by APE_comm_Agg
      # At this point we ditch the ID codes and factors as we return the data and
      # make it available for the rest of the original processing
      APE_AGG %>%
        left_join_error_no_match(Mapping_gcamdata_SUA_PrimaryEquivalent %>% select(APE_comm_Agg, APE_comm) %>% distinct(),
                                 by = c("APE_comm")) %>%
        group_by(region_ID, APE_comm_Agg, element, year) %>%
        summarize(value = sum(value), .groups = "drop") %>%
        ungroup() %>%
        mutate(element = as.character(element)) %>%
        select(region_ID, year, APE_comm_Agg, element, value) ->
        .df_APE

      return(.df_APE)
    }

    # 2.2. Execution: process data into APE ----


    ## Loop through all APE_comm_Agg with available data ----

    FAO_SUA_Kt_R %>%
      left_join(Mapping_gcamdata_SUA_PrimaryEquivalent_ID %>%
                  select(APE_comm, item_code = sink_item_code, nest_level) %>% distinct(), by = c("item_code")) %>%
      left_join(Mapping_gcamdata_SUA_PrimaryEquivalent_ID %>%
                  select(APE_comm_source = APE_comm, item_code = source_item_code) %>% distinct(), by=c("item_code")) %>%
      # find SUA items which are truly not mapped to anything and filter them out
      mutate(APE_comm = if_else(is.na(APE_comm), APE_comm_source, APE_comm)) %>%
      select(-APE_comm_source) %>%
      filter(!is.na(APE_comm)) %>%
      # the remaining rows with NA nest_level are primary, we need to keep them
      # around for processing even though they don't need to be aggregated themselves
      # so we will give them a nest level of -1
      mutate(nest_level = if_else(is.na(nest_level), -1L, nest_level)) %>%
      # we will literally nest by nest level to avoid constant subseting
      # although we end up needed to unnest at times as well so ultimately,
      # it likely makes little difference in performance
      tidyr::nest(data = -nest_level) %>%
      # we are now ready to recursively primarize APE commodities then aggregate to APE
      Proc_primarize() ->
      L107.Traceable_FBS_PCe_2010Plus


    Check_Balance_SUA <- function(.DF){

      NULL -> element -> GCAM_commodity -> Import -> Export -> Production -> Food ->
        Feed -> `Other uses` -> `Regional supply` -> `Regional demand` -> bal ->
        region

      assertthat::assert_that(all(c("element") %in% names(.DF)))

      # 0. Check NA
      if (.DF %>% filter(is.na(value)) %>% nrow() > 0) {
        warning("NA values in SUA Balance")
      }

      # 1. Positive value except stock variation and residues
      if (isFALSE(.DF %>% filter(!element %in% c("Stock Variation", "Residuals")) %>%
                  summarise(min = min(value, na.rm = T)) %>% pull(min) >= -0.001)) {
        warning("Negative values in key elements (not including stock variation and Residuals)")
      }

      # 2. Trade balance in all year and items
      if (isFALSE(.DF %>% filter(element %in% c("Import", "Export")) %>%
                  group_by(year, APE_comm_Agg, element) %>%
                  summarise(value = sum(value), .groups = "drop") %>%
                  spread(element, value) %>% filter(abs(Import - Export) > 0.0001) %>% nrow() == 0)) {
        warning("Gross trade imbalance")
      }

      # 3. SUA balance check
      if (isFALSE(.DF %>%
                  spread(element, value) %>%
                  mutate(`Regional supply` = Production + `Import` +`Opening stocks`,
                         `Regional demand` = `Export` + Feed + Food + Loss + Processed + Seed + `Other uses` +`Closing stocks` + Residuals,
                         bal = abs(`Regional supply` -  `Regional demand`)) %>%
                  filter(bal > 0.0001) %>% nrow() == 0)) {
        warning("Regional supply != Regional demand + Residuals")
      }

      # 4. Balanced in all dimensions but APE_comm_Agg

      assertthat::assert_that(.DF %>% group_by(APE_comm_Agg) %>%
                                summarize(nyear = length(unique(year)),
                                          nreg = length(unique(region_ID)),
                                          nele = length(unique(element)), .groups = "drop"
                                ) %>%
                                summarize(count = sum(nyear * nreg *nele)) %>% pull(count) ==
                                .DF %>% nrow())

    }


    Check_Balance_SUA(L107.Traceable_FBS_PCe_2010Plus)

    ## Done Section2 ----
    #****************************----



    #Section3 Connect food items and macronutrient rates ----

    # 3.1 Separate FAO food items into GCAM food items and NEC for macronutrient ----
    # GCAM included most of the food items
    # All food item with available macronutrient info from FAOSTAT are included

    # a. Get all GCAM SUA items from the mapping by binding both source and sink items
    # 533 items

    Mapping_gcamdata_SUA_PrimaryEquivalent %>%
      select(APE_comm_Agg, item = source_item) %>%
      bind_rows(Mapping_gcamdata_SUA_PrimaryEquivalent %>%
                  select(APE_comm_Agg, item = sink_item)) %>%
      distinct() %>%
      left_join_error_no_match(Mapping_gcamdata_SUA_ItemCode, by = "item") ->
      SUA_Items_APE

    # food is uniquely mapped from SUA to APE_Comm
    assertthat::assert_that(
      SUA_Items_APE %>% distinct(item_code) %>% nrow() == SUA_Items_APE %>% nrow(),
      msg = "Check duplicates in Mapping_gcamdata_SUA_PrimaryEquivalent SUA items"
    )

    # check: there are two items with empty accounts
    Mapping_gcamdata_SUA_ItemCode %>%
      filter(!item_code %in% unique(SUA_Items_APE$item_code)) -> SUA_Items_NEC

    # b. There are 432 FAO food items, all included in FAO_SUA_Kt_2010to2022 (535 items)
    Mapping_gcamdata_SUA_ItemCode %>%
      filter(item_code %in% unique(L106.SUA_food_macronutrient_rate$item_code)) %>%
      left_join_error_no_match(SUA_Items_APE %>% select(-item), by = "item_code") %>%
      # For NA APE_comm_Agg: not elsewhere classified (NEC)
      # So we would know % of food calories not included in GCAM commodities
      # we later updated mapping to account for NEC there already; no No NA here
      mutate(APE_comm_Agg = if_else(is.na(APE_comm_Agg), "NEC", APE_comm_Agg)) ->
      SUA_Items_Food


    # 3.2 Get macronutrient values ----

    ### a. Get world average macronutrient ----
    # For filling in missing values

    L106.SUA_food_macronutrient_rate %>%
      tidyr::gather(macronutrient, macronutrient_value, calperg:proteinperc) %>%
      group_by(item, item_code, macronutrient) %>%
      summarise(macronutrient_value_World = mean(macronutrient_value), .groups = "drop") %>%
      ungroup() ->
      SUA_food_macronutrient_rate_World


    ### b. Calculate SUA food Calories consumption by joining macronutrient rates and SUA food ----

    # Get data ready first
    FAO_SUA_Kt %>%
      filter(element == "Food", item_code %in% SUA_Items_Food$item_code) %>%
      # ensure we at least have a complete series across time otherwise it may
      # throw off moving avg calculations
      complete(area_code = Area_Region_Map$area_code, year = pull(., year) %>% unique(), nesting(item_code, element), fill=list(value=0)) %>%
      rename(Food_Kt = value) %>%
      select(-element) %>%
      left_join_error_no_match(SUA_Items_Food, by = c("item_code")) %>%
      repeat_add_columns(
        tibble(macronutrient = c("calperg", "fatperc", "proteinperc"))) %>%
      left_join(
        L106.SUA_food_macronutrient_rate %>%
          select(-item) %>%
          tidyr::gather(macronutrient, macronutrient_value, calperg:proteinperc),
        by = c("area_code", "item_code", "macronutrient")
      ) %>%
      left_join_error_no_match(SUA_food_macronutrient_rate_World %>% select(-item),
                               by = c("item_code", "macronutrient")) %>%
      mutate(macronutrient_value = if_else(is.na(macronutrient_value),
                                           macronutrient_value_World,
                                           macronutrient_value) ) %>%
    # computation
      # calculate total Cal, protein and fat in food
      # value was in 1000 ton or 10^ 9 g
      mutate(value = macronutrient_value * Food_Kt,
             value = if_else(macronutrient %in% c("fatperc", "proteinperc"),
                             value / 100 /1000, value)) %>% # unit from perc to Mt
      select(-macronutrient_value, -macronutrient_value_World, -Food_Kt) %>%
      # rename element with units
      mutate(
        element = case_when(
          macronutrient == "calperg" ~ "Dietary energy",
          macronutrient == "fatperc" ~ "Fat",
          macronutrient == "proteinperc" ~ "Protein" ),
        unit = case_when(
          macronutrient == "calperg" ~ "Mkcal",
          macronutrient == "fatperc" ~ "Mt",
          macronutrient == "proteinperc" ~ "Mt" )) %>%
      select(-macronutrient)->
      L107.Traceable_FBS_Food_Calorie_Macronutrient_2010Plus

    # Potential discrepancy in food calorie and macronutrient aggregation (due to data quality)
    # when the processing use of primary is zero while there is indeed secondary output and food consumption
    # food in secondary products won't be converted to primary due to zero extraction rates.
    # fortunately, there are only a few cases in small areas/sectors.
    # One example is "Other citrus and products" in PCe in "bra"

    L107.Traceable_FBS_Food_Calorie_Macronutrient_2010Plus %>%
      group_by(area_code, year, APE_comm_Agg, element) %>%
      summarize(value = sum(value), .groups = "drop") %>%
      # anti join ones with positive food mass in PCe
      anti_join(
        L107.Traceable_FBS_PCe_2010Plus %>%
          filter(element == "Food", value >0) %>%
          spread(element, value) %>% rename(area_code = region_ID),
        by = c("area_code", "year", "APE_comm_Agg")
      ) %>%
      # check remaining positive
      filter(value > 0) %>%
      distinct(area_code, year, APE_comm_Agg) %>%
      mutate(ZeroFood = 0) ->
      FBS_FOOD_SCALER

    # After checks above, there were 30 obs; we change the calorie and macronutrient to zero in FBS
    L107.Traceable_FBS_Food_Calorie_Macronutrient_2010Plus %>%
      left_join(FBS_FOOD_SCALER,
                by = c("area_code", "year", "APE_comm_Agg")) %>%
      mutate(value = if_else(is.na(ZeroFood), value, value *0)) %>%
      select(-ZeroFood) ->
      L107.Traceable_FBS_Food_Calorie_Macronutrient_2010Plus


    ## Done Section3 ----
    #****************************----
    # Produce outputs ----

    L107.Traceable_FBS_PCe_2010Plus %>%
      add_title("L107.Traceable_FBS_PCe_2010Plus") %>%
      add_units("kton") %>%
      add_comments("Updated FBS: Supply utilization balance in primary equivalent, FAO countries, APE commoditeis, for 2010 +") %>%
      add_precursors("TM_bilateral_wide",
                     "L105.Bal_new_all",
                     file.path(DIR_RAW_DATA_FAOSTAT, "Mapping_gcamdata_SUA_PrimaryEquivalent"),
                     file.path(DIR_RAW_DATA_FAOSTAT, "Mapping_gcamdata_SUA_ItemCode"),
                     file.path(DIR_RAW_DATA_FAOSTAT, "Mapping_gcamdata_FAO_iso_reg")) ->
      L107.Traceable_FBS_PCe_2010Plus

    L107.Traceable_FBS_Food_Calorie_Macronutrient_2010Plus %>%
      add_title("L107.Traceable_FBS_Food_Calorie_Macronutrient_2010Plus") %>%
      add_units("MKcal, MtFat, MtProtein") %>%
      add_comments("Dietary energy and macronutrients for food supply in PCe, FAO countries, APE commoditeis, for 2010 +") %>%
      add_precursors("L106.SUA_food_macronutrient_rate",
                     FILE = file.path(DIR_RAW_DATA_FAOSTAT, "Mapping_gcamdata_SUA_PrimaryEquivalent"),
                     FILE = file.path(DIR_RAW_DATA_FAOSTAT, "Mapping_gcamdata_SUA_ItemCode"),
                     file.path(DIR_RAW_DATA_FAOSTAT, "Mapping_gcamdata_FAO_iso_reg")) ->
      L107.Traceable_FBS_Food_Calorie_Macronutrient_2010Plus


    return_data(MODULE_OUTPUTS)

  } else {
    stop("Unknown command")
  }
}
