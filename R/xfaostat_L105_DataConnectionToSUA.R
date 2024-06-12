# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_xfaostat_L105_DataConnectionToSUA
#'
#' Connect datasets to build SUA
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs, a vector of output names, or (if
#'   \code{command} is "MAKE") all the generated outputs
#' @details This chunk compiles balanced supply utilization data bu connecting FAO datasets.
#' @importFrom assertthat assert_that
#' @importFrom dplyr summarize bind_rows filter if_else inner_join left_join mutate rename select n group_by_at
#' first case_when vars transmute
#' @importFrom tibble tibble
#' @importFrom tidyr complete drop_na gather nesting spread replace_na fill
#' @author XZ 2023
module_xfaostat_L105_DataConnectionToSUA <- function(command, ...) {

  MODULE_INPUTS <-
    c(FILE = file.path(DIR_RAW_DATA_FAOSTAT, "FAO_items"),
      "QCL_PROD",
      "QCL_AN_LIVEANIMAL_MEATEQ",
      "TCL_wide",
      "TM_bilateral_wide",
      "FBSH_CBH_wide",
      "FBS_wide",
      "SCL_wide")

  MODULE_OUTPUTS <-
    c("Bal_new_all")

  if(command == driver.DECLARE_INPUTS) {
    return(MODULE_INPUTS)
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(MODULE_OUTPUTS)
  } else if(command == driver.MAKE) {

    year <- value <- Year <- Value <- FAO_country <- iso <- NULL    # silence package check.
    SCL_wide <- element_code <- element <- area_code <- item_code <- area <-
      item <- unit <- FBS_wide <- FBSH_CBH_wide <- TCL_wide <- TM_bilateral_wide <-
      QCL_PROD <- FAO_items <- tier <- QCL <- oil <-
      cake <- SCL_item_oil <- SCL_item_cake <- cake_rate <- cake_rate_world <-
      DS_key_coproduct_item <- Production <- Import <- Export <- DS_demand <-
      DS_production <- CoproductRate <- QCL_AN_LIVEANIMAL_MEATEQ <- `Closing stocks` <-
      `Opening stocks` <- `Stock Variation` <- NULL

    all_data <- list(...)[[1]]

    # Load required inputs ----

    get_data_list(all_data, MODULE_INPUTS, strip_attributes = TRUE)


    # Wide to long ----

    SCL_wide %>% gather_years() %>%
      filter(year >= min(FAOSTAT_Hist_Year_FBS)) %>%
      FAOSTAT_AREA_RM_NONEXIST() -> SCL

    FBS_wide %>% gather_years() %>%
      filter(year >= min(FAOSTAT_Hist_Year_FBS)) %>%
      FAOSTAT_AREA_RM_NONEXIST() -> FBS

    FBSH_CBH_wide %>% gather_years() %>%
      filter(year >= min(FAOSTAT_Hist_Year_FBS)) %>%
      FAOSTAT_AREA_RM_NONEXIST() -> FBSH_CB

    TCL_wide %>% gather_years() %>%
      filter(year >= min(FAOSTAT_Hist_Year_FBS)) %>%
      FAOSTAT_AREA_RM_NONEXIST() -> TCL

    TM_bilateral_wide %>% gather_years() %>%
      filter(year >= min(FAOSTAT_Hist_Year_FBS)) %>%
      filter(value > 0) -> TM_bilateral

    rm(SCL_wide, FBS_wide, FBSH_CBH_wide, TCL_wide, TM_bilateral_wide)


    # Get area code in QCL that is consistent with FBS e.g., after 2010 only
    QCL_PROD %>% filter(year >= min (FAOSTAT_Hist_Year_FBS)) %>%  distinct(area_code) %>% pull ->
      QCL_area_code_FBS

    ## 1.2. Get FAO supply-utilization SCL ready ----

    SCL %>% distinct(element)
    # Update SCL element name for convenience
    SCL %>% mutate(element = gsub(" Quantity| supply quantity \\(tonnes\\)| \\(non-food\\)", "", element)) ->
      SCL
    SCL_element_new <-
      c("Opening stocks", "Production", "Export", "Import", "Stock Variation",
        "Food", "Feed", "Seed", "Processed", "Other uses",
        "Tourist consumption", "Loss", "Residuals")

    # SCL has all elements in SCL_element_new
    assertthat::assert_that(setdiff(SCL_element_new, unique(SCL$element)) %>% length == 0)

    ##  1.3. Get gross trade data from bilateral (TM) ----
    #  Trade is balanced if aggregating bilateral but need to filter in QCL_area_code_FBS

    TM_bilateral %>%
      filter(year >= min(FAOSTAT_Hist_Year_FBS)) %>%
      filter(area_code %in% QCL_area_code_FBS,
             source_code %in% QCL_area_code_FBS,
             element %in% c("Import Quantity")) %>%
      mutate(element = "Import") %>%
      group_by(area_code, item_code, element, year) %>%
      summarise(value = sum(value, na.rm = T), .groups = "drop") %>%
      bind_rows(
        TM_bilateral %>%
          filter(year >= min(FAOSTAT_Hist_Year_FBS)) %>%
          filter(area_code %in% QCL_area_code_FBS,
                 source_code %in% QCL_area_code_FBS,
                 element %in% c("Import Quantity")) %>%
          mutate(element = "Export") %>%
          group_by(area_code = source_code, item_code, element, year) %>%
          summarise(value = sum(value, na.rm = T), .groups = "drop") ) -> TCL_TM
    rm(TM_bilateral)

    ## 1.4. Get gross trade data from FAO gross trade (TCL) ----
    TCL %>%
      filter(year >= min(FAOSTAT_Hist_Year_FBS)) %>%
      filter(area_code %in% QCL_area_code_FBS,
             element %in% c("Import Quantity")) %>%
      mutate(element = "Import") %>%
      bind_rows(
        TCL %>%
          filter(year >= min(FAOSTAT_Hist_Year_FBS)) %>%
          filter(area_code %in% QCL_area_code_FBS,
                 element %in% c("Export Quantity")) %>%
          mutate(element = "Export") ) %>%
      select(area_code, item_code, element, year, value) -> TCL_gross
    rm(TCL)

    ## 1.5. Get FBS data (FBS) ----
    FBS %>%
      filter(year >= min(FAOSTAT_Hist_Year_FBS)) %>%
      # keep only balance items
      filter(!element_code %in% c(645, 664, 674, 684)) %>%
      # simplify elements and make them consistent with SUA
      mutate(element = gsub(" Quantity| supply quantity \\(tonnes\\)| \\(non-food\\)", "", element),
             element = replace(element, element == "Losses", "Loss"),
             element = replace(element, element == "Processing", "Processed")) %>%
      # convert units back to tonnes first since FBS originally used 1000 tons
      mutate(value = value * 1000, unit = "tonnes") ->
      FBS

    # FBS has all needed elements in FBS_element_new
    assertthat::assert_that(setdiff(SCL_element_new, unique(FBS$element)) == "Opening stocks")

    ## 1.6. Define balance elements ----

    # Defined in constants
    # c("Opening stocks", "Production", "Import",
    #   "Export", "Processed", "Food", "Feed", "Seed", "Other uses", "Loss", "Closing stocks",
    #   "Residuals", "Regional supply", "Regional demand", "Stock Variation") ->
    #   Bal_element_new  # used in Get_SUA_TEMPLATE and SUA_bal_adjust

    ## 1.7. Filter data by year and merge regions needed----
    # Merge Sudan regions to be consistent with data
    # Mainly for storage data concerns
    # And only keep data > min(FAOSTAT_Hist_Year_FBS)
    for (.DF in c("SCL", "TCL_TM", "TCL_gross", "FBSH_CB", "FBS", "QCL_PROD")) {
      get(.DF) %>% filter(year >= min(FAOSTAT_Hist_Year_FBS)) %>%
        # merge Sudan and South Sudan
        FAO_AREA_DISAGGREGATE_HIST_DISSOLUTION_ALL(SUDAN2012_MERGE = T) %>%
        assign(x = .DF, envir = parent.env(environment()))  }


    # Update area code in QCL
    QCL_PROD %>% filter(year %in% FAOSTAT_Hist_Year_FBS) %>%  distinct(area_code) %>% pull ->
      QCL_area_code_FBS

    # 2. Create helper functions to simplify join by data set ----


    ## 2.1. FN: Get full template and fill in data by Tier ----

    # A function expand.grid to get full set of element, area, year, and item (by Tier)
    # The template will be joined by data from different sources later
    # Note that item code is usually used to ensure a best match but item can also be used
    # Note that Sudan after 2012 (South Sudan and Sudan) are merged
    Get_SUA_TEMPLATE <- function(.ITEM_CODE = NULL, .ITEM = NULL, .YEAR = FAOSTAT_Hist_Year_FBS){

      #Item code will be used first if available
      if (!is.null(.ITEM_CODE)) {
        expand.grid(area_code = QCL_area_code_FBS,
                    year = .YEAR,
                    item_code = .ITEM_CODE,
                    element = SCL_element_new) %>%
          # Adding this to fix Sudan breakup after 2010
          FAOSTAT_AREA_RM_NONEXIST(SUDAN2012_MERGE = T) -> .DF
        return(.DF)
      }

      if (!is.null(.ITEM)) {
        expand.grid(area_code = QCL_area_code_FBS,
                    year = .YEAR,
                    item = .ITEM,
                    element = SCL_element_new) %>%
          # Adding this to fix Sudan breakup after 2010
          FAOSTAT_AREA_RM_NONEXIST(SUDAN2012_MERGE = T) -> .DF
        return(.DF)
      }

    }


    ## 2.2. FN: Join data to template by source ----

    SUA_TEMPLATE_LEFT_JOIN <- function(.DF, .DS, .DS_TM_Assert_Item = T){


      ## Start SUA_TEMPLATE_LEFT_JOIN ----

      ## a. Join QCL_PROD when .DS == "QCL" ----
      if (.DS == "QCL") {

        # assert QCL_PROD exist
        assertthat::assert_that(is.data.frame(QCL_PROD))

        # assert items exist in joined DF
        assertthat::assert_that(.DF %>% distinct(item_code) %>% pull %>%
                                  setdiff(QCL_PROD %>% distinct(item_code) %>% pull) %>%
                                  length() == 0 )
        # Join
        .DF %>% left_join(
          QCL_PROD %>%
            select(area_code, item_code, element, year, QCL = value),
          by = c("area_code", "item_code", "element", "year")
        ) -> .DF1
        return(.DF1)
      }


      ## b. Join TCL_TM when .DS == "TM" ----
      if (.DS == "TM") {

        # assert QCL_PROD exist
        assertthat::assert_that(is.data.frame(TCL_TM))

        if (.DS_TM_Assert_Item == T) {
          # assert items exist in joined DF
          assertthat::assert_that(.DF %>% distinct(item_code) %>% pull %>%
                                    setdiff(TCL_TM %>% distinct(item_code) %>% pull) %>%
                                    length() == 0 )
        }
        # Join
        .DF %>%
          left_join(
            TCL_TM %>% select(area_code, item_code, element, year, TCL = value),
            by = c("area_code", "item_code", "element", "year")
          ) -> .DF1
        return(.DF1)
      }


      ## c. Join TCL_gross when .DS == "TCL_gross" ----
      if (.DS == "TCL_gross") {

        # assert QCL_PROD exist
        assertthat::assert_that(is.data.frame(TCL_gross))

        if (.DS_TM_Assert_Item == T) {
          # assert items exist in joined DF
          assertthat::assert_that(.DF %>% distinct(item_code) %>% pull %>%
                                    setdiff(TCL_gross %>% distinct(item_code) %>% pull) %>%
                                    length() == 0 )
        }
        # Join
        .DF %>% left_join(
          TCL_gross %>% select(area_code, item_code, element, year, TCL_gross = value),
          by = c("area_code", "item_code", "element", "year")
        ) -> .DF1
        return(.DF1)
      }


      ## d. Join SCL when .DS == "SCL" ----
      if (.DS == "SCL") {

        # assert QCL_PROD exist
        assertthat::assert_that(is.data.frame(SCL))

        # assert items exist in joined DF
        assertthat::assert_that(
          .DF %>% distinct(item_code) %>% pull %>%
            setdiff(SCL %>% distinct(item_code) %>% pull) %>%
            length() == 0 )
        # Join
        .DF %>% left_join(
          SCL %>% select(area_code, item_code, element, year, SCL = value),
          by = c("area_code", "item_code", "element", "year")
        ) -> .DF1
        return(.DF1)
      }

      ## e. Join QCL_Cake when .DS == "QCL_Cake" ----
      if (.DS == "QCL_Cake") {

        # assert QCL_Cake exist
        assertthat::assert_that(is.data.frame(QCL_Cake))

        # assert items exist in joined DF
        assertthat::assert_that(.DF %>% distinct(item_code) %>% pull %>%
                                  setdiff(QCL_Cake %>% distinct(item_code) %>% pull) %>%
                                  length() == 0 )
        # Join
        .DF %>%left_join(
          QCL_Cake %>%
            select(area_code, item_code, element, year, QCL = value),
          by = c("area_code", "item_code", "element", "year")
        ) -> .DF1
        return(.DF1)
      }


      ## f. Join QCL_Coproduct when .DS == "QCL_Coproduct" ----
      if (.DS == "QCL_Coproduct") {

        # assert QCL_Coproduct exist
        assertthat::assert_that(is.data.frame(QCL_Coproduct))

        # assert items exist in joined DF
        assertthat::assert_that(.DF %>% distinct(item_code) %>% pull %>%
                                  setdiff(QCL_Coproduct %>% distinct(item_code) %>% pull) %>%
                                  length() == 0 )
        # Join
        .DF %>%left_join(
          QCL_Coproduct %>%
            select(area_code, item_code, element, year, value),
          by = c("area_code", "item_code", "element", "year")
        ) -> .DF1
        return(.DF1)
      }


      ## g. Join FBS when .DS == "FBS" ----
      if (.DS == "FBS") {

        # assert FBS exist
        assertthat::assert_that(is.data.frame(FBS))

        # assert items exist in joined DF
        assertthat::assert_that(.DF %>% distinct(item_code) %>% pull %>%
                                  setdiff(FBS %>% distinct(item_code) %>% pull) %>%
                                  length() == 0 )
        # Join
        .DF %>%left_join(
          FBS %>%
            select(area_code, item_code, element, year, value),
          by = c("area_code", "item_code", "element", "year")
        ) -> .DF1
        return(.DF1)
      }

      ## h. Join APE_live_an_MeatEQ when .DS == "APE_live_an_MeatEQ" ----
      if (.DS == "APE_live_an_MeatEQ") {

        # assert FBS exist
        assertthat::assert_that(is.data.frame(APE_live_an_MeatEQ))

        # assert items exist in joined DF
        assertthat::assert_that(.DF %>% distinct(item_code) %>% pull %>%
                                  setdiff(APE_live_an_MeatEQ %>% distinct(item_code) %>% pull) %>%
                                  length() == 0 )
        # Join
        .DF %>%left_join(
          APE_live_an_MeatEQ %>%
            select(area_code, item_code, element, year, value),
          by = c("area_code", "item_code", "element", "year")
        ) -> .DF1
        return(.DF1)
      }

      ## Done SUA_TEMPLATE_LEFT_JOIN ----
    }


    ## 2.3. FN: Balance gross trade ----

    #GROSS_TRADE_ADJUST function moved to helper functions



    # 3. Process items in FAO_items to get Balanced SUA data ----
    ## 3.1 Bal_new_tier1 ----
    # Tier1 includes 209 = 210-1 items with best sources e.g. bilateral trade (TM)  prodstat (QCL) and supply-utilization-account (SCL)
    # Note that item 237 Oil soybean was moved from Tier1 to Tier2 to use SCL for production due to Brazil data issue in QCL
    # SCL has balanced data processed by FAO but the quality was poor with low consistency


    Get_SUA_TEMPLATE(.ITEM_CODE = FAO_items %>% filter(tier == 1) %>% pull(item_code)) %>%
      SUA_TEMPLATE_LEFT_JOIN("QCL") %>%
      SUA_TEMPLATE_LEFT_JOIN("TM") %>%
      SUA_TEMPLATE_LEFT_JOIN("SCL") %>%
      mutate(value = case_when(
        element %in% c("Production") ~ QCL, #prod in QCL is used and not overwritten in case_when
        element %in% c("Export", "Import") ~ TCL,
        element %in% SCL_element_new ~ SCL) ) %>%
      select(-QCL, -TCL, -SCL) %>%
      # Adjust for balance across all dimensions
      SUA_bal_adjust %>%   # Unit is converted to 1000 tonnes!
      left_join(FAO_items %>% select(item_code, item), by = "item_code") ->
      Bal_new_tier1


    assert_FBS_balance(Bal_new_tier1)

    ## 3.2 Bal_new_tier2 ----
    # Tier2 includes 204 items that had no data or low quality data in QCL so used production from SCL

    Get_SUA_TEMPLATE(.ITEM_CODE = FAO_items %>% filter(tier == 2) %>% pull(item_code)) %>%
      SUA_TEMPLATE_LEFT_JOIN("TM") %>%
      SUA_TEMPLATE_LEFT_JOIN("SCL") %>%
      mutate(value = case_when(
        element %in% c("Export", "Import") ~ TCL,
        element %in% SCL_element_new ~ SCL) ) %>%
      select(-TCL, -SCL)  %>%
      SUA_bal_adjust %>%   # Unit is converted to 1000 tonnes!
      left_join(FAO_items %>% select(item_code, item), by = "item_code") ->
      Bal_new_tier2

    assert_FBS_balance(Bal_new_tier2)


    ## 3.3 Bal_new_tier3 ----
    # Tier3 includes 21 items that had QCL but no bilateral trade data
    # so use gross trade from SCL

    Get_SUA_TEMPLATE(.ITEM_CODE = FAO_items %>% filter(tier == 3) %>% pull(item_code)) %>%
      SUA_TEMPLATE_LEFT_JOIN("SCL")  %>%
      # light cleaning here since more missing data were seen for this group
      # set NA stock variation to zero to avoid fill NA later
      mutate(SCL = if_else(is.na(SCL) & element == "Stock Variation", 0, SCL)) %>%
      group_by(area_code, item_code, element) %>%
      #fill in NA up-down across years
      fill(SCL) %>% fill(SCL, .direction = "updown") %>% ungroup() %>%
      SUA_TEMPLATE_LEFT_JOIN("QCL") %>%
      mutate(value = case_when(
        #element %in% c("Export", "Import") & item_code == 895 ~ 0, # fix/remove trade for Milk, skimmed evaporated
        element %in% c("Production") ~ QCL, #prod in QCL is used and not overwritten by SCL
        element %in% SCL_element_new ~ SCL) ) %>%
      replace_na(list(value = 0)) %>%
      select(-QCL, -SCL) %>%
      GROSS_TRADE_ADJUST(.MIN_TRADE_PROD_RATIO = 0.01) %>%
      SUA_bal_adjust %>%   # Unit is converted to 1000 tonnes!
      left_join(FAO_items %>% select(item_code, item), by = "item_code") ->
      Bal_new_tier3

    assert_FBS_balance(Bal_new_tier3)

    ## 3.4 Bal_new_tier4 ----
    # Tier4 includes 40 items included in SCL but not in Tier1-3

    Get_SUA_TEMPLATE(.ITEM_CODE = FAO_items %>% filter(tier == 4) %>% pull(item_code)) %>%
      SUA_TEMPLATE_LEFT_JOIN("SCL") %>%
      # light cleaning here since more missing data were seen for this group
      rename(value = SCL)  %>%
      mutate(value = if_else(is.na(value) & element == "Stock Variation", 0, value)) %>%
      group_by(area_code, item_code, element) %>%
      #fill in NA up-down across years
      fill(value, .direction = "updown") %>% ungroup() %>%
      replace_na(list(value = 0)) %>%
      # Gross trade is adjusted since the source was not bilateral trade
      GROSS_TRADE_ADJUST(.MIN_TRADE_PROD_RATIO = 0.01) %>%
      SUA_bal_adjust %>%   # Unit is converted to 1000 tonnes!
      left_join(FAO_items %>% select(item_code, item), by = "item_code") ->
      Bal_new_tier4

    assert_FBS_balance(.DF = Bal_new_tier4)


    ## 3.5 Bal_new_tier5 ----
    #Tier5 includes 12 fish items from FBS and FBSH. Item code came from FBS as well

    Get_SUA_TEMPLATE(.ITEM_CODE = FAO_items %>% filter(tier == 5) %>% pull(item_code)) %>%
      SUA_TEMPLATE_LEFT_JOIN("FBS") %>%
      mutate(value = if_else(is.na(value) & element == "Stock Variation", 0, value)) %>%
      group_by(area_code, item_code, element) %>%
      #fill NA up-down across year
      fill(value, .direction = "updown") %>%
      ungroup() %>%
      replace_na(list(value = 0)) %>%
      GROSS_TRADE_ADJUST(.MIN_TRADE_PROD_RATIO = 0.01) %>%
      SUA_bal_adjust %>%  # Unit is converted to 1000 tonnes!
      left_join(FAO_items %>% select(item_code, item), by = "item_code") ->
      Bal_new_tier5

    assert_FBS_balance(.DF = Bal_new_tier5)


    ## 3.6 Bal_new_tier6 ----
    # Tier6 includes 29 items that included in QCL for production but not in Tier1 to Tier5
    # "Rice, paddy (rice milled equivalent)" removed as not needed and excluded by FAOSTAT in 2023
    # 773 (Flax, processed but not spun) is changed to 771 (Flax, raw or retted)

    Get_SUA_TEMPLATE(.ITEM_CODE = FAO_items %>% filter(tier == 6) %>% pull(item_code)) %>%
      SUA_TEMPLATE_LEFT_JOIN("QCL") %>%
      SUA_TEMPLATE_LEFT_JOIN("TM", .DS_TM_Assert_Item = F) %>%
      SUA_TEMPLATE_LEFT_JOIN("TCL_gross", .DS_TM_Assert_Item = F) %>%
      mutate(TCL = if_else(is.na(TCL), TCL_gross, TCL),
             value = case_when(
        element %in% c("Area harvested", "Production") ~ QCL, #prod in QCL is used and not overwritten
        element %in% c("Export", "Import") ~ TCL,
        element %in% SCL_element_new ~ 0) ) %>%
      select(-QCL, -TCL, -TCL_gross) %>%
      replace_na(list(value = 0)) %>%
      GROSS_TRADE_ADJUST(.MIN_TRADE_PROD_RATIO = 0.01) %>%
      spread(element, value) %>%
      # Processing to add demand based on DS_demand in FAO_items
      # Only an exclusive use is assumed
      mutate(Processed = if_else(item_code %in% c(FAO_items %>%
                                                   filter(tier == 6, grepl("Processed", DS_demand)) %>%
                                                   pull(item_code) ) & (Production + Import - Export) > 0,
                                (Production + Import - Export), 0),
             Food = if_else(item_code %in% c(FAO_items %>%
                                              filter(tier == 6, grepl("Food", DS_demand)) %>%
                                              pull(item_code) ) & (Production + Import - Export) > 0,
                           (Production + Import - Export), 0),
             `Other uses` = if_else(item_code %in% c(FAO_items %>%
                                                      filter(tier == 6, grepl("Other", DS_demand)) %>%
                                                      pull(item_code) ) & (Production + Import - Export) > 0,
                                   (Production + Import - Export), 0)) %>%
      gather(element, value, -area_code, -item_code, -year) %>%
      SUA_bal_adjust %>%  # Unit is converted to 1000 tonnes!
      left_join(FAO_items %>% select(item_code, item), by = "item_code") ->
      Bal_new_tier6

    assert_FBS_balance(.DF = Bal_new_tier6)


    ## 3.7 Bal_new_tier7 ----
    # Tier8 include 4 cake or fiber items that coproduced in oil crop crushing. Item code used 0
    #  Note the FAOSTAT does not provide data for Tier8 item so they are generated based extraction rate and assumed no trade and single use.

    ### 3.7.1 Get production of the main product and process coproduction ----
    # Get the production of the corresponding coproducing items from processed Tier 2

    Bal_new_tier2 %>%
      select(area_code, year, element, coproduct_item_code = item_code, value) %>%
      filter(element == "Production") %>%
      mutate(value = value * 1000) %>%  # convert units back to tonne!!!
      # Join to keep Tier 8 items
      right_join(FAO_items %>% filter(tier == 7) %>%
                   # Get co-production rate from DS_production which is uniform across regions
                   mutate(CoproductRate = as.numeric(gsub("Coproduction_Rate \\(|)","", DS_production))) %>%
                   select(item_code, item, coproduct_item = DS_key_coproduct_item, coproduct_item_code = DS_key_coproduct_item_code, CoproductRate),
                 by = "coproduct_item_code") %>%
      transmute(area_code, year, item_code, item, element, value = value * CoproductRate) ->
      QCL_Coproduct


    ### 3.7.2 Process to get Bal_new_tier7 ----
    Get_SUA_TEMPLATE(.ITEM_CODE = FAO_items %>% filter(tier == 7) %>% pull(item_code)) %>%
      SUA_TEMPLATE_LEFT_JOIN("QCL_Coproduct") %>%
      replace_na(list(value = 0)) %>%
      spread(element, value) %>%
      # Processing to add demand based on DS_demand in FAO_items
      # Only an exclusive use is assumed
      mutate(Feed = if_else(item_code %in% c(FAO_items %>% filter(tier == 7, grepl("Feed", DS_demand)) %>%
                                              pull(item_code) ) & Production > 0,
                           Production, 0),
             `Other uses` = if_else(item_code %in% c(FAO_items %>% filter(tier == 7, grepl("Other", DS_demand)) %>%
                                                      pull(item_code) ) & Production > 0,
                                   Production, 0) ) %>%
      gather(element, value, -area_code, -item_code, -year) %>%
      SUA_bal_adjust %>%  # Unit is converted to 1000 tonnes!
      left_join(FAO_items %>% select(item, item_code), by = "item_code") ->
      Bal_new_tier7

    assert_FBS_balance(.DF = Bal_new_tier7)
    rm(QCL_Coproduct)

    ## 3.8 Bal_new_tier8 ----
    # Tier9 includes 16 meat equivalent items converted from live animals. Item code used 0
    #  Note that these items were preprocessed based on mainly carcass yield implied by data for adjusting production and stocks
    #  Note that for meat equivalent items only stock variation was accounted since 2010 when the data first available

    ### 3.8.1 Process the live animal meat equivalent data APE_live_an_MeatEQ ----

    # read in QCL_AN_LIVEANIMAL_MEATEQ live animal meat equivalent
    # Treat live animal as stock and adjust using production or other demand
    # Milk cattle is not included
    # Note that only stock variation is used
    # Because live animal stock is a capital stock included elsewhere
    # But accounting delta allows more accurate estimate of feed uses
    # E.g., additional feed demand due to animal expansion

    QCL_AN_LIVEANIMAL_MEATEQ %>%
      select(area_code, item_code, year, value) %>%
      #mutate(item = gsub("Meat", "AnMeatEq", item)) %>%
      # convert units back to tonne!!! And adjust item_code
      mutate(value = value * 1000, unit = "tonnes",
             item_code = item_code * 10000) %>%
      group_by(item_code, area_code) %>%
      mutate(`Opening stocks` = value,
             `Closing stocks` = dplyr::lead(value)) %>%
      # fill down missing Closing stocks due to lead value
      fill(`Closing stocks`) %>% ungroup() %>%
      # Adjust using meatEQ production or other uses
      mutate(`Stock Variation` = `Closing stocks` - `Opening stocks`,
             Production = if_else(`Stock Variation` > 0, `Stock Variation`, 0),
             `Other uses` = if_else(`Stock Variation` < 0, -`Stock Variation`, 0)
      ) %>% ungroup() %>%
      gather(element, value, c("Opening stocks", "Closing stocks", "Production", "Other uses")) %>%
      transmute(area_code, year, item_code, element, value) ->
      APE_live_an_MeatEQ

    ### 3.8.2 Process to get Bal_new_tier9 ----
    Get_SUA_TEMPLATE(.ITEM_CODE = FAO_items %>% filter(tier == 8) %>% pull(item_code)) %>%
      SUA_TEMPLATE_LEFT_JOIN("APE_live_an_MeatEQ") %>%
      spread(element, value) %>%
      # only keep net openning stock in the study period
      group_by(item_code, area_code) %>%
      mutate(`Opening stocks` = `Opening stocks` - min(`Opening stocks`)) %>%
      ungroup() %>%
      gather(element, value, -area_code, -item_code, -year) %>%
      SUA_bal_adjust %>%  # Unit is converted to 1000 tonnes!
      left_join(FAO_items %>% select(item, item_code), by = "item_code") ->
      Bal_new_tier8

    assert_FBS_balance(.DF = Bal_new_tier8)
    rm(QCL_AN_LIVEANIMAL_MEATEQ, APE_live_an_MeatEQ)


    # 4. Bind all to get Bal_new_all ----
    #[ToDo loop not working in package]
    # lapply(paste0("Bal_new_tier", 1:9), get) %>% bind_rows() %>%
    #   # Add area_code
    #   left_join(QCL_PROD %>% distinct(area, area_code), by = "area_code") ->
    #   Bal_new_all

      Bal_new_tier1 %>%
        bind_rows(Bal_new_tier2) %>%
        bind_rows(Bal_new_tier3) %>%
        bind_rows(Bal_new_tier4) %>%
        bind_rows(Bal_new_tier5) %>%
        bind_rows(Bal_new_tier6) %>%
        bind_rows(Bal_new_tier7) %>%
        bind_rows(Bal_new_tier8) %>%
      # Add area_code
      left_join(QCL_PROD %>% distinct(area, area_code), by = "area_code")->
      Bal_new_all

    assert_FBS_balance(.DF = Bal_new_all)

    rm(TCL_gross, TCL_TM, SCL, FBS, FBSH_CB, FAO_items)
    rm(list = ls(pattern = "Bal_new_tier*"))


    Bal_new_all %>%
      add_title("Bal_new_all") %>%
      add_units("Ktonne") %>%
      add_comments("Preprocessed FAO SUA 2010 - 2021") %>%
      add_precursors(file.path(DIR_RAW_DATA_FAOSTAT, "FAO_items"),
                     "QCL_PROD",
                     "QCL_AN_LIVEANIMAL_MEATEQ",
                     "TCL_wide",
                     "TM_bilateral_wide",
                     "FBSH_CBH_wide",
                     "FBS_wide",
                     "SCL_wide")->
      Bal_new_all


    return_data(MODULE_OUTPUTS)

  } else {
    stop("Unknown command")
  }
}


# Update log
# The old tier 5 was not needed (oil seed cake) as the data is available

# ***Generate/check FAO_items ----

#  Curr_Envir <- environment()
#
#  FAOSTAT_load_raw_data("TM", GET_MAPPINGCODE = "ItemCodes", .Envir = Curr_Envir)
#  TM_ItemCodes
#
#  FAOSTAT_load_raw_data("SCL", GET_MAPPINGCODE = "ItemCodes", .Envir = Curr_Envir)
#
#  SCL_ItemCodes
#  SCL %>% distinct(item)
#
#  TCL_TM %>% mutate(item = 1) -> TCL_TM1
#
#  FF_join_checkmap(c("SCL", "QCL_PROD", "TCL_TM1", "FBSH_CB"), COL_by = c("item_code"), COL_rename = "item" ) ->
#    JoinItemMap
#
#
#  JoinItemMap %>%
#    filter( !(is.na(SCL_item)|is.na(QCL_PROD_item)|is.na(TCL_TM1_item) )) %>%
#    # remove soy oil here and others had data issue
#    filter(!item_code %in% c(237, 982, 953, 311) ) %>%
#    select(item_code, item = QCL_PROD_item) %>%  mutate(tier = 1) -> Tier1
#
#  JoinItemMap %>% anti_join(Tier1, by = "item_code") %>%
#    filter(!(is.na(SCL_item)|is.na(TCL_TM1_item))) %>%
#    select(item_code, item = SCL_item) %>%  mutate(tier = 2) -> Tier2
#
#  JoinItemMap %>%
#    anti_join(Tier1, by = "item_code") %>%
#    anti_join(Tier2, by = "item_code") %>%
#    filter(!(is.na(SCL_item)|is.na(QCL_PROD_item))) %>%
#    select(item_code, item = SCL_item) %>%  mutate(tier = 3) -> Tier3
#
#  JoinItemMap %>%
#    anti_join(Tier1, by = "item_code") %>%
#    anti_join(Tier2, by = "item_code") %>%
#    anti_join(Tier3, by = "item_code") %>%
#    filter(!is.na(SCL_item)) %>%
#    select(item_code, item = SCL_item) %>%  mutate(tier = 4) -> Tier4
#
#  # 12 Fish items
#  JoinItemMap %>%
#    filter(!(is.na(FBSH_CB_item)|is.na(QCL_PROD_item)),
#           is.na(SCL_item)) %>%
#    select(item_code, item = FBSH_CB_item) %>%  mutate(tier = 7) -> Tier5
#
#  assertthat::assert_that(nrow(Tier5) == 12)
#
#  JoinItemMap %>%
#    anti_join(Tier1, by = "item_code") %>%
#    anti_join(Tier2, by = "item_code") %>%
#    anti_join(Tier3, by = "item_code") %>%
#    anti_join(Tier4, by = "item_code") %>%
#    anti_join(Tier5, by = "item_code") %>%
#    filter(!is.na(QCL_PROD_item)) %>%
#    select(item_code, item = QCL_PROD_item) %>%  mutate(tier = 6) %>%
#    # item_code (30 paddy rice milled equivalent) is not included as it is not used and discontinued by FAOSTAT
#    filter(item_code != 30) -> Tier6
#
#  Tier6 %>% left_join(
#    FAO_items %>% filter(tier == 6) %>% select(item_code, DS_trade, DS_production, DS_demand, DS_key_coproduct_item)
#  )  %>% replace_na(list(DS_demand = "Other use only")) -> Tier6
#
#  Tier6 %>% inner_join(
#    TCL_TM %>% distinct(item_code)
#  ) %>% mutate(DS_trade = "TM") %>%
#    bind_rows(
#      Tier6 %>% anti_join(
#        TCL_TM %>% distinct(item_code)) %>% mutate(DS_trade = "TM")
#    ) %>%
#    mutate(DS_production = "QCL") -> Tier6
#
#
#  FAO_items %>% filter(tier %in% 8) %>%
#    select(item_code, item, DS_trade, DS_production, DS_demand, DS_key_coproduct_item) %>%  mutate(tier = 8) %>%
#    filter(!grepl("rice|maize|hempseed|linseed|kapok|poppy|safflower", item)) %>%
#    mutate(coproduct_item_code = c(274, 278, 332, 340))-> Tier7
#
# # "Oil of olive residues|Jojoba oil|Cake of cottonseed|Other oil of vegetable origin, crude n.e.c."
#
#  FAO_items %>% filter(tier %in% 9) %>%
#    select(item_code, item) %>%  mutate(tier = 9) -> Tier8
#
#  Tier1 %>% mutate(DS_trade = "TM", DS_production = "QCL", DS_demand = "SCL") %>%
#    bind_rows(Tier2 %>% mutate(DS_trade = "TM", DS_production = "SCL", DS_demand = "SCL") ) %>%
#    bind_rows(Tier3 %>% mutate(DS_trade = "SCL", DS_production = "QCL", DS_demand = "SCL") ) %>%
#    bind_rows(Tier4 %>% mutate(DS_trade = "SCL", DS_production = "SCL", DS_demand = "SCL") ) %>%
#    bind_rows(Tier5 %>% mutate(DS_trade = "FBS_FBSH", DS_production = "FBS_FBSH", DS_demand = "FBS_FBSH") ) %>%
#    bind_rows(Tier6) %>%
#    bind_rows(Tier7) %>%
#    bind_rows(Tier8) -> Tier_All
#
#  FAO_items %>% filter(tier %in% 1:9) %>% anti_join(Tier_All, by = c("item_code"))
#  Tier_All %>% anti_join(FAO_items, by = c("item_code"))
#
#  Tier_All -> FAO_items
#

