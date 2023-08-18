# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_xfaostat_L106_FoodMacroNutrient
#'
#' Preprocess producer prices
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs, a vector of output names, or (if
#'   \code{command} is "MAKE") all the generated outputs
#' @details This chunk compiles balanced supply utilization data in primary equivalent in GCAM region and commodities.
#' @importFrom assertthat assert_that
#' @importFrom dplyr summarize bind_rows filter if_else inner_join left_join mutate rename select n group_by_at
#' first case_when vars filter_all any_vars
#' @importFrom tibble tibble
#' @importFrom tidyr complete drop_na gather nesting spread replace_na fill
#' @author XZ 2023
module_xfaostat_L106_FoodMacroNutrient <- function(command, ...) {

  MODULE_INPUTS <-
    c("SCL_wide",
      "FBS_wide",
      "OA",
      FILE = "aglu/FAO/FAO_an_items_cal_SUA",
      FILE = "aglu/FAO/MAPPING_FAO_FBS_SUA")

  MODULE_OUTPUTS <-
    c("SUA_food_macronutrient_rate")

  if(command == driver.DECLARE_INPUTS) {
    return(MODULE_INPUTS)
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(MODULE_OUTPUTS)
  } else if(command == driver.MAKE) {

    year <- value <- Year <- Value <- FAO_country <- iso <- NULL    # silence package check.

    all_data <- list(...)[[1]]

    # Load required inputs ----

    get_data_list(all_data, MODULE_INPUTS, strip_attributes = TRUE)


    SCL_wide %>% gather_years() %>%
      FAOSTAT_AREA_RM_NONEXIST() -> SCL

    FBS_wide %>% gather_years() %>%
      FAOSTAT_AREA_RM_NONEXIST() -> FBS


    # Background----
    #*******************************************
    # Identifying food availability and macro-nutrient consumption
    # Reconciling FAO FBS and SUA dataset for global Diet composition estimates
    # Macro-nutrient: fat 9 kcal per g; protein and carbohydrates 4 kcal per g
    # Focus on FBS and SCL first as they both have data since 2010
    # Our World in Data example (FBSH to 2013): https://ourworldindata.org/diet-compositions#cereal-preferences-across-the-world

    #
    #
    # Goals----
    #*******************************************
    # Macro-nutrient mapping at SUA level to identify potential heterogeneity of cal/g across regions
    # For each SUA item, e.g., wheat flour, connect food consumption in balance to cal, protein, and fat; so carb
    # Need to check for each SUA item, whether the conversion rate changes

    # Compare FBS to SUA for the cal/ca accounting by FBS categories (matching well)
    # Recommend new mappings between FAO FBS and GCAM food categories
    # Note that the key of calculating cal, protein, fat consumption was the food supply in mass at the SUA level!
    # Cassidy et al. 2013 ERL used more simplified assumptions in FBSH, similar to many studies

    # More importantly and related to modeling, extraction rates and cal per g coefficient per SUA item determines over conversion
    # Thus, to get macro-nutrients we need to understand the conversion coefficient from mass units
    # First at the SUA level that is constant over time
    # And second, aggregated to FBS level and the mix of the SUA items and the depth of the processing chains matter



    # simplify population data
    OA %>% filter(element_code == 511, item_code == 3010) %>%
      transmute(area_code, year, pop = value) -> POP #1000 persons

    # Quick checks----
    #*******************************************
    # Check area
    Curr_Envir <- environment()
    # 19 countries not in FBS/SUA but in QCL
    # including (3 populous) Somalia, South Sudan, and Singapore (UAE was recently added in FAOSTAT)
    FF_join_checkmap(c("FBS", "SCL"), "area_code", "area", .ENVIR = Curr_Envir) -> checkarea

    #Check element
    FF_join_checkmap(c("FBS", "SCL"), "item_code", "item", .ENVIR = Curr_Envir) -> checkitem
    FF_join_checkmap(c("FBS", "SCL"), "element_code", "element", .ENVIR = Curr_Envir) -> checkelement
    #*******************************************
    # Check calories calculation first to make sure we can trace appropriately
    # the key here is elements, deal with missing values, and conversion rate (cal/g)
    # the goal is to get macro-nutrient conversion rates at the SUA level by area (constant across year)!
    # regional difference is limited by REGIONAL_NUTRIENT_MASS_CONV_OUTLIER_BOUNDARY

    # check unit
    SCL %>% distinct(element, unit)


    # Main processing----
    #*******************************************

    # it is not useful to calculate cal/g using `Food supply (kcal/capita/day)` /`Food supply quantity (g/capita/day)`
    # unit too small
    # `Calories/Year` / `Food supply quantity (tonnes)` is more accurate!
    # similarly for protein and fat
    # Use annual value in SUA to calculate the conversion rate!

    # For FBS (12 fish items) we only have `Food supply (kcal/capita/day)` /`Food supply quantity (g/capita/day)`

    ## Deal with SCL data first ----
    #*******************************************

    SCL %>% filter(element_code %in% c(261, 271, 281, 5141)) %>% #All 3 cal protein fats and food in ton
      right_join(MAPPING_FAO_FBS_SUA %>%
                   filter(!is.na(CPC_code)) %>%
                   select(item_code = SCL_item_code, FAO_FBS_code, FBS_label),
                 by = "item_code") %>%
      filter(!is.na(item)) %>%
      select(-element_code, -unit) ->
      SUA_food_macronutrient

    # calculate time-series mean at SUA levels
    SUA_food_macronutrient %>%
      filter(year %in% Hist_MEAN_Year_NUTRIENT_MASS_CONV) %>%
      spread(element, value) %>%
      dplyr::filter_all(any_vars(!is.na(.))) %>%
      mutate(calperg =  `Calories/Year` / `Food supply quantity (tonnes)` *1000,
             proteinperc =  `Proteins/Year` / `Food supply quantity (tonnes)` * 100,
             fatperc =  `Fats/Year` / `Food supply quantity (tonnes)` * 100) %>%
      select(area_code, item_code, item, year, calperg, proteinperc, fatperc) %>%
      gather(element, value, calperg, proteinperc, fatperc) %>%
      filter(is.finite(value), value > 0) %>% # calculate world mean for positive values later
      group_by(area_code, item_code, item, element) %>%
      summarise(value = mean(value, na.rm = T), .groups = "drop") %>%
      mutate(value = if_else(element == "calperg", round(value, -1), value)) %>% # round to nearest 10
      ungroup() ->
      SUA_food_yearmean

    # Check if any item has NA for all areas; affecting fill
    SUA_food_yearmean %>%
      group_by(item_code, item, element) %>% summarise(value = sum(value), .groups = "drop") %>%
      filter(value == 0) %>% spread(element, value) -> A # Empty

    # world mean (ex ante adjustment)
    SUA_food_yearmean %>%
      group_by(item_code, item, element) %>%
      summarise(value_world = mean(value, na.rm = T), .groups = "drop") %>%
      mutate(value_world = if_else(element == "calperg", round(value_world, -1), value_world))->
      SUA_food_yearareamean

    # fill regional NA using world mean
    SUA_food_yearmean %>% spread(area_code, value) %>%
      gather(area_code, value_reg, -item_code, -item, -element) %>%
      left_join(SUA_food_yearareamean, by = c("item_code", "item", "element")) %>%
      mutate(value_reg = if_else(is.na(value_reg), value_world, value_reg),
             Diff = value_reg -  value_world,
             p_Diff = Diff / value_world) ->
      SUA_food_yearmean_fill
    #*******************************************
    # check to make sure REGIONAL_NUTRIENT_MASS_CONV_OUTLIER_BOUNDARY is reasonable (compared with world mean)
    # REGIONAL_NUTRIENT_MASS_CONV_OUTLIER_BOUNDARY = 0.15
    SUA_food_yearmean_fill %>% group_by(element) %>%
      summarise(pmean = mean(p_Diff, na.rm = T),
                P05 = quantile(p_Diff, 0.15, na.rm = T), P95 = quantile(p_Diff, 0.85, na.rm = T),
                dmean = mean(Diff)) -> A
    rm(A)
    # data is okay generally
    # but set outliers (p_Diff > 0.15 or p_Diff < -0.15) to world conversion value
    # That is macro-nutrient coefficient per mass unit across regions should not be too different
    # smaller than +-15% roughly of ex ante simple average

    SUA_food_yearmean_fill %>%
      transmute(area_code, item_code, item, element,
                value = if_else(p_Diff > REGIONAL_NUTRIENT_MASS_CONV_OUTLIER_BOUNDARY| p_Diff< -REGIONAL_NUTRIENT_MASS_CONV_OUTLIER_BOUNDARY, value_world, value_reg)) %>%
      spread(element, value) %>%
      gather(element, value, -area_code, -item_code, -item) %>%  # replace na to zero for some fats and protein items
      mutate(area_code = as.integer(area_code)) %>%
      replace_na(list(value = 0)) %>%
      spread(element, value)->
      SUA_food_macronutrient_rate_nofish # Final product

    ## Add in non-SUA FBS items (12 fish items) ----
    #*******************************************

    # Adding the 12 fish item from FBS
    MAPPING_FAO_FBS_SUA %>% filter(is.na(CPC_code)) %>%
      select(item = FBS_label, item_code = FAO_FBS_code) -> Fish_item

    FBS %>% right_join(Fish_item, by = c("item_code", "item")) -> FBS_fish
    FBS_fish %>% distinct(element, element_code, unit)

    FBS_fish %>% filter(element_code %in% c(645, 664, 674, 684)) %>%
      select(-unit, -element_code) ->
      FBS_fish_food_macronutrient

    # calculate time-series mean at SUA/FBS levels
    FBS_fish_food_macronutrient %>% filter(year %in% Hist_MEAN_Year_NUTRIENT_MASS_CONV) %>%
      spread(element, value) %>%
      filter_all(any_vars(!is.na(.))) %>%
      mutate(calperg =  `Food supply (kcal/capita/day)` / `Food supply quantity (kg/capita/yr)`,
             proteinperc =  `Protein supply quantity (g/capita/day)` / `Food supply quantity (kg/capita/yr)` /1000 * 100,
             fatperc =  `Fat supply quantity (g/capita/day)` / `Food supply quantity (kg/capita/yr)` /1000 * 100) %>%
      select(area_code, item_code, item, year, calperg, proteinperc, fatperc) %>%
      gather(element, value, calperg, proteinperc, fatperc) %>%
      filter(is.finite(value), value > 0) %>% # calculate world mean for positive values later
      group_by(area_code, item_code, item, element) %>%
      summarise(value = mean(value, na.rm = T), .groups = "drop") %>%
      mutate(value = if_else(element == "calperg", round(value, -1), value)) %>% # round to nearest 10
      ungroup() ->
      FBS_fish_food_yearmean
    # The data quality is too poor from FBS for deriving the conversion rates
    rm(FBS_fish, FBS_fish_food_macronutrient, FBS_fish_food_yearmean)

    # Change strategy here by using fixed values from
    # https://www.fao.org/3/X9892E/X9892e05.htm#P8217_125315

    SUA_food_macronutrient_rate_nofish %>%
      bind_rows(
        SUA_food_macronutrient_rate_nofish %>% distinct(area_code) %>%
          full_join(Fish_item,  by = character()) %>%
          left_join(FAO_an_items_cal_SUA %>%
                      select(item_code, calperg = Mcal_t,fatperc = fat_Perc,
                             proteinperc = protein_Perc), by = "item_code" )
      ) -> SUA_food_macronutrient_rate

    unique(SUA_food_macronutrient_rate$area_code) %>% length()
    unique(SUA_food_macronutrient_rate$item_code) %>% length()
    SUA_food_macronutrient_rate %>% distinct(item, item_code) -> SUA_COMM_FOOD_NUTRIENT
    # 76244 =179 area * (414 items + 12 fish items)
    # remove processing data

    rm(SUA_food_macronutrient, SUA_food_yearmean,
       SUA_food_yearmean_fill, SUA_food_yearareamean,
       OA, POP, SCL, FBS, MAPPING_FAO_FBS_SUA,
       SUA_food_macronutrient_rate_nofish, FAO_an_items_cal_SUA)
    rm(Fish_item)
    rm(checkarea, checkitem, checkelement)



    SUA_food_macronutrient_rate %>%
      add_title("FAO food calories and macronutrient rate") %>%
      add_units("rates") %>%
      add_comments("Detailed FAO food calories and macrotunitent info for 414 SUA items + 12 fish items") %>%
      add_precursors("SCL_wide",
                     "FBS_wide",
                     "OA",
                     "aglu/FAO/FAO_an_items_cal_SUA",
                     "aglu/FAO/MAPPING_FAO_FBS_SUA") ->
      SUA_food_macronutrient_rate

    # P.S. ----
    # China Wheat, bran for food? Need to fix in SUA later; no changes needed here for this

    return_data(MODULE_OUTPUTS)

  } else {
    stop("Unknown command")
  }
}
