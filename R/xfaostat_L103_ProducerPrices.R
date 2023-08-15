# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_xfaostat_L103_ProducerPrices
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
#' first case_when vars
#' @importFrom tibble tibble
#' @importFrom tidyr complete drop_na gather nesting spread replace_na fill
#' @author XZ 2023
module_xfaostat_L103_ProducerPrices <- function(command, ...) {

  MODULE_INPUTS <-
    c(FILE = "aglu/FAO/FAO_an_items_PRODSTAT",
      FILE = "aglu/FAO/FAO_ag_items_PRODSTAT",
      "QCL_PROD",
      "PP_wide")

  MODULE_OUTPUTS <-
    c("QCL_PRIMARY_PROD_PV")

  if(command == driver.DECLARE_INPUTS) {
    return(MODULE_INPUTS)
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(MODULE_OUTPUTS)
  } else if(command == driver.MAKE) {

    year <- value <- Year <- Value <- FAO_country <- iso <- NULL    # silence package check.

    all_data <- list(...)[[1]]

    # Load required inputs ----

    get_data_list(all_data, MODULE_INPUTS, strip_attributes = TRUE)


    PP_wide %>% gather_years() %>%
      FAOSTAT_AREA_RM_NONEXIST() ->
      PP


    FAO_an_items_PRODSTAT <- FAO_an_items_PRODSTAT %>% filter(!is.na(GCAM_commodity))
    FAO_ag_items_PRODSTAT <- FAO_ag_items_PRODSTAT %>% filter(!is.na(GCAM_commodity))

    # Primary crops
    QCL_PROD %>% filter(item_set == "QCL_COMM_CROP_PRIMARY") %>%
      distinct(item, item_code) -> QCL_COMM_CROP_PRIMARY
    # Primary animal products, including fat hides etc. 45
    QCL_PROD %>% filter(grepl("AN_PRIMARY", item_set) ) %>%
      distinct(item, item_code) -> QCL_COMM_AN_PRIMARY


    QCL_COMM_CROP_PRIMARY %>%
      bind_rows(QCL_COMM_AN_PRIMARY) %>%
      left_join(QCL_PROD, by = c("item_code", "item")) ->
      QCL_PRIMARY

    QCL_PRIMARY %>% distinct(item, element, item_set, unit)

    #FF_check_count_plot(QCL_PRIMARY) # 205 items
    #FF_check_count_plot(PP) # PP %>% distinct(area)

    # check items
    FF_join_checkmap(c("QCL_PRIMARY", "PP"), "item_code", "item") -> A
    A %>% filter(!is.na(QCL_PRIMARY_item)) %>% filter(is.na(PP_item)) -> PP_NO_DATA_ITEM
    # 20 items out of 205 were missing; mostly animal offals, fats or skins

    ## QCL_PRIMARY and PP ----
    #*******************************************

    QCL_PRIMARY %>%
      distinct(element, unit) -> UnitMap

    QCL_PRIMARY %>%
      # remove no PP data items; value would be otherwise replaced by zero
      filter(!item_code %in% c(PP_NO_DATA_ITEM %>% pull(item_code))) %>%
      # keep only PP area
      filter(area_code %in% unique(PP$area_code)) %>%
      # Keep only PP years
      filter(year %in% unique(PP$year)) %>%
      select(-unit, -element_code, - item_set) %>%
      spread(element, value) %>%
      left_join(PP %>% mutate(element = "PP") %>%
                  select(-element_code) %>%
                  spread(element, value),
                by = c("item_code", "item", "area_code", "area", "year")) %>%
      mutate(Prod_Value = PP * Production) ->
      QV

    # fill in missing based on regional price index ratio to world average
    QV %>% filter(year >= 2010) %>%
      filter(Prod_Value > 0) %>%
      group_by(area_code) %>%
      summarise(PP_index = sum(Prod_Value) / sum(Production)) %>%
      ungroup() %>%
      left_join(QV %>% filter(year >= 2010) %>%
                  filter(Prod_Value > 0) %>%
                  summarise(World_PP_index = sum(Prod_Value) / sum(Production)) %>%
                  ungroup(), by = character()) %>%
      mutate(PP_Multiplier = PP_index / World_PP_index) %>%
      select(area_code, PP_Multiplier) ->
      PP_Multiplier_REG_WORLD

    QV %>% filter(Prod_Value > 0) %>%
      group_by(item_code, year) %>%
      summarise(PP_item_world = sum(Prod_Value) / sum(Production)) %>%
      ungroup() -> PP_item_WORLD

    QV %>% filter(Production > 0) %>%
      left_join(PP_item_WORLD, by = c("item_code", "year")) %>%
      left_join(PP_Multiplier_REG_WORLD, by = "area_code") %>%
      mutate(Prod_Value = ifelse(is.na(Prod_Value),
                                 PP_item_world * PP_Multiplier * Production, Prod_Value)) %>%
      select(-PP, -PP_item_world, -PP_Multiplier) ->
      QV1

    # Further fill in missing based relationship and world average prices
    QV1 %>%
      FF_FILL_NUMERATOR_DENOMINATOR(NUMERATOR_c = "Prod_Value",
                                    DENOMINATOR_c = "Production") %>%
      # Remove area x year that should no exist
      FAOSTAT_AREA_RM_NONEXIST %>%
      left_join(UnitMap %>%
                  bind_rows(UnitMap %>% mutate(element = "Prod_Value", unit = "USD")),
                by = "element") ->
      QCL_PRIMARY_PROD_PV

    rm(QV1, QV)

    QCL_PRIMARY_PROD_PV %>%
      add_title("FAO crop and livestock production and crop area") %>%
      add_units("USD and tonne") %>%
      add_comments("Detailed FAO QCL data processing. FBS fish data is used") %>%
      add_precursors("aglu/FAO/FAO_an_items_PRODSTAT",
                     "aglu/FAO/FAO_ag_items_PRODSTAT",
                     "QCL_PROD",
                     "PP_wide") ->
      QCL_PRIMARY_PROD_PV

    return_data(MODULE_OUTPUTS)

  } else {
    stop("Unknown command")
  }
}
