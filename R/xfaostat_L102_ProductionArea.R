# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_xfaostat_L102_ProductionArea
#'
#' Preprocess raw faostat data
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
module_xfaostat_L102_ProductionArea <- function(command, ...) {

  MODULE_INPUTS <-
    c(FILE = "aglu/FAO/FAO_an_items_PRODSTAT",
      FILE = "aglu/FAO/FAO_ag_items_PRODSTAT",
        "QCL", "FBS", "FBSH_CB")

  MODULE_OUTPUTS <-
    c("QCL_PROD",
      "QCL_AN_LIVEANIMAL",
      "QCL_AN_PRIMARY_MILK",
      "QCL_AN_LIVEANIMAL_MEATEQ",
      "QCL_CROP_PRIMARY")

  if(command == driver.DECLARE_INPUTS) {
    return(MODULE_INPUTS)
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(MODULE_OUTPUTS)
  } else if(command == driver.MAKE) {

    year <- value <- Year <- Value <- FAO_country <- iso <- NULL    # silence package check.

    all_data <- list(...)[[1]]

    # Load required inputs ----

    get_data_list(all_data, MODULE_INPUTS, strip_attributes = TRUE)

    FAO_an_items_PRODSTAT <- FAO_an_items_PRODSTAT %>% filter(!is.na(GCAM_commodity))
    FAO_ag_items_PRODSTAT <- FAO_ag_items_PRODSTAT %>% filter(!is.na(GCAM_commodity))


    # All Ag and An production data, including livestock animal, and area harvested, are included in QCL.
    # FBSH and FBS are added to get fish production data

    # quick plot
    # QCL %>% FF_check_count_plot(c("Production", "Area harvested", "Stocks", "Milk Animals", "Laying"))

    # Check elements
    QCL %>% distinct(element, element_code, unit)
    QCL %>% distinct(item, item_code) # 276 = 160 primary crop + 45 primary an + 17 live animal + 54 others

    # QCL will be grouped by elements
    # Pull primary crop items with positive harvested area 160
    QCL %>% filter(element_code == 5312) %>% filter(value >0) %>%
      distinct(item_code, item) -> QCL_COMM_CROP_PRIMARY

    # Primary animal products, including fat hides etc. 45
    QCL %>% filter(element_code %in% c(5410, 5413, 5420, 5417, 5422, 5424, 5320)) %>%
      distinct(item, item_code) -> QCL_COMM_AN_PRIMARY

    # Stock element, returns 17 live animals, not including Milk Animals and Laying animals
    QCL %>% filter(element_code %in% c(5111, 5112, 5114)) %>%
      distinct(item, item_code) -> QCL_COMM_AN_LIVEANIMAL
    # Note that live animals will be converted to meat equivalent QCL_AN_LIVEANIMAL_EQ

    # The rest including snails, oil, cheese etc.
    QCL %>% filter(!item_code %in% c(QCL_COMM_AN_LIVEANIMAL %>% pull(item_code))) %>%
      filter(!item_code %in% c(QCL_COMM_AN_PRIMARY %>% pull(item_code))) %>%
      filter(!item_code %in% c(QCL_COMM_CROP_PRIMARY %>% pull(item_code))) %>%
      distinct(item, item_code) -> QCL_COMM_OTHERPROC


    ## QCL_COMM_CROP_PRIMARY ----
    #*******************************************
    # Calculate yield and fill in yield; Interpolate production; Calculate area when production exist
    # Fill in yield with world average where needed

    QCL %>%
      filter(item_code %in% c(QCL_COMM_CROP_PRIMARY %>% pull(item_code))) %>%
      distinct(element,element_code, unit) -> UnitMap

    QCL %>%
      filter(item_code %in% c(QCL_COMM_CROP_PRIMARY %>% pull(item_code))) %>%  #160 primary items
      filter(element_code != 5419) %>%
      # complete all
      complete(nesting(area_code, area), nesting(item_code, item), nesting(element_code, element, unit), year) %>%
      select(-unit, -element_code) %>%
      spread(element, value) %>%
      # Fill in missing
      FF_FILL_NUMERATOR_DENOMINATOR(NUMERATOR_c = "Production",
                                    DENOMINATOR_c = "Area harvested") %>%
      # Remove area x year that should no exist
      FAO_AREA_RM_NONEXIST %>%
      left_join(UnitMap, by = "element") ->
      QCL_CROP_PRIMARY

    #QCL_CROP_PRIMARY %>% FF_check_count_plot()
    QCL_CROP_PRIMARY %>% nrow()/(160); QCL_CROP_PRIMARY %>% distinct(year); QCL_CROP_PRIMARY %>% distinct(area_code)

    ## QCL_COMM_AN_PRIMARY ----
    #*******************************************
    # 45 items 19 meat + 2 egg + 5 milk + (2+9) bee & hide + 13 fat & offal
    QCL %>%
      filter(item_code %in% c(QCL_COMM_AN_PRIMARY %>% pull(item_code))) %>%
      distinct(element_code, element, unit)

    QCL %>% filter(item_code %in% c(QCL_COMM_AN_PRIMARY %>% pull(item_code))) %>%
      filter(element_code == 5417) %>% distinct(item, item_code) -> QCL_COMM_AN_PRIMARY_MEAT1

    QCL %>% filter(item_code %in% c(QCL_COMM_AN_PRIMARY %>% pull(item_code))) %>%
      filter(element_code == 5424) %>% distinct(item, item_code) -> QCL_COMM_AN_PRIMARY_MEAT2

    QCL %>% filter(item_code %in% c(QCL_COMM_AN_PRIMARY %>% pull(item_code))) %>%
      filter(element_code == 5313) %>% distinct(item, item_code) -> QCL_COMM_AN_PRIMARY_EGG

    QCL %>% filter(item_code %in% c(QCL_COMM_AN_PRIMARY %>% pull(item_code))) %>%
      filter(element_code == 5318) %>% distinct(item, item_code) -> QCL_COMM_AN_PRIMARY_MILK

    QCL %>% filter(item_code %in% c(QCL_COMM_AN_PRIMARY %>% pull(item_code))) %>%
      filter(element_code == c(5422)) %>% distinct(item, item_code) -> QCL_COMM_AN_PRIMARY_BEE

    QCL %>% filter(item_code %in% c(QCL_COMM_AN_PRIMARY %>% pull(item_code))) %>%
      filter(!item_code %in% c(QCL_COMM_AN_PRIMARY_MEAT1 %>% pull(item_code))) %>%
      filter(!item_code %in% c(QCL_COMM_AN_PRIMARY_MEAT2 %>% pull(item_code))) %>%
      filter(!item_code %in% c(QCL_COMM_AN_PRIMARY_EGG %>% pull(item_code))) %>%
      filter(!item_code %in% c(QCL_COMM_AN_PRIMARY_MILK %>% pull(item_code))) %>%
      filter(!item_code %in% c(QCL_COMM_AN_PRIMARY_BEE %>% pull(item_code))) %>%
      distinct(item, item_code) -> QCL_COMM_AN_PRIMARY_FATOFFALHIDE

    ### QCL_COMM_AN_PRIMARY_MEAT1 ----

    QCL %>%
      filter(item_code %in% c(QCL_COMM_AN_PRIMARY_MEAT1 %>% pull(item_code))) %>%
      distinct(element,element_code, unit) -> UnitMap

    QCL %>%
      filter(item_code %in% c(QCL_COMM_AN_PRIMARY_MEAT1 %>% pull(item_code))) %>%
      filter(element_code != 5417) %>%
      # complete year
      complete(nesting(area_code, area, item_code, item), nesting(element_code, element, unit), year) %>%
      select(-unit, -element_code) %>%
      spread(element, value) %>%
      # Fill in missing
      FF_FILL_NUMERATOR_DENOMINATOR(NUMERATOR_c = "Production",
                                    DENOMINATOR_c = "Producing Animals/Slaughtered") %>%
      # Remove area x year that should no exist
      FAO_AREA_RM_NONEXIST %>%
      left_join(UnitMap, by = "element") ->
      QCL_AN_PRIMARY_MEAT1

    ### QCL_COMM_AN_PRIMARY_MEAT2 ----
    QCL %>%
      filter(item_code %in% c(QCL_COMM_AN_PRIMARY_MEAT2 %>% pull(item_code))) %>%
      distinct(element,element_code, unit) -> UnitMap

    QCL %>%
      filter(item_code %in% c(QCL_COMM_AN_PRIMARY_MEAT2 %>% pull(item_code))) %>%
      filter(element_code != 5424) %>%
      # complete year
      complete(nesting(area_code, area, item_code, item), nesting(element_code, element, unit), year) %>%
      select(-unit, -element_code) %>%
      spread(element, value) %>%
      # Fill in missing
      FF_FILL_NUMERATOR_DENOMINATOR(NUMERATOR_c = "Production",
                                    DENOMINATOR_c = "Producing Animals/Slaughtered") %>%
      # Remove area x year that should no exist
      FAO_AREA_RM_NONEXIST %>%
      left_join(UnitMap, by = "element") ->
      QCL_AN_PRIMARY_MEAT2

    ### QCL_COMM_AN_PRIMARY_EGG ----

    QCL %>%
      filter(item_code %in% c(QCL_COMM_AN_PRIMARY_EGG %>% pull(item_code))) %>%
      distinct(element,element_code, unit) -> UnitMap

    QCL %>%
      filter(item_code %in% c(QCL_COMM_AN_PRIMARY_EGG %>% pull(item_code))) %>%
      filter(element_code %in% c(5510, 5313)) %>%
      complete(nesting(area_code, area, item_code, item), nesting(element_code, element, unit), year) %>%
      select(-unit, -element_code) %>%
      spread(element, value) %>%
      # Fill in missing
      FF_FILL_NUMERATOR_DENOMINATOR(NUMERATOR_c = "Production",
                                    DENOMINATOR_c = "Laying") %>%
      # Remove area x year that should no exist
      FAO_AREA_RM_NONEXIST %>%
      left_join(UnitMap %>%
                  filter(element_code %in% c(5510, 5313)), by = "element") ->
      QCL_AN_PRIMARY_EGG

    ### QCL_COMM_AN_PRIMARY_MILK ----

    QCL %>%
      filter(item_code %in% c(QCL_COMM_AN_PRIMARY_MILK %>% pull(item_code))) %>%
      distinct(element,element_code, unit) -> UnitMap

    QCL %>%
      filter(item_code %in% c(QCL_COMM_AN_PRIMARY_MILK %>% pull(item_code))) %>%
      filter(element_code != 5420) %>%
      # complete year
      complete(nesting(area_code, area, item_code, item), nesting(element_code, element, unit), year) %>%
      select(-unit, -element_code) %>%
      spread(element, value) %>%
      # Fill in missing
      FF_FILL_NUMERATOR_DENOMINATOR(NUMERATOR_c = "Production",
                                    DENOMINATOR_c = "Milk Animals") %>%
      # Remove area x year that should no exist
      FAO_AREA_RM_NONEXIST %>%
      left_join(UnitMap, by = "element") ->
      QCL_AN_PRIMARY_MILK

    ### QCL_COMM_AN_PRIMARY_BEE ----
    QCL %>%
      filter(item_code %in% c(QCL_COMM_AN_PRIMARY_BEE %>% pull(item_code))) %>%
      distinct(element,element_code, unit) -> UnitMap

    QCL %>%
      filter(item_code %in% c(QCL_COMM_AN_PRIMARY_BEE %>% pull(item_code))) %>%
      filter(element_code == 5510) %>%
      # complete year
      complete(nesting(area_code, area, item_code, item), nesting(element_code, element, unit), year) %>%
      group_by(area_code, area, item_code, item, element_code, element, unit) %>%
      # linearly interpolate value forward
      mutate(value = gcamdata::approx_fun(year, value)) %>%
      # fill down only to fill the last year
      tidyr::fill(value, .direction = "down") %>%
      replace_na(list(value = 0)) %>%
      ungroup() %>%
      FAO_AREA_RM_NONEXIST ->
      QCL_AN_PRIMARY_BEE

    ### QCL_COMM_AN_PRIMARY_FATOFFALHIDE ----
    # No adjustment made for these

    QCL %>%
      filter(item_code %in% c(QCL_COMM_AN_PRIMARY_FATOFFALHIDE %>% pull(item_code))) %>%
      distinct(element,element_code, unit) -> UnitMap

    QCL %>%
      filter(item_code %in% c(QCL_COMM_AN_PRIMARY_FATOFFALHIDE %>% pull(item_code))) %>%
      filter(element_code == 5510) %>%
      # complete year
      complete(nesting(area_code, area, item_code, item, element_code, element, unit), year) %>%
      group_by(area_code, area, item_code, item, element_code, element, unit) %>%
      # linearly interpolate value forward
      mutate(value = gcamdata::approx_fun(year, value)) %>%
      # fill down only to fill the last year
      tidyr::fill(value, .direction = "down") %>%
      replace_na(list(value = 0)) %>%
      ungroup() %>%
      FAO_AREA_RM_NONEXIST ->
      QCL_AN_PRIMARY_FATOFFALHIDE


    ## QCL_COMM_OTHERPROC ----
    #*******************************************
    QCL %>%
      filter(item_code %in% c(QCL_COMM_OTHERPROC %>% pull(item_code))) %>%
      distinct(element,element_code, unit) -> UnitMap # only production

    QCL %>%
      filter(item_code %in% c(QCL_COMM_OTHERPROC %>% pull(item_code))) %>%
      complete(nesting(area_code, area, item_code, item, element_code, element, unit), year) %>%
      group_by(area_code, area, item_code, item) %>%
      # linearly interpolate production and fill in yield down-up
      mutate(value = gcamdata::approx_fun(year, value)) %>%
      tidyr::fill(value, .direction = "down") %>%
      replace_na(list(value = 0)) %>%
      ungroup() %>%
      FAO_AREA_RM_NONEXIST -> QCL_OTHERPROC

    ## QCL_COMM_AN_LIVEANIMAL ----
    #*******************************************
    # There are 17 live animals and 5 dairy/milk animals (included below)

    QCL %>%
      filter(item_code %in% c(QCL_COMM_AN_LIVEANIMAL %>% pull(item_code))) %>%
      distinct(element_code, element, unit) -> UnitMap

    QCL %>% filter(item_code %in% c(QCL_COMM_AN_LIVEANIMAL %>% pull(item_code))) %>%
      # complete year
      complete(nesting(area_code, area, item_code, item, element_code, element, unit), year) %>%
      group_by(area_code, area, item_code, item, element_code, element, unit) %>%
      # linearly interpolate value forward
      mutate(value = gcamdata::approx_fun(year, value)) %>%
      tidyr::fill(value, .direction = "down") %>%
      replace_na(list(value = 0)) %>%
      ungroup() %>%
      FAO_AREA_RM_NONEXIST -> QCL_AN_LIVEANIMAL


    ## QCL_AN_LIVEANIMAL_MEATEQ ----
    #*******************************************

    QCL_AN_LIVEANIMAL %>%
      distinct(element_code, element, unit)
    QCL_AN_PRIMARY_MEAT1 %>%
      distinct(element_code, element, unit)
    QCL_AN_PRIMARY_MEAT2 %>%
      distinct(element_code, element, unit)

    # live animal item_code + 1 matches primary meat code
    QCL_COMM_AN_LIVEANIMAL %>% mutate(item_code = item_code + 1) %>%
      right_join(QCL_COMM_AN_PRIMARY_MEAT1 %>%
                   bind_rows(QCL_COMM_AN_PRIMARY_MEAT2) %>%
                   rename(item_meat = item), by = "item_code" )

    QCL_AN_PRIMARY_MEAT1 %>% bind_rows(
      QCL_AN_PRIMARY_MEAT2) %>%
      select(-element_code, -unit) %>%
      spread(element, value) %>%
      mutate(Yield = Production / `Producing Animals/Slaughtered`) %>%
      left_join(QCL_AN_LIVEANIMAL %>%
                  select(area_code, area, item_code, year, value) %>%
                  mutate(item_code = item_code+1),
                by = c("area_code", "area", "item_code", "year")) %>%
      dplyr::transmute(area_code, area, item_code, item,
                element = "Stock in Meat Eq",
                year,
                value = value * Yield / 1000,
                unit = "1000 tonnes") %>%
      filter(!is.na(value)) -> QCL_AN_LIVEANIMAL_MEATEQ

    # FAO definition:
    # The enumeration to be chosen, when more than one survey is taken,
    # is the closest to the beginning of the calendar year.


    ## FBS Fish products ----
    #*******************************************

    FBS %>% distinct(element, element_code, unit)

    # Get fish items through mapping

    checkitem <-
      FF_join_checkmap(c("QCL_COMM_AN_PRIMARY", "FAO_an_items_PRODSTAT"), "item_code", "item") %>%
      mutate(match = if_else(QCL_COMM_AN_PRIMARY_item == FAO_an_items_PRODSTAT_item, T, F))


    checkitem %>% filter(is.na(match)|match == F)
    FBS_COMM_FISH <- checkitem %>% filter(is.na(QCL_COMM_AN_PRIMARY_item)) %>%
      dplyr::transmute(item_code, item = FAO_an_items_PRODSTAT_item) %>%
      filter(item_code %in% c(2761 : 2782))
    # 1176 snails were in other proc

    FBS_FISH <-
      FBS %>% filter(year >= 2010, element_code == 5511, # production
                     item_code %in% c(FBS_COMM_FISH %>% pull(item_code))) %>%
      bind_rows(
        FBSH_CB %>% filter(year < 2010, element_code == 5511, # production
                        item_code %in% c(FBS_COMM_FISH %>% pull(item_code)))
      ) %>% mutate(value = value *1000,
                   unit = "tonnes",
                   element_code = 5510 # changed here for consistency
      ) %>%
      select(area_code, area, item_code, item, element, element_code, year, value, unit) %>%
      FAO_AREA_RM_NONEXIST



    # Bind QCL_ALL and save RDS ----
    #*******************************************

    QCL_CROP_PRIMARY %>% mutate(item_set = "QCL_COMM_CROP_PRIMARY") %>%
      bind_rows(QCL_AN_PRIMARY_MEAT1 %>% mutate(item_set = "QCL_COMM_AN_PRIMARY_MEAT1")) %>%
      bind_rows(QCL_AN_PRIMARY_MEAT2 %>% mutate(item_set = "QCL_COMM_AN_PRIMARY_MEAT2")) %>%
      bind_rows(QCL_AN_PRIMARY_EGG %>% mutate(item_set = "QCL_COMM_AN_PRIMARY_EGG")) %>%
      bind_rows(QCL_AN_PRIMARY_MILK %>% mutate(item_set = "QCL_COMM_AN_PRIMARY_MILK")) %>%
      bind_rows(QCL_AN_PRIMARY_BEE %>% mutate(item_set = "QCL_COMM_AN_PRIMARY_BEE")) %>%
      bind_rows(QCL_AN_PRIMARY_FATOFFALHIDE %>% mutate(item_set = "QCL_COMM_AN_PRIMARY_FATOFFALHIDE")) %>%
      bind_rows(QCL_OTHERPROC %>% mutate(item_set = "QCL_COMM_OTHERPROC")) %>%
      bind_rows(QCL_AN_LIVEANIMAL %>% mutate(item_set = "QCL_COMM_AN_LIVEANIMAL")) %>%
      bind_rows(QCL_AN_LIVEANIMAL_MEATEQ %>% mutate(item_set = "QCL_COMM_AN_LIVEANIMAL_MEATEQ")) %>%
      bind_rows(FBS_FISH %>% mutate(item_set = "FBS_COMM_FISH"))->
      QCL_ALL


    QCL_AN_LIVEANIMAL %>%
      add_title("FAO live animal stock and production") %>%
      add_units("various") %>%
      add_comments("Detailed FAO QCL data processing for live animal and production") ->
      QCL_AN_LIVEANIMAL

    QCL_AN_PRIMARY_MILK %>%
      add_title("FAO milk animal stock and production") %>%
      add_units("various") %>%
      add_comments("Detailed FAO QCL data processing for dairy animal and production") ->
      QCL_AN_PRIMARY_MILK

    QCL_AN_LIVEANIMAL_MEATEQ %>%
      add_title("FAO live animal stock meat equivalent") %>%
      add_units("various") %>%
      add_comments("Detailed FAO QCL data processing for live animal stock meat equivalent") ->
      QCL_AN_LIVEANIMAL_MEATEQ

    QCL_CROP_PRIMARY %>%
      add_title("FAO primary crop area and production") %>%
      add_units("various") %>%
      add_comments("Detailed FAO QCL data processing for crop area and production") ->
      QCL_CROP_PRIMARY


    # Production only
    QCL_ALL %>% filter(element_code == 5510) ->
      QCL_PROD

    QCL_PROD %>%
      add_title("FAO primary production") %>%
      add_units("tonnes") %>%
      add_comments("FAO primary production") ->
      QCL_PROD

    # No NA
    assertthat::assert_that(QCL_ALL %>% filter(is.na(value)) %>% nrow() == 0)

    #QCL_ALL %>% FF_check_count_plot -> p; p
    # ggsave(file.path(DIR_DATAPROC_PLOT, "QCL_ALL.png"),
    #        plot = p + ggtitle("gcamdata-FAOSTAT (QCL & FBS) production data over time"),
    #        dpi = 200, width = 9, height = 5 )
    # rm(p)

    QCL_ALL %>% distinct(year); # 60 years
    QCL_ALL %>% distinct(element, element_code, unit) # QCL_COMM_AN_LIVEANIMAL_MEATEQ has no element_code
    QCL_ALL %>% distinct(item)  # 160 primary crop + 45 primary an + 54 others + 17 +12


    # QCL_ALL %>%
    #   add_title("FAO crop and livestock production and crop area") %>%
    #   add_units("various") %>%
    #   add_comments("Detailed FAO QCL data processing. FBS fish data is used") ->
    #   QCL_ALL



    # P.S.  Check primary product mapping ----

    # checkitem <-
    #   FF_join_checkmap(c("QCL_COMM_CROP_PRIMARY", "FAO_ag_items_PRODSTAT"), "item_code", "item") %>%
    #   mutate(match = if_else(QCL_COMM_CROP_PRIMARY_item == FAO_ag_items_PRODSTAT_item , T, F))
    # checkitem %>% filter(is.na(match)|match == F)
    # # 160 primary items (matching here) + 15/16 fodder crops
    #
    # checkitem <-
    #   FF_join_checkmap(c("QCL_COMM_AN_PRIMARY", "FAO_an_items_PRODSTAT"), "item_code", "item") %>%
    #   mutate(match = if_else(QCL_COMM_AN_PRIMARY_item == FAO_an_items_PRODSTAT_item, T, F))
    # checkitem %>% filter(is.na(match)|match == F)
    # # Snails, not sea production came from SCL
    # # 12 fish items will be provided by FBS



    return_data(MODULE_OUTPUTS)

  } else {
    stop("Unknown command")
  }
}
