# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_xfaostat_L104_ProductionAreaAddFodder
#'
#' Process fodder crop data
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
module_xfaostat_L104_ProductionAreaAddFodder <- function(command, ...) {

  MODULE_INPUTS <-
    c(FILE = "aglu/FAO/FAOSTAT/Other_supplementary/FAO_fodder_Prod_t_HA_ha_PRODSTAT_2011",
      "QCL_area_code_map")

  MODULE_OUTPUTS <-
    c("QCL_FODDERCROP")

  if(command == driver.DECLARE_INPUTS) {
    return(MODULE_INPUTS)
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(MODULE_OUTPUTS)
  } else if(command == driver.MAKE) {

    year <- value <- Year <- Value <- FAO_country <- iso <- NULL    # silence package check.
    QCL_area_code_map <- element_code <- element <- area_code <- item_code <- area <-
      item <- unit <- QCL_FODDERCROP_item <- NULL

    all_data <- list(...)[[1]]

    # Load required inputs ----

    get_data_list(all_data, MODULE_INPUTS, strip_attributes = TRUE)

    QCL_area_code <- QCL_area_code_map %>% distinct(area_code) %>% pull()

    # Fodder data are no longer updated by FAOSTAT (latest year = 2011)

    # Though Xu et al. was using fodder data from GCAM (old FAOSTAT)
    # Xu, Xiaoming, et al. "Global greenhouse gas emissions from animal-based foods
    # are twice those of plant-based foods." Nature Food 2.9 (2021): 724-732.
    # Some Alfalfa into
    # https://fyi.extension.wisc.edu/forage/alfalfa-yield-and-stand/


    # "Tea Nes" will not be included; very small
    # Popcorn will also be removed as most zeros. Only USA before 1981 available
    # For some reason, pumpkins for fodder is not included in the mapping; E.g., 80 Mha!!!
    # Need to check data again carefully
    # inconsistency compared with USDA
    # yields are too high; wet ton vs. dry ton;
    # USA Alfalfa prod was divided by 4 later to match USDA...same concerns on others
    # Vetches is already in the new data under other categories

    FAO_fodder_Prod_t_HA_ha_PRODSTAT_2011 %>%
      filter(year %in% FAOSTAT_Hist_Year) %>%
      filter(area_code %in% QCL_area_code) %>%
      FAOSTAT_AREA_RM_NONEXIST() ->
      FAO_fodder_Prod_t_HA_ha_PRODSTAT_2011

    # Low data quality compared to USDA briefly (partial region coverage, e.g., no Brazil or Africa)
    # https://www.progressivepublish.com/downloads/2021/general/2020-pf-stats-highres.pdf
    # Not changing anything here for now.


    FAO_fodder_Prod_t_HA_ha_PRODSTAT_2011 %>%  distinct(element, element_code, unit) -> UnitMap

    FAO_fodder_Prod_t_HA_ha_PRODSTAT_2011 %>%
      # include adjustments made in gcamdata here fro alfalfa production
      # area == "United States of America" & item == "Alfalfa for forage and silage"
      mutate(value = if_else(area_code == 231 & element == "Production"& item_code == 641,
                             value / 4, value)) %>%
      # complete all
      complete(nesting(area_code, area), nesting(item_code, item),
               nesting(element_code, element, unit), year = FAOSTAT_Hist_Year) %>%
      select(-unit, -element_code) %>%
      spread(element, value) %>%
      # Fill in missing
      FF_FILL_NUMERATOR_DENOMINATOR(NUMERATOR_c = "Production",
                                    DENOMINATOR_c = "Area harvested") %>%
      mutate(area_code = as.numeric(area_code)) %>%
      # 206  Sudan (former) filled in after dissolution
      # but desolved regions were missing.
      # so replaced with 276 Sudan after 2012
      mutate(area_code = if_else(area_code == 206 & year >= 2012, 276, area_code),
             area = if_else(area == "Sudan (former)" & year >= 2012, "Sudan", area)) %>%
      # Remove area x year that should no exist
      FAOSTAT_AREA_RM_NONEXIST %>%
      left_join(UnitMap, by = "element") ->
      QCL_FODDERCROP

    rm(FAO_fodder_Prod_t_HA_ha_PRODSTAT_2011, UnitMap)

    QCL_FODDERCROP %>%
      add_title("Processed fodder crop production and area") %>%
      add_units("tonne and ha") %>%
      add_comments("Data is from old GCAM data v5.4") %>%
      add_precursors("aglu/FAO/FAOSTAT/Other_supplementary/FAO_fodder_Prod_t_HA_ha_PRODSTAT_2011",
                     "QCL_area_code_map")->
      QCL_FODDERCROP


    # FAO_ag_items_PRODSTAT is not read in anymore
    # comment out these GCAM checks
    # Curr_Envir <- environment()
    # # 160 primary items +16 fodder crops
    # FF_join_checkmap(DFs = c("QCL_FODDERCROP", "FAO_ag_items_PRODSTAT"),
    #                  COL_by = "item_code",
    #                  COL_rename = "item", .ENVIR = Curr_Envir) %>%
    #   mutate(M = if_else(QCL_FODDERCROP_item == FAO_ag_items_PRODSTAT_item , T, F))-> checkitem
    # checkitem %>% filter(M == T)
    # # 15 fodder crops + 1 pumpkin removed in mapping
    # rm(FAO_ag_items_PRODSTAT, checkitem)


    return_data(MODULE_OUTPUTS)

  } else {
    stop("Unknown command")
  }
}
