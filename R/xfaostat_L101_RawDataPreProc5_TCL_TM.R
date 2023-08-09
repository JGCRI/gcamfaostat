# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_xfaostat_L101_RawDataPreProc5_TCL_TM
#'
#' Preprocess raw faostat data part 5 Trade
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
#' @author XZ 2023
module_xfaostat_L101_RawDataPreProc5_TCL_TM <- function(command, ...) {

  MODULE_INPUTS <-
    c("QCL_area_code_map")

  MODULE_OUTPUTS <-
    c("TCL",       # Gross trade
      "TM_bilateral")   # Bilateral trade


  if(command == driver.DECLARE_INPUTS) {
    return(MODULE_INPUTS)
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(MODULE_OUTPUTS)
  } else if(command == driver.MAKE) {

    year <- value <- Year <- Value <- FAO_country <- iso <- NULL    # silence package check.

    all_data <- list(...)[[1]]

    # Load required inputs ----

    get_data_list(all_data, MODULE_INPUTS, strip_attributes = TRUE)



    FAOSTAT_RDS <- c("TCL", "TM_bilateral")

    DIR_PREBUILT_FAOSTAT <- "data/PREBUILT_FAOSTAT"

    lapply(FAOSTAT_RDS, function(d){
      assertthat::assert_that(file.exists(file.path(DIR_PREBUILT_FAOSTAT, paste0(d, ".rds"))))
      assign(d, readRDS(file.path(DIR_PREBUILT_FAOSTAT, paste0(d, ".rds"))),
             envir = parent.env(environment()))
    })

    ### output TCL and clean memory ----
    TCL %>%
      add_title("FAO TCL") %>%
      add_units("tonne") %>%
      add_comments("Preprocessed FAO TCL") ->
      TCL

    TM_bilateral %>%
      add_title("FAO TM") %>%
      add_units("tonne") %>%
      add_comments("Preprocessed FAO TM_wide") ->
      TM_bilateral



#
#     QCL_area_code <- QCL_area_code_map %>% distinct(area_code) %>% pull()
#
#     # *[TCL] Gross trade ----
#     FAOSTAT_load_raw_data("TCL")   # Gross trade
#
#     TCL %>% distinct(element, element_code, unit)
#     TCL %>% distinct(item, item_code)
#
#     TCL %>%
#       filter(item_code <= 1700,
#              # only keep quantity
#              !element_code %in% c(5622 , 5922),
#              area_code %in% QCL_area_code) %>%
#       select(area_code, area, item_code, item, element_code, element, year, value, unit) %>%
#       rm_accent("item", "area") -> TCL1
#
#     ### output TCL and clean memory ----
#     TCL1 %>%
#       add_title("FAO TCL") %>%
#       add_units("tonne") %>%
#       add_comments("Preprocessed FAO TCL") ->
#       TCL
#
#     rm(TCL1)
#
#
#
#     # *[TM] Bilateral trade ----
#     #*FAO has better quality bilateral data since 1992, covering most SUA items
#     FAOSTAT_load_raw_data("TM")    # Bilateral trade
#
#     TM %>%
#       # Only keep quantities for elements with a unit of tonnes
#       filter(element_code %in% c(5910, 5610),
#              item_code < 1700,
#              # Bilateral trade year starts from 1986 but higher quality after 1992
#              # Subset data also to shrink the size
#              year >= min(FAOSTAT__Year_Bilateral),
#              partner_country_code %in% QCL_area_code,
#              reporter_country_code %in% QCL_area_code) %>%
#       select(reporter_country_code, reporter_countries,
#              partner_country_code, partner_countries,
#              item_code, item, element_code, element, year, value, unit)  ->
#       TM1
#     rm(TM)
#
#
#     ## **Reconcile export and import bilateral flow ----
#     # Full join export and import and use available import to fill missing and zero export
#     TM1 %>% filter(element %in% c("Export Quantity")) %>% spread(element, value) %>%
#       select(exporter = reporter_country_code,
#              importer = partner_country_code, item_code, year, expflow = `Export Quantity`) %>%
#       full_join(
#         TM1 %>% filter(element %in% c("Import Quantity")) %>% spread(element, value)%>%
#           select(importer = reporter_country_code,
#                  exporter = partner_country_code, item_code, year, impflow = `Import Quantity`),
#         by = c("exporter", "importer", "item_code", "year")
#       )  %>%
#       # replace na with zero but use import to replace zero export later
#       replace_na(list(expflow = 0, impflow = 0)) %>%
#       transmute(area_code = importer, year, item_code, source_code = exporter,
#                 value = if_else(expflow == 0, impflow, expflow)) %>%
#       mutate(element = "Import Quantity") ->
#       TM2
#
#
#     TM2 %>%
#       # remove self-trade (per unaggregated area_code) which existed in FAO TM importing data and likely due to data processing mistakes.
#       filter(area_code != source_code) %>%
#       left_join(TM1 %>% distinct(item, item_code), by = c("item_code")) %>%
#       left_join(TM1 %>% distinct(area = partner_countries, area_code = partner_country_code), by = c("area_code")) %>%
#       left_join(TM1 %>% distinct(source = partner_countries, source_code = partner_country_code), by = c("source_code")) %>%
#       rm_accent("item", "area", "source") %>%
#       mutate(unit = "tonnes") ->
#       TM3
#     rm(TM1, TM2)
#
#     TM3 %>% mutate(value = value / 1000, unit = "1000 tons") -> TM_bilateral
#
#     ### output TM_bilateral ----
#     TM_bilateral %>%
#       add_title("FAO TM") %>%
#       add_units("tonne") %>%
#       add_comments("Preprocessed FAO TM") ->
#       TM_bilateral



    return_data(MODULE_OUTPUTS)

  } else {
    stop("Unknown command")
  }
}
