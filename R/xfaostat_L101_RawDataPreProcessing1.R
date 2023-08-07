# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_xfaostat_L101_RawDataPreProcessing1
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
#' @importFrom tidyr complete drop_na gather nesting spread replace_na
#' @author XZ 2023
module_xfaostat_L101_RawDataPreProcessing1 <- function(command, ...) {

  MODULE_INPUTS <-
    c(FILE = "aglu/AGLU_ctry")

  MODULE_OUTPUTS <-
    c("QCL",              # Ag production quantity and harvested area
      "PP",               # Producer prices
      "SCL",              # Supply utilization accounting
      "FBS",              # New food balance sheet
      "QCL_area_code_map" # Country code
    )

  if(command == driver.DECLARE_INPUTS) {
    return(MODULE_INPUTS)
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(MODULE_OUTPUTS)
  } else if(command == driver.MAKE) {

    year <- value <- Year <- Value <- FAO_country <- iso <- NULL    # silence package check.

    all_data <- list(...)[[1]]

    # Load required inputs ----

    get_data_list(all_data, MODULE_INPUTS, strip_attributes = TRUE)

    #source("data-raw/generate_package_data_faostat_helper_funcs.R")
    #DIR_RAW_DATA_FAOSTAT <- system.file("extdata", "aglu/FAO/FAOSTAT", package = "gcamdata.faostat")

    # *[QCL] FAOSTAT Production and area ----

    ## Load raw data
    FAOSTAT_load_raw_data(DATASETCODE = "QCL", DATA_FOLDER = DIR_RAW_DATA_FAOSTAT)


    QCL %>%
      # Remove aggregated areas and items
      filter(area_code < 350, item_code < 1700) %>%
      select(area_code, area, item_code, item, element_code, element, year, value, unit) %>%
      # When dealing with animal/livestock data, units are important
      # Prod Popultn (5314) for Beewax and honey is removed since data is only before 1990
      filter(element_code != 5314) %>%
      # Remove NA for simplicity for now; expend.grid later
      # All Coir (coconut fiber) is filtered out due to NA
      filter(!is.na(value)) %>%
      # remove accent
      rm_accent("item", "area") -> QCL1


  QCL1 %>%
    distinct(area_code, area) %>%
      add_title("FAO primary production country and code") %>%
      add_units("NA") %>%
      add_comments("FAO Country and code") ->
      QCL_area_code_map

    # Other data uses OCL area for consistency
    QCL_area_code <- QCL1 %>% distinct(area_code) %>% pull()


    QCL1 %>%
      add_title("FAO primary production") %>%
      add_units("USD/tonne") %>%
      add_comments("Preprocessed FAOSTAT primary production") ->
      QCL



    # *[PP] Producer price ----

    FAOSTAT_load_raw_data(DATASETCODE = "PP", DATA_FOLDER = DIR_RAW_DATA_FAOSTAT)

    PP %>% distinct(element, element_code, unit)


    PP %>%
      filter(area_code < 350,  # rm aggregated regions
             item_code < 1700, #rm aggregated items
             area_code %in% QCL_area_code, # only keep regions with production
             element_code %in% c(5532, 5539)) %>% #keep USD/tonne and index
      rm_accent("item", "area") -> PP1


    # Using index to fill in missing across years
    PP1 %>%
      filter(element_code %in% c(5532, 5539)) %>%
      select(area_code, area, item_code, item, element, year, value) %>%
      # Not completing year and area here
      spread(element, value) %>%
      left_join(
        PP1 %>% filter(element_code %in% c(5532, 5539)) %>%
          select(area_code, area, item_code, item, element, year, value) %>%
          spread(element, value) %>%
          rename(pp_base = `Producer Price (USD/tonne)`,
                 pp_baseindex = `Producer Price Index (2014-2016 = 100)`) %>%
          filter(!is.na(pp_base)) %>%
          group_by(area, area_code, item) %>%
          filter(year == 2015) %>% within(rm(year)) %>%
          ungroup(),
        by = c("area_code", "area", "item_code", "item")
      ) %>% mutate(
        `Producer Price (USD/tonne)` = if_else(is.na(`Producer Price (USD/tonne)`),
                                               pp_base* `Producer Price Index (2014-2016 = 100)` /pp_baseindex,
                                               `Producer Price (USD/tonne)`)
      )  %>%
      select(area_code, area, item_code, item, year, `Producer Price (USD/tonne)`) %>%
      gather(element, value, `Producer Price (USD/tonne)`) %>%
      mutate(element_code = 5532) -> PP2


    ### output PP and clean memory ----
    PP2 %>%
      add_title("FAO producer prices") %>%
      add_units("USD/tonne") %>%
      add_comments("Preprocessed FAOSTAT producer prices") ->
      PP

    rm(PP1, PP2)

    # Food balance and Supply-Utilization-Account

    ## *[FBS] new food balance sheet (2010-) ----

    ## Load raw data
    FAOSTAT_load_raw_data("FBS") # New FBS  2010+
    FBS %>% distinct(element, element_code, unit)

    FBS %>% filter(item_code < 2901, item_code != 2501,
                   !element_code %in% c(511, 5301),
                   area_code %in% QCL_area_code) %>%
      select(area_code, area, item_code, item, element_code, element, year, value, unit) %>%
      rm_accent("item", "area") -> FBS1


    ### output FBS and clean memory ----
    FBS1 %>%
      add_title("FAO SCL") %>%
      add_units("tonne") %>%
      add_comments("Preprocessed FAOSTAT SCL") ->
      FBS
    rm(FBS1)


    ## *[SCL] SUA: supply utilization accounting ----

    FAOSTAT_load_raw_data("SCL")   # SUA      2010+
    SCL %>% distinct(element, element_code, unit)
    # FAOSTAT "accidentally" used CPC code in SCL;
    if (is.numeric(SCL$item_code)) {
      SCL %>% filter(item_code <= 1700, item_code != 1) -> SCL
    }

    SCL %>% filter(!element_code %in% c(664, 665, 674, 684, 511),
                   # it is not useful to calculate cal/g using `Food supply (kcal/capita/day)` /`Food supply quantity (g/capita/day)`
                   # unit too small so remove them here
                   # `Calories/Year` / `Food supply quantity (tonnes)` is more accurate!
                   # similarly for protein and fat
                   # Use annual value in SUA to calculate the conversion rate!
                   area_code %in% QCL_area_code) %>%
      select(area_code, area, item_code, item, element_code, element, year, value, unit) %>%
      rm_accent("item", "area") -> SCL1


     ### output SCL and clean memory ----
    SCL1 %>%
       add_title("FAO SCL") %>%
       add_units("tonne") %>%
       add_comments("Preprocessed FAOSTAT SCL") ->
      SCL
     rm(SCL1)


    return_data(MODULE_OUTPUTS)

  } else {
    stop("Unknown command")
  }
}
