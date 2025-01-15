# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_yfaostat_Traceable_FBS_DataExport
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
#' @author XZ Sep2024
module_yfaostat_Traceable_FBS_DataExport <- function(command, ...) {

  MODULE_INPUTS <-
    c(FILE = file.path(DIR_RAW_DATA_FAOSTAT, "Mapping_gcamdata_SUA_PrimaryEquivalent"),
      FILE = file.path(DIR_RAW_DATA_FAOSTAT, "Mapping_gcamdata_SUA_ItemCode"),
      FILE = file.path(DIR_RAW_DATA_FAOSTAT, "Mapping_gcamdata_FAO_iso_reg"),
      "L105.Bal_new_all",
      "L107.Traceable_FBS_PCe_2010Plus",
      "L107.Traceable_FBS_Food_Calorie_Macronutrient_2010Plus",
      "L107.Traceable_FBS_PCe_Gross_Extraction_Rates_2010Plus")

    MODULE_OUTPUTS <-
      c(CSV = "Metadata_GCAMFAOSTAT_Traceable_FBS",
        CSV = "Nested_Mapping_SUA_To_Traceable_FBS",
        CSV = "SUA_2010_2022",
        CSV = "SUA_Food_Calorie_Macronutrient_2010_2022",
        CSV = "Traceable_FBS_PCe_2010_2022",
        CSV = "Traceable_FBS_Food_Calorie_Macronutrient_2010_2022",
        CSV = "Traceable_FBS_Extraction_Rate_2010_2022")

  if(command == driver.DECLARE_INPUTS) {
    return(MODULE_INPUTS)
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(MODULE_OUTPUTS)
  } else if(command == driver.MAKE) {

    year <- value <- Year <- Value <- FAO_country <- iso <- NULL    # silence package check.

    all_data <- list(...)[[1]]


    Curr_Envir <- environment()

    # out_filename_suffix is appended to output file names, e.g., dates could be used
    out_filename_suffix <- paste0("_", Sys.Date())
    out_filename_suffix <- ""

    # adding dummy output ----
    Metadata_GCAMFAOSTAT_Traceable_FBS <-
      tibble(CSV_export = MODULE_OUTPUTS)

    if (OUTPUT_Export_CSV == "Traceable_FBS") {

      # Load required inputs ----
      get_data_list(all_data, MODULE_INPUTS, strip_attributes = TRUE)

      # Helper functions ----
      Metadata_GCAMFAOSTAT_Traceable_FBS <- NULL
      add_to_output_meta <- function(.df,
                                     .datasetname,
                                     .skip_stat = FALSE){

        if (.skip_stat == FALSE) {

          .df %>% gather_years() -> .df
          if ("year" %in% names(.df)) {
            YearRange = paste0(.df %>% distinct(year) %>% min," to ", .df %>% distinct(year) %>% max)
          } else{
            YearRange = "BaseYears"
          }

          Metadata_GCAMFAOSTAT_Traceable_FBS %>%
            bind_rows(
              data.frame(dataset = .datasetname,
                         nReg = .df %>% distinct(iso) %>% nrow,
                         nItem = .df %>% distinct(item) %>% nrow,
                         Years = YearRange)
            ) -> .df1
        } else {

          Metadata_GCAMFAOSTAT_Traceable_FBS %>%
            bind_rows(
              data.frame(dataset = .datasetname)
            ) -> .df1
        }

        assign("Metadata_GCAMFAOSTAT_Traceable_FBS", .df1, envir = Curr_Envir)
      }


      # * Mapping_PrimaryEquivalent_nested_SUA ----

      Mapping_gcamdata_SUA_PrimaryEquivalent %>%
        transmute(aggregated_PCe_item = APE_comm_Agg, PCe_item = APE_comm,
                  nest_level, source_item0 = source_item, sink_item0 = sink_item) %>%
        distinct() %>%
        left_join_error_no_match(
          Mapping_gcamdata_SUA_ItemCode %>%
            distinct(source_item0 = item,  source_item_code = item_code, source_item = item_new_2024), by = "source_item0") %>%
        left_join_error_no_match(
          Mapping_gcamdata_SUA_ItemCode %>%
            distinct(sink_item0 = item,  sink_item_code = item_code, sink_item = item_new_2024), by = "sink_item0") %>%
        transmute(aggregated_PCe_item, PCe_item,
                  nest_level, source_item, source_item_code, sink_item, sink_item_code) %>%
        add_title("Nested_Mapping_SUA_To_Traceable_FBS") %>%
        add_units("none") %>%
        add_comments("gcamfaostat Export CSV") %>%
        add_precursors(file.path(DIR_RAW_DATA_FAOSTAT, "Mapping_gcamdata_SUA_PrimaryEquivalent")) ->
        Nested_Mapping_SUA_To_Traceable_FBS

      output_csv_data(
        gcam_dataset = Nested_Mapping_SUA_To_Traceable_FBS,
        out_filename = "Nested_Mapping_SUA_To_Traceable_FBS" %>% paste0(out_filename_suffix),
        col_type_nonyear = "ccicici",
        title = "Mapping of SUA items to aggregated primary commodity equivalent products by level of nestings",
        unit = "NA",
        code = "NA",
        description = "Data is compiled and generated by gcamfaostat.",
        out_dir = DIR_OUTPUT_CSV,
        GZIP = F)

      add_to_output_meta(.df = Nested_Mapping_SUA_To_Traceable_FBS,
                         "Nested_Mapping_SUA_To_Traceable_FBS", .skip_stat = TRUE)


      # Zeros were removed in all dataset in exporting to reduce size
      # We updated item names to the latest when applicable

      # * SUA_2010_2022 ----

      L105.Bal_new_all %>%
        filter(value != 0.0) %>%
        left_join_error_no_match(
          Mapping_gcamdata_FAO_iso_reg %>% select(area_code, iso), by = "area_code") %>%
        select(-item) %>%
        left_join_error_no_match(
          Mapping_gcamdata_SUA_ItemCode %>% distinct(item_code, item = item_new_2024),
          by = "item_code") %>%
        transmute(iso, year, item, item_code, element, unit = "1000 tonnes", value) %>%
        spread(year, value, fill = 0) %>%
        add_title("SUA_2010_2022") %>%
        add_units("1000 tonnes") %>%
        add_comments("gcamfaostat Export CSV") %>%
        add_precursors("L105.Bal_new_all",
                       file.path(DIR_RAW_DATA_FAOSTAT, "Mapping_gcamdata_SUA_ItemCode")) ->
        SUA_2010_2022

      output_csv_data(
        gcam_dataset = SUA_2010_2022,
        out_filename = "SUA_2010_2022" %>% paste0(out_filename_suffix),
        col_type_nonyear = "ccifc",
        title = "Supply utilization accounts for all FAO items in 2010 - 2022",
        unit = "1000 tonnes",
        code = "SCL",
        description = "Data is compiled and generated by gcamfaostat. Data is balanced in trade, supply_utilization, and storage",
        out_dir = DIR_OUTPUT_CSV,
        GZIP = T)

      add_to_output_meta(.df = SUA_2010_2022, "SUA_2010_2022")


      #* SUA_Food_Calorie_Macronutrient_2010_2022 ----

      L107.Traceable_FBS_Food_Calorie_Macronutrient_2010Plus %>%
        left_join_error_no_match(
          Mapping_gcamdata_FAO_iso_reg %>% select(area_code, iso), by = "area_code") %>%
        group_by(iso, year, item, item_code, element, unit) %>%
        summarize(value = sum(value), .groups = "drop") %>%
        filter(value >0) %>%
        spread(year, value, fill = 0)  %>%
        add_title("SUA_Food_Calorie_Macronutrient_2010_2022") %>%
        add_units("Mt or Mkcal") %>%
        add_comments("gcamfaostat Export CSV") %>%
        add_precursors("L107.Traceable_FBS_Food_Calorie_Macronutrient_2010Plus") ->
        SUA_Food_Calorie_Macronutrient_2010_2022

      output_csv_data(
        gcam_dataset = SUA_Food_Calorie_Macronutrient_2010_2022,
        out_filename = "SUA_Food_Calorie_Macronutrient_2010_2022" %>% paste0(out_filename_suffix),
        col_type_nonyear = "ccicc",
        title = "SUA: food energy and macronutrient in 2010 - 2022",
        unit = "Mt or Mkcal",
        code = "SCL",
        description = "Data is compiled and generated by gcamfaostat. Food energy and macronutrient by iso and SUA items. Item and item code are based on FAO/gcamfaostat definitions",
        out_dir = DIR_OUTPUT_CSV,
        GZIP = T)

      add_to_output_meta(.df = SUA_Food_Calorie_Macronutrient_2010_2022,
                         "SUA_Food_Calorie_Macronutrient_2010_2022")


      #* Traceable_FBS_PCe_2010_2022 ----

      L107.Traceable_FBS_PCe_2010Plus %>%
        filter(value != 0.0) %>%
        left_join_error_no_match(
          Mapping_gcamdata_FAO_iso_reg %>% select(region_ID, iso), by = "region_ID") %>%
        transmute(iso, year, item = APE_comm_Agg, element, unit = "1000 tonnes", value) %>%
        spread(year, value, fill = 0)  %>%
        add_title("Traceable_FBS_PCe_2010_2022") %>%
        add_units("1000 tonnes") %>%
        add_comments("gcamfaostat Export CSV") %>%
        add_precursors("L107.Traceable_FBS_PCe_2010Plus") ->
        Traceable_FBS_PCe_2010_2022

      output_csv_data(
        gcam_dataset = Traceable_FBS_PCe_2010_2022,
        out_filename = "Traceable_FBS_PCe_2010_2022" %>% paste0(out_filename_suffix),
        col_type_nonyear = "cccc",
        title = "Traceable FBS: supply utilization accounts in primary equivalent for APE items in 2010 - 2022",
        unit = "1000 tonnes",
        code = "SCL",
        description = "Data is compiled and generated by gcamfaostat.",
        out_dir = DIR_OUTPUT_CSV,
        GZIP = T)

      add_to_output_meta(.df = Traceable_FBS_PCe_2010_2022,
                         "Traceable_FBS_PCe_2010_2022")

      #* Traceable_FBS_Food_Calorie_Macronutrient_2010_2022 ----

      L107.Traceable_FBS_Food_Calorie_Macronutrient_2010Plus %>%
        left_join_error_no_match(
          Mapping_gcamdata_FAO_iso_reg %>% select(area_code, iso), by = "area_code") %>%
        group_by(iso, year, item = APE_comm_Agg, element, unit) %>%
        summarize(value = sum(value), .groups = "drop") %>%
        filter(value >0) %>%
        spread(year, value, fill = 0) %>%
        add_title("Traceable_FBS_Food_Calorie_Macronutrient_2010_2022") %>%
        add_units("Mt or Mkcal") %>%
        add_comments("gcamfaostat Export CSV") %>%
        add_precursors("L107.Traceable_FBS_Food_Calorie_Macronutrient_2010Plus") ->
        Traceable_FBS_Food_Calorie_Macronutrient_2010_2022

      output_csv_data(
        gcam_dataset = Traceable_FBS_Food_Calorie_Macronutrient_2010_2022,
        out_filename = "Traceable_FBS_Food_Calorie_Macronutrient_2010_2022" %>% paste0(out_filename_suffix),
        col_type_nonyear = "cccc",
        title = "Traceable FBS: food energy and macronutrient in 2010 - 2022",
        unit = "Mt or Mkcal",
        code = "SCL",
        description = "Data is compiled and generated by gcamfaostat. Food energy and macronutrient by iso and APE items",
        out_dir = DIR_OUTPUT_CSV,
        GZIP = T)

      add_to_output_meta(.df = Traceable_FBS_Food_Calorie_Macronutrient_2010_2022,
                         "Traceable_FBS_Food_Calorie_Macronutrient_2010_2022")





     # * Traceable_FBS_Extraction_Rate_2010_2022 ----
      # generate a base mapping to join table
      # there is some info from the mapping to be used to update the rates
      # E.g., output specific

      Mapping_gcamdata_SUA_PrimaryEquivalent %>%
        distinct(APE_comm_Agg, APE_comm, nest_level, source_item,
                 sink_item, sink_item, source_item,
                 output_specific_extraction_rate,
                 min_extraction_rate = extraction_rate_Q25, Q25asMin ) %>%
        filter(nest_level >= 1) %>%
        mutate(min_extraction_rate = if_else(Q25asMin == F, 0, min_extraction_rate)) ->
        Extraction_Rate_Mapping

      Extraction_Rate_Mapping %>%
        replace_na(list(output_specific_extraction_rate = 1)) %>%
        full_join(L107.Traceable_FBS_PCe_Gross_Extraction_Rates_2010Plus,
                  by = c("APE_comm", "nest_level", "source_item", "sink_item")) %>%
        left_join_error_no_match(
          Mapping_gcamdata_FAO_iso_reg %>% select(region_ID, iso), by = "region_ID") %>%
        transmute(aggregated_PCe_item = APE_comm_Agg, PCe_item = APE_comm, iso,  year,
                  nest_level, source_item, sink_item,
                  output_specific_extraction_rate,
                  ER_imported = bal_import,
                  ER_domestic = bal_domestic_current,
                  ER_lagged =  bal_domestic_lag )  %>%
        gather(ER, value, ER_imported, ER_domestic, ER_lagged) %>%
        # incorporate output_specific_extraction_rate into ER
        mutate(value = value * output_specific_extraction_rate) %>%
        select(-output_specific_extraction_rate) %>%
        rename(source_item0 = source_item, sink_item0 = sink_item) %>%
        left_join_error_no_match(
          Mapping_gcamdata_SUA_ItemCode %>%
            distinct(source_item0 = item,  source_item_code = item_code, source_item = item_new_2024), by = "source_item0") %>%
        left_join_error_no_match(
          Mapping_gcamdata_SUA_ItemCode %>%
            distinct(sink_item0 = item,  sink_item_code = item_code, sink_item = item_new_2024), by = "sink_item0") %>%
        transmute(aggregated_PCe_item, PCe_item, iso,  year,
                  nest_level, source_item, sink_item, extraction_point = ER, value) %>%
        filter(is.finite(value)) %>%
        add_title("Traceable_FBS_Extraction_Rate_2010_2022") %>%
        add_units("none") %>%
        add_comments("gcamfaostat Export CSV") %>%
        add_precursors(file.path(DIR_RAW_DATA_FAOSTAT, "Mapping_gcamdata_SUA_PrimaryEquivalent")) %>%
        spread(year, value)->
        Traceable_FBS_Extraction_Rate_2010_2022

      output_csv_data(
        gcam_dataset = Traceable_FBS_Extraction_Rate_2010_2022,
        out_filename = "Traceable_FBS_Extraction_Rate_2010_2022" %>% paste0(out_filename_suffix),
        col_type_nonyear = "cciiccc",
        title = "Extraction rates used for compiling the traceable FBS dataset",
        unit = "NA",
        code = "NA",
        description = "Data is compiled and generated by gcamfaostat.",
        out_dir = DIR_OUTPUT_CSV,
        GZIP = T)

      add_to_output_meta(.df = Traceable_FBS_Extraction_Rate_2010_2022 %>% rename(item = sink_item),
                         "Traceable_FBS_Extraction_Rate_2010_2022")


      # Metadata ----
      output_csv_data(
        gcam_dataset = Metadata_GCAMFAOSTAT_Traceable_FBS,
        out_filename = "Metadata_GCAMFAOSTAT_Traceable_FBS",
        col_type_nonyear = "ciic",
        title = "Information of dataset exported by gcamfaostat",
        unit = "NA",
        description = "Data is preprocessed and generated by gcamfaostat v1.1",
        code = "NA",
        out_dir = DIR_OUTPUT_CSV)

      }
    else {

      lapply(MODULE_OUTPUTS[MODULE_OUTPUTS %>% names() == "CSV"],
             function(output){
               assign(output, empty_data() %>%
                        add_title(output) %>%
                        add_precursors("Metadata_GCAMFAOSTAT_Traceable_FBS"), envir =  Curr_Envir)
             })

        }

    Metadata_GCAMFAOSTAT_Traceable_FBS %>%
      add_title("Export CSV to DIR_OUTPUT_CSV", overwrite = T) %>%
      add_units("NA") %>%
      add_comments("Export CSV") %>%
      add_precursors(MODULE_INPUTS) ->
      Metadata_GCAMFAOSTAT_Traceable_FBS

    return_data(MODULE_OUTPUTS)

  } else {
    stop("Unknown command")
  }
}
