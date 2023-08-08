# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_xfaostat_L101_RawDataPreProcessing2
#'
#' Preprocess raw faostat data part 2
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
module_xfaostat_L101_RawDataPreProcessing2 <- function(command, ...) {

  MODULE_INPUTS <-
    c(FILE = "aglu/AGLU_ctry",
      "QCL_area_code_map")

  MODULE_OUTPUTS <-
    c("FBSH",             # Old food balance sheet
      "CB",               # Old non food utilization accounting
      "FBSH_CB",          # Combined FBSH and CB
      "OA"                # Population
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



    QCL_area_code <- QCL_area_code_map %>% distinct(area_code) %>% pull()


    # Food balance and Supply-Utilization-Account

    ## *[FBSH] old food balance sheet (-2013) ----

    FAOSTAT_load_raw_data("FBSH")  # Old FBS -2013
    FBSH %>% distinct(element, element_code, unit)
    # Keep population (old)
    FBSH %>% filter(item_code < 2901,
                    !element_code %in% c(5301),
                    area_code %in% QCL_area_code) %>%
      select(area_code, area, item_code, item, element_code, element, year, value, unit) %>%
      rm_accent("item", "area") -> FBSH1


    ### output FBSH and clean memory ----

    FBSH1 %>%
      add_title("FAO FBSH") %>%
      add_units("tonne") %>%
      add_comments("Preprocessed FAO FBSH") ->
      FBSH

    rm(FBSH1)


    ## *[CB] Non-food Balance ----

    FAOSTAT_load_raw_data("CB")    # Old FBS-nonfood -2013
    CB %>% distinct(element, element_code, unit)
    # Keep population (old)
    CB %>% filter(item_code < 2901,
                  !element_code %in% c(5300),
                  area_code %in% QCL_area_code) %>%
      select(area_code, area, item_code, item, element_code, element, year, value, unit) %>%
      rm_accent("item", "area") -> CB1

    ### output CB and clean memory ----

    CB1 %>%
      add_title("FAO CB") %>%
      add_units("tonne") %>%
      add_comments("Preprocessed FAO CB") ->
      CB

    rm(CB1)


    ## *FBSH_CB merge the two----
    # load processed data

    FBSH %>% distinct(item_code) %>%
      dplyr::intersect(CB %>% distinct(item_code) ) %>%
      pull ->
      dup_item_code


    FBSH %>% mutate(value = value * 1000) %>% # convert to ton
      bind_rows(CB %>% filter(!item_code %in% dup_item_code)) %>%
      select(-unit) %>%
      filter(!element_code %in% c(645, 664, 674, 684, 511)) %>%  # remove non-balance items
      mutate(element = gsub(" Quantity| supply quantity \\(tonnes\\)| \\(non-food\\)", "", element)) %>%
      mutate( element = replace(element, element == "Losses", "Loss"),
              element = replace(element, element == "Processing", "Processed")) %>%
      select(-element_code) %>%
      mutate(unit = "tonnes") ->
      FBSH_CB

    # Rice and products and Groundnuts related adjustments
    # to make FBS and FBSH more consistent
    SHELL_RATE_groundnuts <- 0.7
    Mill_RATE_rice <- 0.667

    FBSH_CB %>% distinct(element, unit)
    FBSH_CB %>%
      filter(!item_code %in% c(2805, 2556)) %>%
      # Adjust to Rice and products and Groundnuts in FBS and bind
      bind_rows(
        FBSH_CB %>% filter(item_code %in%  c(2805)) %>%
          mutate(item = "Rice and products", item_code = 2807,
                 value = value / Mill_RATE_rice) %>%
          bind_rows(FBSH_CB %>% filter(item_code %in%  c(2556)) %>%
                      mutate(item = "Groundnuts", item_code = 2552,
                             value = value / SHELL_RATE_groundnuts) )
      ) ->
      FBSH_CB1

    ### output FBSH_CB and clean memory ----

    FBSH_CB1 %>%
      add_title("FAO FBSH_CB", overwrite = T) %>%
      add_units("tonne") %>%
      add_comments("Preprocessed FAO FBSH_CB") ->
      FBSH_CB

    rm(SHELL_RATE_groundnuts, Mill_RATE_rice);
    rm(FBSH_CB1, dup_item_code)


    # *[OA]: Population ----

    FAOSTAT_load_raw_data("OA")    # Population
    OA %>% distinct(element, element_code)
    OA %>% distinct(item, item_code)

    OA %>% filter(element_code == 511, item_code == 3010)  %>%
      select(area_code, area, item_code, item, element_code, element, year, value, unit) %>%
      rm_accent("item", "area") -> OA1

    ### output OA and clean memory ----
    OA1 %>%
      add_title("FAO OA") %>%
      add_units("tonne") %>%
      add_comments("Preprocessed FAO OA") ->
      OA
    rm(OA1)
    rm(QCL_area_code)


    return_data(MODULE_OUTPUTS)

  } else {
    stop("Unknown command")
  }
}
