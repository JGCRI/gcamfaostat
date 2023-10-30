# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

# module-helpers.R
# Module specific helper functions



#' downscale_FAO_country
#'
#' Helper function to downscale the countries that separated into
#' multiple modern countries (e.g. USSR).
#'
#' @param data Data to downscale, tibble
#' @param country_name Pre-dissolution country name, character
#' @param dissolution_year Year of country dissolution, integer
#' @param years Years to operate on, integer vector
#' @importFrom dplyr filter group_by select summarise_all ungroup
#' @importFrom stats aggregate
#' @return Downscaled data.
downscale_FAO_country <- function(data, country_name, dissolution_year, years = aglu.AGLU_HISTORICAL_YEARS) {

  assert_that(is_tibble(data))
  assert_that(is.character(country_name))
  assert_that(is.integer(dissolution_year))
  assert_that(is.integer(years))
  assert_that(dissolution_year %in% years)

  countries <- item <- element <- NULL                     # silence package check notes

  # Compute the ratio for all years leading up to the dissolution year, and including it
  # I.e. normalizing the time series by the value in the dissolution year
  ctry_years <- years[years < dissolution_year]
  yrs <- as.character(c(ctry_years, dissolution_year))
  data %>%
    select(item, element, yrs) %>%
    group_by(item, element) %>%
    summarise_all(sum) %>%
    ungroup ->
    data_ratio

  data_ratio[yrs] <- data_ratio[yrs] / data_ratio[[as.character(dissolution_year)]]

  # Use these ratios to project the post-dissolution country data backwards in time
  newyrs <- as.character(ctry_years)
  data_new <- filter(data, countries != country_name)
  data_new[newyrs] <- data_new[[as.character(dissolution_year)]] *
    data_ratio[match(paste(data_new[["item"]], data_new[["element"]]),
                     paste(data_ratio[["item"]], data_ratio[["element"]])), newyrs]
  data_new[newyrs][is.na(data_new[newyrs])] <- 0
  data_new
}



#' Moving average
#' @description function to calculate moving average
#'
#' @param x A data frame contain the variable for calculation
#' @param periods An odd number of the periods in MA. The default is 5, i.e., 2 lags and 2 leads
#' @param NA_RM If TRUE, remove NA in calculating mean, otherwise returning NA
#'
#' @return A data frame
#' @export

Moving_average <- function(x, periods = 5, NA_RM = TRUE){
  if (periods == 1) {
    return(x)
  }

  if ((periods %% 2) == 0) {
    stop("Periods should be an odd value")
  } else{

    #new method to allow na.rm in mean calculation
    c(lapply(seq((periods -1 )/2, 1), function(a){lag(x, n = a)}),
      list(x),
      lapply(seq(1, (periods -1 )/2), function(a){lead(x, n = a)})) %>% unlist %>%
      matrix(ncol = periods) %>%
      rowMeans(na.rm = NA_RM)

  }
}


# Function to dissaggregate dissolved regions in historical years ----

#' FAO_AREA_DISAGGREGATE_HIST_DISSOLUTION
#'
#' @param .DF dataframe to disaggregate
#' @param AFFECTED_AREA_CODE  FAO area codes for regions affected; first one should be pre-dissolved region (e.g., USSR) followed by post-dissolved regions.
#' @param YEAR_DISSOLVE_DONE  First year after dissolution
#' @param YEAR_AFTER_DISSOLVE_ACCOUNT Number of years of data after dissolution used for sharing historical data
#'
#' @return Disaggregated data for the historical period for of the dissolved region
#'
FAO_AREA_DISAGGREGATE_HIST_DISSOLUTION <-
  function(.DF,
           AFFECTED_AREA_CODE, #first one should be dissolved area
           YEAR_DISSOLVE_DONE,
           # using 3 year data after dissolution for sharing
           YEAR_AFTER_DISSOLVE_ACCOUNT = 3){

    area_code <- year <- value <- NODATA <- Share <- . <- NULL

    .DF %>%
      # filter dissolved region related areas by their years
      filter((area_code %in% AFFECTED_AREA_CODE[-1] & year >= YEAR_DISSOLVE_DONE)|
               (area_code %in% AFFECTED_AREA_CODE[1] & year <= YEAR_DISSOLVE_DONE)) ->
      .DF1

    Number_of_Regions_After_Dissolution <- AFFECTED_AREA_CODE %>% length -1

    .DF1 %>% filter(year < YEAR_DISSOLVE_DONE) %>%
      select(-area_code) %>%
      right_join(
        .DF1 %>% filter(year %in% c(YEAR_DISSOLVE_DONE:(YEAR_DISSOLVE_DONE + YEAR_AFTER_DISSOLVE_ACCOUNT))) %>%
          dplyr::group_by_at(dplyr::vars(-year, -value)) %>%
          replace_na(list(value = 0)) %>%
          summarise(value = sum(value), .groups = "drop") %>%
          dplyr::group_by_at(dplyr::vars(-value, -area_code)) %>%
          mutate(Share = value/sum(value)) %>%
          # using average share if data after dissolved does not exist
          mutate(NODATA = if_else(sum(value) == 0, T, F)) %>%
          mutate(Share = if_else(NODATA == T, 1/Number_of_Regions_After_Dissolution, Share)) %>%
          ungroup() %>%select(-value, -NODATA),
        by = names(.) %>% setdiff(c("year", "value", "area_code"))
      ) %>% mutate(value = value * Share) %>% select(-Share)

  }

#' FAO_AREA_DISAGGREGATE_HIST_DISSOLUTION_ALL
#'
#' @param .DF Input data frame
#' @param SUDAN2012_BREAK If T break Sudan before 2012 based on 2013- 2016 data
#' @param SUDAN2012_MERGE If T merge South Sudan into Sudan
#' @param .FAO_AREA_CODE_COL Name of col of area
#' @param .AREA_COL Name of col of area code
#'
#' @return data with historical periods of dissolved region disaggregated to small pieces.

FAO_AREA_DISAGGREGATE_HIST_DISSOLUTION_ALL <- function(.DF,
                                                       .FAO_AREA_CODE_COL = "area_code",
                                                       .AREA_COL = "area",
                                                       SUDAN2012_BREAK = F,
                                                       SUDAN2012_MERGE = T){

  area_code <- value <- area_code_TEMP <- NULL

  assertthat::assert_that(.FAO_AREA_CODE_COL %in% names(.DF),
                          msg = "Date frame is required and need a col of area_code")

  # Remove area if exist
  .DF0 <- .DF
  if (all_of(.AREA_COL) %in% names(.DF)) {
    .DF %>% select(-all_of(.AREA_COL)) -> .DF }

  if(.FAO_AREA_CODE_COL != "area_code"){
    # Check if "area_code" exist, replace to area_code_TEMP
    if ("area_code" %in% names(.DF)) {
      assertthat::assert_that("area_code_TEMP" %in% names(.DF) == F)
      .DF %>% rename(area_code_TEMP = area_code) -> .DF
    }

    # rename .FAO_AREA_CODE_COL to area_code

    .DF %>% rename(area_code = .FAO_AREA_CODE_COL) -> .DF

    # Will need to replace back later
  }


  # Define area code based on FAO ----
  # first one is dissolved area
  # In 1991 USSR(228) collapsed into 15 countries
  area_code_USSR = c(228, 1, 52, 57, 63, 73, 108, 113, 119, 126, 146, 185, 208, 213, 230, 235)
  # first one is Russia

  # In 1992 Yugoslav SFR dissolved into 5 countries
  # Yugoslav SFR (248)
  # Croatia (98)
  # North Macedonia (154)
  # Slovenia (198)
  # Bosnia and Herzegovina (80)
  # Serbia and Montenegro (186)
  # In 2006 further broke into 2:
  # Montenegro (273)
  # Serbia (272)
  # These regions will be merged for all years in data as most models aggregated them into a single region
  area_code_Yugoslav <- c(248, 98, 154, 198, 80, 186)
  area_code_SerbiaandMontenegro <- c(186, 273, 272)
  # In 1999/2000 Belgium-Luxembourg (15) partitioned in 1999 to 255 (Belgium) and 256 (Luxembourg)
  area_code_Belgium_Luxembourg <- c(15, 255, 256)
  # In 1993 Czechoslovakia (51) to Czechia (167) and Slovakia (199)
  area_code_Czechoslovakia <- c(51, 167, 199)
  # In 2011 Sudan (former) (206) broke into South Sudan (277) and Sudan (276)
  area_code_Sudan <- c(206, 276, 277)
  # Ethiopia PDR (62) dissolved into Ethiopia (238) and Eritrea (178) in 1993
  area_code_Ethiopia <- c(62, 238, 178)

  .DF %>%
    # remove Yugoslav by their years first and area_code_SerbiaandMontenegro later
    filter(!(area_code %in% area_code_Yugoslav[1] )) %>%
    bind_rows(FAO_AREA_DISAGGREGATE_HIST_DISSOLUTION(.DF, area_code_Yugoslav, 1992, 3))->
    .DF1


  FAO_AREA_DISAGGREGATE_HIST_DISSOLUTION(.DF1, area_code_USSR, 1992, 3) %>%
    bind_rows(FAO_AREA_DISAGGREGATE_HIST_DISSOLUTION(.DF1, area_code_SerbiaandMontenegro, 2006, 3)) %>%
    bind_rows(FAO_AREA_DISAGGREGATE_HIST_DISSOLUTION(.DF1, area_code_Belgium_Luxembourg, 2000, 3)) %>%
    bind_rows(FAO_AREA_DISAGGREGATE_HIST_DISSOLUTION(.DF1, area_code_Czechoslovakia, 1993, 3)) %>%
    bind_rows(FAO_AREA_DISAGGREGATE_HIST_DISSOLUTION(.DF1, area_code_Ethiopia, 1993, 3)) ->
    DF_FAO_AREA_DISAGGREGATE_HIST

  .DF1 %>%
    # remove USSR by their years
    filter(!(area_code %in% area_code_USSR[1])) %>%
    # remove Serbia & Montenegro by their years
    filter(!(area_code %in% area_code_SerbiaandMontenegro[1] )) %>%
    # remove Belgium_Luxembourg by their years
    filter(!(area_code %in% area_code_Belgium_Luxembourg[1])) %>%
    # remove area_code_Czechoslovakia by their years
    filter(!(area_code %in% area_code_Czechoslovakia[1] )) %>%
    # remove area_code_Ethiopia by their years
    filter(!(area_code %in% area_code_Ethiopia[1] )) %>%
    bind_rows(DF_FAO_AREA_DISAGGREGATE_HIST) ->
    .DF2

  if (SUDAN2012_BREAK == T) {
    .DF2 %>%
      # remove area_code_Sudan by their years
      filter(!(area_code %in% area_code_Sudan[1] )) %>%
      bind_rows(FAO_AREA_DISAGGREGATE_HIST_DISSOLUTION(.DF1, area_code_Sudan, 2012, 3)) ->
      .DF2
  }

  if (SUDAN2012_MERGE == T) {

    .DF2 %>%
      mutate(area_code = replace(area_code, area_code %in% area_code_Sudan, area_code_Sudan[1])) %>%
      dplyr::group_by_at(dplyr::vars(-value)) %>%
      summarise(value = sum(value, na.rm = T), .groups = "drop") %>%
      ungroup() -> .DF2
  }



  if(.FAO_AREA_CODE_COL != "area_code"){

    # rename .FAO_AREA_CODE_COL to area_code
    .DF2[[.FAO_AREA_CODE_COL]] <- .DF2[["area_code"]]
    .DF2 <- .DF2[, !names(.DF2) %in% "area_code"]

    # Check if "area_code_TEMP" exist, replace to area_code
    if ("area_code_TEMP" %in% names(.DF2)) {
      assertthat::assert_that("area_code" %in% names(.DF2) == F)
      .DF2 %>% rename(area_code = area_code_TEMP) -> .DF2
    }
  }



  if (all_of(.AREA_COL) %in% names(.DF0)) {
    .DF2 %>%# Get area back
      left_join(.DF0 %>% dplyr::distinct(across(c(.AREA_COL, .FAO_AREA_CODE_COL))) ,
                by = .FAO_AREA_CODE_COL) -> .DF2
  }

  return(.DF2)

}

