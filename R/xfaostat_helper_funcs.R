
# Functions start with FF or FAOSTAT indicate Functions processing FAO data


#' FAOSTAT_metadata: get the most recent metadata from FAOSTAT API and return a summary data frame
#' @description Download and parse the metadata file from the FAOSTAT portal. FAOSTAT_metadata was adapted from the FAOSTAT package.
#' @param datasetcode Dataset code in FAO metadata, e.g., QCL is the production dataset. If NULL, full metadata will be pulled.
#' @importFrom  XML xmlParse xmlToDataFrame
#' @importFrom  xml2 read_xml
#' @return Dataframe including the metadata of FAOSTAT including URL for download, updating date, detailed descriptions, etc.
#' @export
#'
#' @examples FAOSTAT_metadata
FAOSTAT_metadata <- function (code = NULL){
  FAOxml <- XML::xmlParse(xml2::read_xml("http://fenixservices.fao.org/faostat/static/bulkdownloads/datasets_E.xml"))
  metadata <- XML::xmlToDataFrame(FAOxml, stringsAsFactors = FALSE)
  names(metadata) <- tolower(gsub("\\.", "_", names(metadata)))

  # Bug fix for CB; can remove later if FAOSTAT update the link later
  metadata["CB" == metadata[, "datasetcode"],"filelocation"] <-
    "https://fenixservices.fao.org/faostat/static/bulkdownloads/CommodityBalances_(non-food)_E_All_Data_(Normalized).zip"

  if (!is.null(code)) {
    metadata <- metadata[code == metadata[, "datasetcode"],]
  }

  return(metadata)
}




#' FAOSTAT_load_raw_data: load raw csv data
#' @description Read csv data and "." in column name is substituted with "_".
#' @param DATASETCODE Dataset code in FAO metadata or the name of a csv file.
#' @param DATA_FOLDER Path to the folder storing the data.
#' @importFrom  readr read_csv
#' @importFrom  magrittr %>%
#' @importFrom  assertthat assert_that
#' @export

FAOSTAT_load_raw_data <- function(DATASETCODE,
                                  DATA_FOLDER = DIR_RAW_DATA_FAOSTAT){
  assertthat::assert_that(is.character(DATASETCODE))
  assertthat::assert_that(is.character(DATA_FOLDER))

  my_env <- new.env()

  metadata <- FAOSTAT_metadata()

  # Loop through each code
  for (CODE in DATASETCODE) {

    metadata %>% filter(datasetcode == CODE) -> metadata1

    zip_file_name <- file.path(DATA_FOLDER, basename(metadata1$filelocation))
    assertthat::assert_that(file.exists(zip_file_name))
    # assuming the csv in zip has the same base name
    csv_file_name <- gsub(".zip$", ".csv", basename(zip_file_name))

    df <- readr::read_csv(unz(zip_file_name, csv_file_name), col_types = NULL)
    # Lower case col names and use _ as delimiter
    names(df) <- tolower(gsub("\\.| ", "_", names(df)))
    # Assigned to parent env
    #assign(CODE, df, envir = parent.frame())
    assign(CODE, df, envir = parent.env(environment()))
    # Assigned to current env
    #assign(CODE, df, envir = .GlobalEnv)
  }

}





#' FAO_AREA_RM_NONEXIST: remove nonexistent FAO region using area_code, e.g., USSR after 1991
#' @description This function was developed when exploring FAO production data.
#' Additional small regions/areas are also remove due to low data quality.
#'
#' @param .DF A input data frame
#' @param RM_AREA_CODE Additional area_codes to remove, e.g., small regions with low data quality.
#' @param SUDAN2012_MERGE If TRUE, only keep one merged Sudan region
#'
#' @return A data frame with nonexistent area and area_code removed
#' @export

FAO_AREA_RM_NONEXIST <- function(.DF,
                                 SUDAN2012_MERGE = F,
                                 RM_AREA_CODE = c(69, 87, 127, 135, 145, 182, 299)){

  assertthat::assert_that("area_code" %in% names(.DF),
                          msg = "Date frame is required and need a col of area_code")

  # Removed area due to missing data in other places and incomplete years mostly
  # There are 7
  # 69 French Guyana
  # 87 Guadeloupe
  # 127 Marshall Islands
  # 135 Martinique
  # 145 Micronesia (Federated States of)
  # 299 Palestine
  # 182 Reunion
  # area_code_remove <- c(69, 87, 127, 135, 145, 182, 299)


  # In 1991 USSR(228) collapsed into 15 countries
  area_code_USSR = c(228, 1, 52, 57, 63, 73, 108, 113, 119, 126, 146, 185, 208, 213, 230, 235)

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
    filter(!area_code %in% RM_AREA_CODE) %>%
    # remove USSR related regions by their years
    filter(!(area_code %in% area_code_USSR[1] & year >= 1992),
           !(area_code %in% area_code_USSR[-1] & year <= 1991)) %>%
    # remove Yugoslav and Serbia & Montenegro related regions by their years
    filter(!(area_code %in% area_code_Yugoslav[1] & year >= 1992),
           !(area_code %in% area_code_Yugoslav[-1] & year <= 1991)) %>%
    filter(!(area_code %in% area_code_SerbiaandMontenegro[1] & year >= 2006),
           !(area_code %in% area_code_SerbiaandMontenegro[-1] & year <= 2005)) %>%
    # remove Belgium_Luxembourg related regions by their years
    filter(!(area_code %in% area_code_Belgium_Luxembourg[1] & year >= 2000),
           !(area_code %in% area_code_Belgium_Luxembourg[-1] & year <= 1999)) %>%
    # remove area_code_Czechoslovakia related regions by their years
    filter(!(area_code %in% area_code_Czechoslovakia[1] & year >= 1993),
           !(area_code %in% area_code_Czechoslovakia[-1] & year <= 1992)) %>%
    # remove area_code_Ethiopia related regions by their years
    filter(!(area_code %in% area_code_Ethiopia[1] & year >= 1993),
           !(area_code %in% area_code_Ethiopia[-1] & year <= 1992)) ->
    .DF1

  if (SUDAN2012_MERGE == F) {
    .DF1 %>%
      # remove area_code_Sudan related regions by their years
      filter(!(area_code %in% area_code_Sudan[1] & year >= 2012),
             !(area_code %in% area_code_Sudan[-1] & year <= 2011)) ->
      .DF1
  } else {
    .DF1 %>%
      filter(!(area_code %in% area_code_Sudan[-1])) ->
      .DF1
  }


  return(.DF1)

}



#' FF_summary summarize a loaded FAO dataset
#'
#' @param DF A data frame
#' @param COL_CNTY Country column name
#' @param COL_ITEM Item column name
#' @importFrom dplyr summarize
#' @importFrom  magrittr %>%
#'
#' @return A data frame with summary information,
#' @export

FF_summary <- function(DF, COL_CNTY = "area", COL_ITEM = "item"){
  assert_that(is.character(DF))

  get(DF) %>%
    gather_years() -> .tbl1
  list_out<- list(
    dataset_code = DF,
    ncountry = length(unique(.tbl1[COL_CNTY])%>% pull()),
    nitem = length(unique(.tbl1[COL_ITEM])%>% pull()),
    nyear = length(unique(.tbl1$year)),
    start_year = min(unique(.tbl1$year)),
    end_year = max(unique(.tbl1$year)) ,
    NA_perc = paste0(round(.tbl1 %>%
                             summarize(sum(is.na(value))/n()) %>%
                             as.numeric() *100 , 1), "%"),
    nelement = length(unique(.tbl1$element)),
    element = paste(unique(.tbl1$element),collapse = ", ")

  )

  data.frame(t(sapply(list_out %>% unlist(),c)))
}


#' FF_FILL_NUMERATOR_DENOMINATOR Fill in missing values considering relationship between two variables
#' @description Fill in based on relationship between NUMERATOR_c and DENOMINATOR_c.
#' E.g., yield = production / area. NUMERATOR_c will be linearly interpolated forward
#' then NUMERATOR_FILL_DIRECTION. Yield will be filled down-up, World average yield
#' is used when only NUMERATOR_c or DENOMINATOR_c is available
#' @param .DF Input data frame
#' @param NUMERATOR_c NUMERATOR col name (production) in the ratio
#' @param DENOMINATOR_c DENOMINATOR col name (area) in the ratio
#' @param NUMERATOR_FILL_DIRECTION Direction of filling in NUMERATOR
#' @import dplyr
#' @importFrom tidyr fill replace_na
#'
#' @return A data frame with missing values filled
#' @export

FF_FILL_NUMERATOR_DENOMINATOR <- function(.DF, NUMERATOR_c, DENOMINATOR_c,
                                          NUMERATOR_FILL_DIRECTION = "down"){
  .DF %>% rename(NUMERATOR = NUMERATOR_c, DENOMINATOR = DENOMINATOR_c) %>%
    mutate(Yield = if_else(DENOMINATOR > 0, NUMERATOR / DENOMINATOR, NA_real_)) %>%
    group_by(area_code, area, item_code, item) %>%
    # setting NUMERATOR to NA if both prod and area are 0; it improves NUMERATOR interpolation
    mutate(NUMERATOR = if_else(NUMERATOR == 0 & DENOMINATOR == 0, NA_real_, NUMERATOR)) %>%
    # linearly interpolate NUMERATOR and fill in yield down-up
    mutate(NUMERATOR = approx_fun(year, NUMERATOR)) %>%
    #mutate(NUMERATOR = gcamdata::approx_fun(year, NUMERATOR)) %>%
    tidyr::fill(NUMERATOR, .direction = NUMERATOR_FILL_DIRECTION) %>%
    tidyr::fill(Yield, .direction = "downup") %>%
    ungroup() %>%
    # Calculate area based on checked NUMERATOR and yield
    mutate(DENOMINATOR = if_else(is.na(DENOMINATOR) & Yield != 0,
                                 NUMERATOR / Yield, DENOMINATOR)) %>%
    # setting back to zero when both NA
    replace_na(list(NUMERATOR = 0, DENOMINATOR = 0)) %>%
    select(-Yield) ->
    .DF1

  .DF1 %>%
    group_by(item_code, year) %>%
    summarise(NUMERATOR = sum(NUMERATOR),
              DENOMINATOR = sum(DENOMINATOR), .groups = "drop") %>%
    mutate(Yield_Mean = NUMERATOR / DENOMINATOR) %>%
    ungroup() %>%
    select(item_code, year, Yield_Mean) %>%
    group_by(item_code) %>%
    tidyr::fill(Yield_Mean, .direction = "downup") %>%
    ungroup() ->
    DF1_Yield_Mean

  # Fill some missing yield values with ex-ante world average yield
  # Only matters for items with missing prod for all years but with positive area
  # Could have used regional yield ratio to world of a set of item or an anchor item with higher uncertainty though (depending on the anchor)

  .DF1 %>%
    left_join(DF1_Yield_Mean, by = c("item_code", "year")) %>%
    mutate(DENOMINATOR = if_else(DENOMINATOR == 0 & NUMERATOR > 0, NUMERATOR / Yield_Mean,  DENOMINATOR),
           NUMERATOR = if_else(DENOMINATOR > 0 & NUMERATOR == 0, DENOMINATOR* Yield_Mean,  NUMERATOR)) %>%
    select(-Yield_Mean) %>%
    gather(element, value, DENOMINATOR, NUMERATOR) %>%
    mutate(element = replace(element, element == "DENOMINATOR", DENOMINATOR_c)) %>%
    mutate(element = replace(element, element == "NUMERATOR", NUMERATOR_c)) ->
    .DF2
  return((.DF2))
}


#' FF_join_checkmap: full-join data frames by a common COL_by variable to checking mapping
#'
#' @param DFs Data frames to be full joined.
#' @param COL_by By variable in join.
#' @param COL_rename Other common variables to rename (by adding df names as prefix) before the join.
#' @importFrom  magrittr %>%
#' @importFrom dplyr rename_at select any_of all_of full_join
#' @importFrom purrr reduce
#' @return A joined data frame
#' @export
FF_join_checkmap <- function(DFs, COL_by, COL_rename){
  lapply(DFs, function(df){

    get(df, envir = parent.frame(n = 3)) %>%
      select(all_of(COL_by), any_of(COL_rename)) %>% distinct() %>%
      dplyr::rename_at(vars(any_of(COL_rename)), list(~paste0(df, "_", .)))
  }) %>% purrr:: reduce(full_join, by = COL_by)
}

#' FF_check_count_plot: count item_code and area_code by year
#'
#' @param .DF Input data frame
#' @param .ELEMENT A set of elements (in Char) to focus. If empty, all elements are summarized
#' @importFrom  dplyr summarize
#' @importFrom  magrittr %>%
#' @importFrom  tidyr gather
#' @importFrom  ggplot2 ggplot aes facet_wrap geom_line theme_bw
#'
#' @return A plot summarizing the time-series of changing the count of item_code and area_code (grouped by element).
#' @export
FF_check_count_plot <- function(.DF, .ELEMENT = c()){
  if (.ELEMENT %>% length() == 0 ) {
    .DF %>% distinct(element) %>% pull -> .ELEMENT
  }
  .DF %>% group_by(year, element) %>%
    summarise(Country = length(unique(area_code)),
              Item = length(unique(item_code)), .groups = "drop") %>%
    gather(header, count, -year, -element) %>%
    filter(element %in% .ELEMENT) %>%
    ggplot() + facet_wrap(~header, scales = "free") +
    geom_line(aes(x = year, y = count, color = element)) +
    theme_bw()
}

#remove accent and apostrophe for cols in a df
#' rm_accent: Remove accent and prime in selected columns of a data frame
#'
#' @param .df Input data frame
#' @param ... A character set of column names
#' @importFrom  magrittr %>%
#' @importFrom  assertthat assert_that
#' @importFrom  dplyr intersect mutate_at
#'
#' @return A data frame with accent and prime removed
#' @export

rm_accent <- function(.df, ...){

  assertthat::assert_that(
    length(intersect(c(...), names(.df))) == length(c(...)),
    msg = "Columns listed not included in the data frame")

  # .df %>%
  #   mutate_at(c(...), iconv,  to = 'ASCII//TRANSLIT') %>%
  #   mutate_at(c(...), .funs = gsub, pattern = "\\'", replacement = "")

  .df %>%
    mutate(dplyr::across(c(...), iconv, to = 'ASCII//TRANSLIT')) %>%
    mutate(dplyr::across(c(...), gsub, pattern = "\\'", replacement = ""))

}

assert_FBS_balance <- function(.DF){


  # assert .DF structure
  assert_that(is.data.frame(.DF))
  assertthat::assert_that(all(c("element") %in% names(.DF)))
  assertthat::assert_that(dplyr::is.grouped_df(.DF) == F)

  # Check data
  # 1. Positive value except stock variation and residues
  if (isTRUE(.DF %>%
             filter(!element %in% c("Stock Variation", "Residuals")) %>%
             summarise(min = min(value, na.rm = T)) %>% pull(min) >= -0.001)) {
    message("Good! Signs checked") } else{
      warning("Negative values in key elements (not including stock variation and Residuals)")
    }

  # 2. Trade balance in all year and items
  if (isTRUE(.DF %>% filter(element %in% c("Import", "Export")) %>%
             group_by(year, item_code, element) %>%
             summarise(value = sum(value, na.rm = T), .groups = "drop") %>%
             spread(element, value) %>% filter(abs(Import - Export) > 0.0001) %>% nrow() == 0)) {
    message("Good! Gross trade in balance") } else{
      warning("Gross trade imbalance")
    }

  # 3. SUA balance check
  if (isTRUE(.DF %>%
             spread(element, value) %>%
             mutate(`Regional supply` = Production + `Import`,
                    `Regional demand` = `Export` + Feed + Food + Loss + Processed + Seed + `Other uses` + `Stock Variation`,
                    bal = abs(`Regional supply` -  `Regional demand` - Residuals)) %>%
             filter(bal > 0.0001) %>% nrow() == 0)) {
    message("Good! Regional supply = Regional demand + Residuals") } else{
      warning("Regional supply != Regional demand + Residuals")
    }

  # 4. Storage in balance across time
  if (isTRUE(.DF %>% filter(element %in% c("Opening stocks", "Closing stocks", "Stock Variation")) %>%
             spread(element, value) %>%
             filter(`Opening stocks` + `Stock Variation` - `Closing stocks` != 0) %>% nrow() == 0 &
             .DF %>% filter(element %in% c("Opening stocks", "Closing stocks", "Stock Variation")) %>%
             spread(element, value) %>% group_by(area_code, item_code) %>%
             dplyr::arrange(area_code, item_code, year) %>%
             mutate(bal = abs(dplyr::lag(`Closing stocks`) - `Opening stocks`)) %>%
             filter(is.na(bal) == F, bal > 0.0001) %>% nrow() == 0)){
    message("Good! Storage in balance across time") } else{
      warning("Stock imbalance across time or inconsistent stock variation")
    }

}


# Be careful with the unit as value / 1000 here
# Light adjustments to merge Tourist consumption into food
# And maintain balance across dimensions
SUA_bal_adjust <- function(.df){

  SCL_element_new <-
    c("Opening stocks", "Production", "Export", "Import", "Stock Variation",
      "Food", "Feed", "Seed", "Processed", "Other uses",
      "Tourist consumption", "Loss", "Residuals")

  .df %>%
    mutate(element = factor(element,levels = SCL_element_new),
           value = value / 1000) %>%  # unit: 1000 tonnes
    replace_na(list(value = 0)) %>%
    spread(element, value) %>%
    #Merge tourist consumption into food
    mutate(Food = Food + `Tourist consumption`) %>%
    select(- `Tourist consumption`) %>%
    #Maintain stock balance across time; SCL data (2010-) quality was high
    #Calculate closing stock, when negative, shift up stocks in all years.
    group_by(area_code, item_code) %>% dplyr::arrange(-year, item_code) %>%
    mutate(cumSV = cumsum(`Stock Variation`) - first(`Stock Variation`),
           `Opening stocks1` = first(`Opening stocks`) - cumSV) %>%
    select(-cumSV, -`Opening stocks`) %>%
    rename(`Opening stocks` = `Opening stocks1`) %>%
    mutate(`Closing stocks` = `Opening stocks` + `Stock Variation`) %>%
    mutate(Stockshifter = if_else(`Closing stocks` < 0, abs(`Closing stocks`), 0)) %>%
    mutate(Stockshifter = if_else(`Opening stocks` < 0 & `Opening stocks` < `Closing stocks`,
                                  abs(`Opening stocks`), Stockshifter)) %>%
    mutate(Stockshifter = max(Stockshifter),
           `Opening stocks` = `Opening stocks` + Stockshifter,
           `Closing stocks` = `Opening stocks` + `Stock Variation`) %>%
    select(-Stockshifter) %>%
    ungroup() %>%
    #Correct negative processed where applicable
    #For few regions (e.g., Congo), move residual to food so SUA data was not exist
    mutate(Processed = if_else(Processed < 0, 0, Processed),
           Food = ifelse(Production >0 & Food == 0 & Feed == 0 & Processed == 0 & Seed == 0 & `Other uses` == 0,
                         Production + Import - Export + `Stock Variation`, Food),
           Food = ifelse(Food < 0, 0, Food)) %>%
    #Check regional supply, demand, and residue
    mutate(`Regional supply` = `Opening stocks` + Production + `Import`,
           `Regional demand` = `Export` + Feed + Food + Loss + Processed + Seed + `Other uses` +`Closing stocks`,
           Residuals = `Regional supply` -  `Regional demand`) %>%
    gather(element, value, -area_code, -item_code, -year) %>%
    mutate(element = factor(element, levels = Bal_element_new))
}




#' Function saving dataset to csv file with headers
#'
#' @param gcam_dataset dataframe name to be saved
#' @param col_type_nonyear column type that is non-year; numeric for years will be pasted
#' @param title title in header
#' @param unit  unit in header
#' @param description description in header
#' @param code fao dataset domain code
#' @param out_dir output directory
#' @param GZIP IF TRUE
#'
#' @export

output_csv_data <- function(gcam_dataset, col_type_nonyear,
                            title, unit, description = NA,
                            code,
                            out_dir = out_dir,
                            GZIP = F){

  if (!missing(code)) {code = code}

  col_type = paste0(col_type_nonyear, paste0(rep("n", ncol(get(gcam_dataset, envir = parent.frame())) - nchar(col_type_nonyear)), collapse = "") )

  cmnts <- c(
    paste0("File: ", gcam_dataset, ifelse(GZIP, ".csv.gz", ".csv")),
    paste0("Title: ", title),
    paste0("Units: ", unit),
    paste0("Description:  ", description),
    paste0("Data source: FAOSTAT (main domain:", code ," FAO.udpate:",FAOSTAT_metadata(code = code)$dateupdate,")"),
    paste0("Date of CSV last update: ", Sys.Date()),
    paste0("Column types: ",col_type) ,
    "----------"
  )
  fqfn <- file.path(out_dir, paste0(gcam_dataset, ".csv"))
  suppressWarnings(file.remove(fqfn))

  if (GZIP == F) {
    cat(paste("#", cmnts), file = fqfn, sep = "\n", append = TRUE)
    readr::write_csv(get(gcam_dataset, envir = parent.frame()), fqfn, append = TRUE, col_names = TRUE, na = "")
  } else {
    cat(paste("#", cmnts), file = gzfile(paste0(fqfn, ".gz")), sep = "\n", append = TRUE)
    readr::write_csv(get(gcam_dataset, envir = parent.frame()), gzfile(paste0(fqfn, ".gz")), append = TRUE, col_names = TRUE, na = "")
  }
}



# Fn adjusting gross trade in all regions to be consistent with average (world export and import)

#' Balance gross trade
#' @description Scale gross export and import in all regions to make them equal at the world level.
#'
#' @param .DF An input dataframe with an element col including Import and Export
#' @param .MIN_TRADE_PROD_RATIO Trade will be removed if world total export or import over production is smaller than .MIN_TRADE_PROD_RATIO (1% default value)
#' @param .Reg_VAR Region variable name; default is area_code
#' @param .GROUP_VAR Group variable; default is item_code and year
#'
#' @return The same dataframe with balanced world export and import.

GROSS_TRADE_ADJUST <- function(.DF,
                               .MIN_TRADE_PROD_RATIO = 0.01,
                               .Reg_VAR = 'area_code',
                               .GROUP_VAR = c("item_code", "year")){

  # assert .DF structure
  assertthat::assert_that(all(c("element", .GROUP_VAR) %in% names(.DF)))
  assertthat::assert_that(dplyr::is.grouped_df(.DF) == F)
  assertthat::assert_that(all(c("Import", "Export", "Production") %in%
                                c(.DF %>% distinct(element) %>% pull)))

  .DF %>%
    # Join ExportScaler and ImportScaler
    left_join(
      .DF %>%
        #group_by_at(vars(all_of(.GROUP_VAR), element)) %>%
        #summarise(value = sum(value, na.rm = T), .groups = "drop") %>%
        spread(element, value) %>%
        group_by_at(vars(all_of(.GROUP_VAR))) %>%
        # filter out items with zero world trade or production
        # and replace na to zero later for scaler
        replace_na(list(Export = 0, Import = 0, Production = 0)) %>%
        filter(sum(Export) != 0, sum(Import) != 0, sum(Production) != 0) %>%
        # world trade should be later than .MIN_TRADE_PROD_RATIO to have meaningful data
        # depending on item group, .MIN_TRADE_PROD_RATIO can be set differently
        filter(sum(Export) / sum(Production) > .MIN_TRADE_PROD_RATIO) %>%
        filter(sum(Import) / sum(Production) > .MIN_TRADE_PROD_RATIO) %>%
        # finally,
        # use average gross trade value to calculate trade scaler
        # the trade scalers will be applied to all regions
        mutate(ExportScaler = (sum(Export) + sum(Import))/ 2 / sum(Export),
               ImportScaler = (sum(Export) + sum(Import))/ 2 / sum(Import)) %>%
        select(all_of(c(.Reg_VAR, .GROUP_VAR)), ExportScaler, ImportScaler) %>%
        ungroup(),
      by = c(all_of(c(.Reg_VAR, .GROUP_VAR)))) %>%
    replace_na(list(ExportScaler = 0, ImportScaler = 0)) %>%
    # If world export, import, or prod is 0, trade will be zero
    mutate(value = case_when(
      element %in% c("Export") ~ value * ExportScaler,
      element %in% c("Import") ~ value * ImportScaler,
      TRUE ~ value)) %>%
    select(-ExportScaler, -ImportScaler)

}


FAOSTAT_download_bulk <- function(DATASETCODE,
                                  DATA_FOLDER = DIR_RAW_DATA_FAOSTAT){

  assertthat::assert_that(is.character(DATASETCODE))
  assertthat::assert_that(is.character(DATA_FOLDER))


  lapply(DATASETCODE, function(d){
    metadata <- FAOSTAT_metadata(code = d)
    url_bulk = metadata$filelocation

    file_name <- basename(url_bulk)
    download.file(url_bulk, file.path(DATA_FOLDER, file_name))
  })

}


# decimal places in ggplot
scaleFUN <- function(x) sprintf("%.0f", x)

lookup <- function(.lookupvalue, .lookup_df, .lookup_col, .target_col){

  assert_that(is.character(.lookupvalue))
  assert_that(is.data.frame(.lookup_df))
  assert_that(.lookup_col %in% colnames(.lookup_df))
  assert_that(.target_col %in% colnames(.lookup_df))

  .lookup_df[grep(paste0("^",.lookupvalue,"$"),
                  .lookup_df[, .lookup_col]), .target_col]
}
