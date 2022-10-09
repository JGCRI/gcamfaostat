
# Load libs ----
library(dplyr)
library(tidyr)
library(assertthat)
require(gcamdata)

source("data-raw/generate_package_data_faostat_helper_funcs.R")

DIR_PREBUILT_FAOSTAT <- "data/PREBUILT_FAOSTAT"



DIR_PLOTS <- "figures"
dir.create(DIR_PLOTS, showWarnings = FALSE)
Hist_Year_FBS <- 2010:2019

# 1. Load and prepare data ----

## 1.1. Get mapping that indicates item Tiers and sources ----

FAO_items <- readr::read_csv(file.path("inst/extdata/aglu/FAO", "FAO_items.csv"), comment = "#")
### unique items in FAO_items
assertthat::assert_that(FAO_items %>% nrow == FAO_items %>% distinct(item, item_code) %>% nrow)

# Load raw package data
FAOSTAT_RDS <- c("QCL_PROD", "SCL", "TCL", "FBSH", "CB", "FBS", "TM_wide")
lapply(FAOSTAT_RDS, function(d){
  assertthat::assert_that(file.exists(file.path(DIR_PREBUILT_FAOSTAT, paste0(d, ".rds"))))
  assign(d, readRDS(file.path(DIR_PREBUILT_FAOSTAT, paste0(d, ".rds"))),
         envir = parent.env(environment()))
})

# Get area code in QCL that is consistent with FBS e.g., after 2010 only
QCL_PROD %>% filter(year %in% Hist_Year_FBS) %>%  distinct(area_code) %>% pull ->
  QCL_area_code_FBS


## 1.2. Get FAO supply-utilization SCL ready ----

SCL %>% distinct(element)
# Update SCL element name for convenience
SCL %>% mutate(element = gsub(" Quantity| supply quantity \\(tonnes\\)| \\(non-food\\)", "", element)) ->
  SCL
SCL_element_new <-
  c("Opening stocks", "Production", "Export", "Import", "Stock Variation",
    "Food", "Feed", "Seed", "Processed", "Other uses",
    "Tourist consumption", "Loss", "Residuals")

# SCL has all elements in SCL_element_new
assertthat::assert_that(setdiff(SCL_element_new, unique(SCL$element)) %>% length == 0)


##  1.3. Get gross trade data from bilateral (TM) ----
#  Trade is balanced if aggregating bilateral but need to filter in QCL_area_code_FBS

TM_wide %>%
  # gather years
  gcamdata::gather_years() %>% drop_na() %>%
  filter(area_code %in% QCL_area_code_FBS,
         source_code %in% QCL_area_code_FBS,
         element %in% c("Import Quantity")) %>%
  mutate(element = "Import") %>%
  group_by(area_code, item_code, element, year) %>%
  summarise(value = sum(value, na.rm = T), .groups = "drop") %>%
  bind_rows(
    TM_wide %>%
      gcamdata::gather_years() %>% drop_na() %>%
      filter(area_code %in% QCL_area_code_FBS,
             source_code %in% QCL_area_code_FBS,
             element %in% c("Import Quantity")) %>%
      mutate(element = "Export") %>%
      group_by(area_code = source_code, item_code, element, year) %>%
      summarise(value = sum(value, na.rm = T), .groups = "drop") ) -> TCL_TM
rm(TM_wide)

## 1.4. Get gross trade data from FAO gross trade (TCL) ----
TCL %>%
  filter(area_code %in% QCL_area_code_FBS,
         element %in% c("Import Quantity")) %>%
  mutate(element = "Import") %>%
  bind_rows(
    TCL %>%
      filter(area_code %in% QCL_area_code_FBS,
             element %in% c("Export Quantity")) %>%
      mutate(element = "Export") ) %>%
  select(area_code, item_code, element, year, value) -> TCL_gross
rm(TCL)

## 1.5. Get FBS data (FBS) ----
FBS %>%
  # keep only balance items
  filter(!element_code %in% c(645, 664, 674, 684)) %>%
  # simplify elements and make them consistent with SUA
  mutate(element = gsub(" Quantity| supply quantity \\(tonnes\\)| \\(non-food\\)", "", element)) %>%
  mutate(element = replace(element, element == "Losses", "Loss"),
         element = replace(element, element == "Processing", "Processed")) %>%
  # convert units back to tonnes first since FBS originally used 1000 tons
  mutate(value = value * 1000, unit = "tonnes")->
  FBS

## 1.6. Define balance elements ----
c("Opening stocks", "Production", "Import",
  "Export", "Processed", "Food", "Feed", "Seed", "Other uses", "Loss", "Closing stocks",
  "Residuals", "Regional supply", "Regional demand", "Stock Variation") ->
  Bal_element_new  # used in Get_SUA_TEMPLATE and SUA_bal_adjust

## 1.7. Filter data by year and merge regions needed----
# Merge Sudan regions to be consistent with data
# Mainly for storage data concerns
# And only keep data > min(Hist_Year_FBS)
for (.DF in c("SCL", "TCL_TM", "TCL_gross", "FBSH", "CB", "FBS", "QCL_PROD")) {
  get(.DF) %>% filter(year >= min(Hist_Year_FBS)) %>%
    # merge Sudan and South Sudan
    FAO_AREA_DISAGGREGATE_HIST_DISSOLUTION_ALL(SUDAN2012_MERGE = T) %>%
    assign(x = .DF, envir = parent.env(environment()))  }


# Update area code in QCL
QCL_PROD %>% filter(year %in% Hist_Year_FBS) %>%  distinct(area_code) %>% pull ->
  QCL_area_code_FBS


# 2. Create helper functions to simplify join by data set ----


## 2.1. FN: Get full template and fill in data by Tier ----

# A function expand.grid to get full set of element, area, year, and item (by Tier)
# The template will be joined by data from different sources later
# Note that item code is usually used to ensure a best match but item can also be used
# Note that Sudan after 2012 (South Sudan and Sudan) are merged
Get_SUA_TEMPLATE <- function(.ITEM_CODE = NULL, .ITEM = NULL, .YEAR = Hist_Year_FBS){

  #Item code will be used first if available
  if (!is.null(.ITEM_CODE)) {
    expand.grid(area_code = QCL_area_code_FBS,
                year = .YEAR,
                item_code = .ITEM_CODE,
                element = SCL_element_new) %>%
      # Adding this to fix Sudan breakup after 2010
      FAO_AREA_RM_NONEXIST(SUDAN2012_MERGE = T) -> .DF
    return(.DF)
  }

  if (!is.null(.ITEM)) {
    expand.grid(area_code = QCL_area_code_FBS,
                year = .YEAR,
                item = .ITEM,
                element = SCL_element_new) %>%
      # Adding this to fix Sudan breakup after 2010
      FAO_AREA_RM_NONEXIST(SUDAN2012_MERGE = T) -> .DF
    return(.DF)
  }

}


## 2.2. FN: Join data to template by source ----

SUA_TEMPLATE_LEFT_JOIN <- function(.DF, .DS, .DS_TM_Assert_Item = T){

  ## Start SUA_TEMPLATE_LEFT_JOIN ----

  ## a. Join QCL_PROD when .DS == "QCL" ----
  if (.DS == "QCL") {

    # assert QCL_PROD exist
    assertthat::assert_that(is.data.frame(QCL_PROD))

    # assert items exist in joined DF
    assertthat::assert_that(.DF %>% distinct(item_code) %>% pull %>%
                              setdiff(QCL_PROD %>% distinct(item_code) %>% pull) %>%
                              length() == 0 )
    # Join
    .DF %>% left_join(
      QCL_PROD %>%
        select(area_code, item_code, element, year, QCL = value),
      by = c("area_code", "item_code", "element", "year")
    ) -> .DF1
    return(.DF1)
  }


  ## b. Join TCL_TM when .DS == "TM" ----
  if (.DS == "TM") {

    # assert QCL_PROD exist
    assertthat::assert_that(is.data.frame(TCL_TM))

    if (.DS_TM_Assert_Item == T) {
      # assert items exist in joined DF
      assertthat::assert_that(.DF %>% distinct(item_code) %>% pull %>%
                                setdiff(TCL_TM %>% distinct(item_code) %>% pull) %>%
                                length() == 0 )
    }
    # Join
    .DF %>%
      left_join(
        TCL_TM %>% select(area_code, item_code, element, year, TCL = value),
        by = c("area_code", "item_code", "element", "year")
      ) -> .DF1
    return(.DF1)
  }


  ## c. Join TCL_gross when .DS == "TCL_gross" ----
  if (.DS == "TCL_gross") {

    # assert QCL_PROD exist
    assertthat::assert_that(is.data.frame(TCL_gross))

    if (.DS_TM_Assert_Item == T) {
      # assert items exist in joined DF
      assertthat::assert_that(.DF %>% distinct(item_code) %>% pull %>%
                                setdiff(TCL_gross %>% distinct(item_code) %>% pull) %>%
                                length() == 0 )
    }
    # Join
    .DF %>% left_join(
      TCL_gross %>% select(area_code, item_code, element, year, TCL_gross = value),
      by = c("area_code", "item_code", "element", "year")
    ) -> .DF1
    return(.DF1)
  }


  ## d. Join SCL when .DS == "SCL" ----
  if (.DS == "SCL") {

    # assert QCL_PROD exist
    assertthat::assert_that(is.data.frame(SCL))

    # assert items exist in joined DF
    assertthat::assert_that(
      .DF %>% distinct(item_code) %>% pull %>%
        setdiff(SCL %>% distinct(item_code) %>% pull) %>%
        length() == 0 )
    # Join
    .DF %>% left_join(
      SCL %>% select(area_code, item_code, element, year, SCL = value),
      by = c("area_code", "item_code", "element", "year")
    ) -> .DF1
    return(.DF1)
  }

  ## e. Join QCL_Cake when .DS == "QCL_Cake" ----
  if (.DS == "QCL_Cake") {

    # assert QCL_Cake exist
    assertthat::assert_that(is.data.frame(QCL_Cake))

    # assert items exist in joined DF
    assertthat::assert_that(.DF %>% distinct(item_code) %>% pull %>%
                              setdiff(QCL_Cake %>% distinct(item_code) %>% pull) %>%
                              length() == 0 )
    # Join
    .DF %>%left_join(
      QCL_Cake %>%
        select(area_code, item_code, element, year, QCL = value),
      by = c("area_code", "item_code", "element", "year")
    ) -> .DF1
    return(.DF1)
  }


  ## f. Join QCL_Coproduct when .DS == "QCL_Coproduct" ----
  if (.DS == "QCL_Coproduct") {

    # assert QCL_Coproduct exist
    assertthat::assert_that(is.data.frame(QCL_Coproduct))

    # assert items exist in joined DF
    assertthat::assert_that(.DF %>% distinct(item_code) %>% pull %>%
                              setdiff(QCL_Coproduct %>% distinct(item_code) %>% pull) %>%
                              length() == 0 )
    # Join
    .DF %>%left_join(
      QCL_Coproduct %>%
        select(area_code, item_code, element, year, value),
      by = c("area_code", "item_code", "element", "year")
    ) -> .DF1
    return(.DF1)
  }


  ## g. Join FBS when .DS == "FBS" ----
  if (.DS == "FBS") {

    # assert FBS exist
    assertthat::assert_that(is.data.frame(FBS))

    # assert items exist in joined DF
    assertthat::assert_that(.DF %>% distinct(item_code) %>% pull %>%
                              setdiff(FBS %>% distinct(item_code) %>% pull) %>%
                              length() == 0 )
    # Join
    .DF %>%left_join(
      FBS %>%
        select(area_code, item_code, element, year, value),
      by = c("area_code", "item_code", "element", "year")
    ) -> .DF1
    return(.DF1)
  }

  ## h. Join APE_live_an_MeatEQ when .DS == "APE_live_an_MeatEQ" ----
  if (.DS == "APE_live_an_MeatEQ") {

    # assert FBS exist
    assertthat::assert_that(is.data.frame(APE_live_an_MeatEQ))

    # assert items exist in joined DF
    assertthat::assert_that(.DF %>% distinct(item_code) %>% pull %>%
                              setdiff(APE_live_an_MeatEQ %>% distinct(item_code) %>% pull) %>%
                              length() == 0 )
    # Join
    .DF %>%left_join(
      APE_live_an_MeatEQ %>%
        select(area_code, item_code, element, year, value),
      by = c("area_code", "item_code", "element", "year")
    ) -> .DF1
    return(.DF1)
  }

  ## Done SUA_TEMPLATE_LEFT_JOIN ----
}


## 2.3. FN: Balance gross trade ----
# Fn adjusting gross trade in all regions to be consistent with average (world export and import)

#' Balance gross trade
#' @description Scale gross export and import in all regions to make them equal at the world level.
#' @param .DF An input dataframe with an element col including Import and Export
#' @param .MIN_TRADE_PROD_RATIO Trade will be removed if world total export or import over production is smaller than .MIN_TRADE_PROD_RATIO (1% default value)
#' @param .Reg_VAR Region variable name; default is ("area_code")
#' @param .GROUP_VAR Group variable; default is ("item_code", "year")
#' @return The same dataframe with balanced world export and import.

GROSS_TRADE_ADJUST <- function(.DF,
                               .MIN_TRADE_PROD_RATIO = 0.01,
                               .Reg_VAR = 'area_code',
                               .GROUP_VAR = c("item_code", "year")){

  # assert .DF structure
  assertthat::assert_that(all(c("element", .GROUP_VAR) %in% names(.DF)))
  assertthat::assert_that(is.grouped_df(.DF) == F)
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


# 3. Process items in FAO_items to get Balanced SUA data ----
## 3.1 Bal_new_tier1 ----
# Tier1 includes 169 items with best sources e.g. bilateral trade (TM)  prodstat (QCL) and supply-utilization-account (SCL)
#  SCL has balanced data processed by FAO but the quality was poor with low consistency

Get_SUA_TEMPLATE(.ITEM_CODE = FAO_items %>% filter(tier == 1) %>% pull(item_code)) %>%
  SUA_TEMPLATE_LEFT_JOIN("QCL") %>%
  SUA_TEMPLATE_LEFT_JOIN("TM") %>%
  SUA_TEMPLATE_LEFT_JOIN("SCL") %>%
  mutate(value = case_when(
    element %in% c("Production") ~ QCL, #prod in QCL is used and not overwritten in case_when
    element %in% c("Export", "Import") ~ TCL,
    element %in% SCL_element_new ~ SCL) ) %>%
  select(-QCL, -TCL, -SCL) %>%
  # Adjust for balance across all dimensions
  SUA_bal_adjust %>%   # Unit is converted to 1000 tonnes!
  left_join(FAO_items %>% select(item_code, item), by = "item_code") ->
  Bal_new_tier1


assert_FBS_balance(Bal_new_tier1)


## 3.2 Bal_new_tier2 ----
# Tier2 includes 139 items that had no data or low quality data in QCL so used production from SCL

Get_SUA_TEMPLATE(.ITEM_CODE = FAO_items %>% filter(tier == 2) %>% pull(item_code)) %>%
  SUA_TEMPLATE_LEFT_JOIN("TM") %>%
  SUA_TEMPLATE_LEFT_JOIN("SCL") %>%
  mutate(value = case_when(
    element %in% c("Export", "Import") ~ TCL,
    element %in% SCL_element_new ~ SCL) ) %>%
  select(-TCL, -SCL)  %>%
  SUA_bal_adjust %>%   # Unit is converted to 1000 tonnes!
  left_join(FAO_items %>% select(item_code, item), by = "item_code") ->
  Bal_new_tier2

assert_FBS_balance(Bal_new_tier2)


## 3.3 Bal_new_tier3 ----
# Tier3 includes 60 items that had QCL but no bilateral trade data
# so use gross trade from SCL

Get_SUA_TEMPLATE(.ITEM_CODE = FAO_items %>% filter(tier == 3) %>% pull(item_code)) %>%
  SUA_TEMPLATE_LEFT_JOIN("SCL")  %>%
  # light cleaning here since more missing data were seen for this group
  # set NA stock variation to zero to avoid fill NA later
  mutate(SCL = if_else(is.na(SCL) & element == "Stock Variation", 0, SCL)) %>%
  group_by(area_code, item_code, element) %>%
  #fill in NA up-down across years
  fill(SCL) %>% fill(SCL, .direction = "updown") %>% ungroup() %>%
  SUA_TEMPLATE_LEFT_JOIN("QCL") %>%
  mutate(value = case_when(
    #element %in% c("Export", "Import") & item_code == 895 ~ 0, # fix/remove trade for Milk, skimmed evaporated
    element %in% c("Production") ~ QCL, #prod in QCL is used and not overwritten by SCL
    element %in% SCL_element_new ~ SCL) ) %>%
  replace_na(list(value = 0)) %>%
  select(-QCL, -SCL) %>%
  GROSS_TRADE_ADJUST(.MIN_TRADE_PROD_RATIO = 0.01) %>%
  SUA_bal_adjust %>%   # Unit is converted to 1000 tonnes!
  left_join(FAO_items %>% select(item_code, item), by = "item_code") ->
  Bal_new_tier3

assert_FBS_balance(Bal_new_tier3)

## 3.4 Bal_new_tier4 ----
# Tier4 includes 84 items included in SCL but not in Tier1-3

Get_SUA_TEMPLATE(.ITEM_CODE = FAO_items %>% filter(tier == 4) %>% pull(item_code)) %>%
  SUA_TEMPLATE_LEFT_JOIN("SCL") %>%
  # light cleaning here since more missing data were seen for this group
  rename(value = SCL)  %>%
  mutate(value = if_else(is.na(value) & element == "Stock Variation", 0, value)) %>%
  group_by(area_code, item_code, element) %>%
  #fill in NA up-down across years
  fill(value, .direction = "updown") %>% ungroup() %>%
  replace_na(list(value = 0)) %>%
  # Gross trade is adjusted since the source was not bilateral trade
  GROSS_TRADE_ADJUST(.MIN_TRADE_PROD_RATIO = 0.01) %>%
  SUA_bal_adjust %>%   # Unit is converted to 1000 tonnes!
  left_join(FAO_items %>% select(item_code, item), by = "item_code") ->
  Bal_new_tier4

assert_FBS_balance(.DF = Bal_new_tier4)

## 3.5 Bal_new_tier5 ----
# Tier5 includes 10 oilseed cake items that were omitted in FAOSTAT
# Note  key oilseed cake data are not currently provided by FAOSTAT after 2013.
# We extrapolate the data based on oil-cake rates in 2010 to 2013

### 3.5.1 Get oil cake production data based on cake rate from CB_FBS_CakeRate ----

# Get the mapping of oil and cake items between FBSH, CB and SCL
Mapping_FBSH_SCL_OilCake <- readr::read_csv(file.path("inst/extdata/aglu/FAO", "Mapping_FBSH_SCL_OilCake.csv"), comment = "#")

# The reference period is 2011: 2013 for cake rate calculation
# Merge oil and cake data
Mapping_FBSH_SCL_OilCake %>%
  # Join oilseed oil production from FBSH in the reference periods
  left_join(FBSH %>% filter(year %in% 2011:2013, element == "Production") %>%
              transmute(area_code, FBSH_item_oil = item, year, oil = value),
            by = "FBSH_item_oil") %>%
  # Join oilseed cake production from CB
  left_join(CB %>% filter(year %in% 2011:2013, element == "Production") %>%
              # Convert unit for consistency to Kton
              transmute(area_code, FBSH_item_cake = item, year, cake = value /1000),
            by = c("FBSH_item_cake", "area_code", "year")) %>%
  # Sum across time
  group_by_at(vars(-year, -oil, -cake)) %>%
  summarise(oil = sum(oil, na.rm = T),
            cake = sum(cake, na.rm = T), .groups = "drop") ->
  FBSH_SCL_OilCake
# Calculate cake rate
FBSH_SCL_OilCake %>% mutate(cake_rate = cake / oil) %>%
  select(area_code, item = SCL_item_oil, cake_item = SCL_item_cake, cake_rate) %>%
  # Join world average rates
  left_join(
    FBSH_SCL_OilCake %>%
      group_by_at(vars(-area_code, -oil, -cake)) %>%
      summarise(oil = sum(oil, na.rm = T),
                cake = sum(cake, na.rm = T), .groups = "drop") %>%
      transmute(item = SCL_item_oil, cake_rate_world = cake / oil),
    by = "item"
  ) %>%
  # Use world average to fill in NA
  # No need to complete area_code here since world average is kept in the data
  mutate(cake_rate = if_else(cake_rate == 0 | is.finite(cake_rate) == F,
                             cake_rate_world, cake_rate)) ->
  CB_FBS_CakeRate

# Get the production of the corresponding oil items from processed Tier 1, 2, and 4
Bal_new_tier1 %>%
  bind_rows(Bal_new_tier2) %>%
  bind_rows(Bal_new_tier4) %>%
  select(area_code, year, element, item_oil = item, value) %>%
  filter(element == "Production") %>%
  mutate(value = value * 1000) %>%  # convert units back to tonne!!!
  # Join to keep Tier 5 items
  right_join(FAO_items %>% filter(tier == 5) %>% select(item_code, item, item_oil = DS_key_coproduct_item),
             by = "item_oil") %>%
  # Join cake rate
  left_join(CB_FBS_CakeRate %>% select(area_code, cake_rate, cake_rate_world, item_oil = item),
            by = c("area_code", "item_oil")) %>%
  # fill world same rate for all areas here
  group_by(item_code) %>%
  fill(cake_rate_world, .direction = "updown") %>%
  ungroup() %>%
  # and world rate when regional rate is not available
  mutate(cake_rate = if_else(is.na(cake_rate), cake_rate_world, cake_rate)) %>%
  transmute(area_code, year, item_code, element, value = value * cake_rate) ->
  QCL_Cake

### 3.5.2 Process to get Bal_new_tier5 ----
Get_SUA_TEMPLATE(.ITEM_CODE = FAO_items %>% filter(tier == 5) %>% pull(item_code)) %>%
  SUA_TEMPLATE_LEFT_JOIN("TM", .DS_TM_Assert_Item = F) %>%
  SUA_TEMPLATE_LEFT_JOIN("QCL_Cake") %>%
  mutate(value = case_when(
    element %in% c("Export", "Import") & item_code == 341 ~ 0, # Cake, others no TM
    element %in% c("Export", "Import") ~ TCL,
    element %in% c("Production") ~ QCL,
    element %in% SCL_element_new ~ 0) ) %>%
  select(-TCL, -QCL) %>%
  replace_na(list(value = 0)) %>%
  spread(element, value) %>%
  # Allocate all demostic demand to feed
  mutate(Feed = if_else(Production + Import - Export > 0,
                        Production + Import - Export, 0)) %>%
  gather(element, value, -area_code, -year, -item_code) %>%
  SUA_bal_adjust %>%  # Unit is converted to 1000 tonnes!
  left_join(FAO_items %>% select(item_code, item), by = "item_code") ->
  Bal_new_tier5
assert_FBS_balance(.DF = Bal_new_tier5)
rm(QCL_Cake, CB_FBS_CakeRate, FBSH_SCL_OilCake, Mapping_FBSH_SCL_OilCake)

## 3.6 Bal_new_tier6 ----
# Tier6 includes 29 items that included in QCL for production but not in Tier1 to Tier5

Get_SUA_TEMPLATE(.ITEM_CODE = FAO_items %>% filter(tier == 6) %>% pull(item_code)) %>%
  SUA_TEMPLATE_LEFT_JOIN("QCL") %>%
  SUA_TEMPLATE_LEFT_JOIN("TM", .DS_TM_Assert_Item = F) %>%
  SUA_TEMPLATE_LEFT_JOIN("TCL_gross", .DS_TM_Assert_Item = F) %>%
  mutate(TCL = if_else(is.na(TCL), TCL_gross, TCL)) %>%
  mutate(value = case_when(
    element %in% c("Area harvested", "Production") ~ QCL, #prod in QCL is used and not overwritten
    element %in% c("Export", "Import") ~ TCL,
    element %in% SCL_element_new ~ 0) ) %>%
  select(-QCL, -TCL, -TCL_gross) %>%
  replace_na(list(value = 0)) %>%
  GROSS_TRADE_ADJUST(.MIN_TRADE_PROD_RATIO = 0.01) %>%
  spread(element, value) %>%
  # Processing to add demand based on DS_demand in FAO_items
  # Only an exclusive use is assumed
  mutate(Processed = ifelse(item_code %in% c(FAO_items %>%
                                               filter(tier == 6, grepl("Processed", DS_demand)) %>%
                                               pull(item_code) ) & (Production + Import - Export) > 0,
                            (Production + Import - Export), 0),
         Food = ifelse(item_code %in% c(FAO_items %>%
                                          filter(tier == 6, grepl("Food", DS_demand)) %>%
                                          pull(item_code) ) & (Production + Import - Export) > 0,
                       (Production + Import - Export), 0),
         `Other uses` = ifelse(item_code %in% c(FAO_items %>%
                                                  filter(tier == 6, grepl("Other", DS_demand)) %>%
                                                  pull(item_code) ) & (Production + Import - Export) > 0,
                               (Production + Import - Export), 0)) %>%
  gather(element, value, -area_code, -item_code, -year) %>%
  SUA_bal_adjust %>%  # Unit is converted to 1000 tonnes!
  left_join(FAO_items %>% select(item_code, item), by = "item_code") ->
  Bal_new_tier6
assert_FBS_balance(.DF = Bal_new_tier6)


## 3.7 Bal_new_tier7 ----
#Tier7 includes 12 fish items from FBS and FBSH. Item code came from FBS as well

Get_SUA_TEMPLATE(.ITEM_CODE = FAO_items %>% filter(tier == 7) %>% pull(item_code)) %>%
  SUA_TEMPLATE_LEFT_JOIN("FBS") %>%
  mutate(value = if_else(is.na(value) & element == "Stock Variation", 0, value)) %>%
  group_by(area_code, item_code, element) %>%
  #fill NA up-down across year
  fill(value, .direction = "updown") %>%
  ungroup() %>%
  replace_na(list(value = 0)) %>%
  GROSS_TRADE_ADJUST(.MIN_TRADE_PROD_RATIO = 0.01) %>%
  SUA_bal_adjust %>%  # Unit is converted to 1000 tonnes!
  left_join(FAO_items %>% select(item_code, item), by = "item_code") ->
  Bal_new_tier7

assert_FBS_balance(.DF = Bal_new_tier7)


## 3.8 Bal_new_tier8 ----
# Tier8 include 11 cake or fiber items that coproduced in oil crop crushing. Item code used 0
#  Note the FAOSTAT does not provide data for Tier8 item so they are generated based extraction rate and assumed no trade and single use.

### 3.8.1 Get production of the main product and process coproduction ----
# Get the production of the corresponding coproducing items from processed Tier 1, 2, 4 and 5
Bal_new_tier1 %>%
  bind_rows(Bal_new_tier2) %>%
  bind_rows(Bal_new_tier4) %>%
  bind_rows(Bal_new_tier5) %>%
  select(area_code, year, element, coproduct_item = item, value) %>%
  filter(element == "Production") %>%
  mutate(value = value * 1000) %>%  # convert units back to tonne!!!
  # Join to keep Tier 8 items
  right_join(FAO_items %>% filter(tier == 8) %>%
               # Get co-production rate from DS_production which is uniform across regions
               mutate(CoproductRate = as.numeric(gsub("Coproduction_Rate \\(|)","", DS_production))) %>%
               select(item_code, item, coproduct_item = DS_key_coproduct_item, CoproductRate),
             by = "coproduct_item") %>%
  transmute(area_code, year, item_code, item, element, value = value * CoproductRate) ->
  QCL_Coproduct


### 3.8.2 Process to get Bal_new_tier8 ----
Get_SUA_TEMPLATE(.ITEM_CODE = FAO_items %>% filter(tier == 8) %>% pull(item_code)) %>%
  SUA_TEMPLATE_LEFT_JOIN("QCL_Coproduct") %>%
  replace_na(list(value = 0)) %>%
  spread(element, value) %>%
  # Processing to add demand based on DS_demand in FAO_items
  # Only an exclusive use is assumed
  mutate(Feed = ifelse(item_code %in% c(FAO_items %>% filter(tier == 8, grepl("Feed", DS_demand)) %>%
                                          pull(item_code) ) & Production > 0,
                       Production, 0),
         `Other uses` = ifelse(item_code %in% c(FAO_items %>% filter(tier == 8, grepl("Other", DS_demand)) %>%
                                                  pull(item_code) ) & Production > 0,
                               Production, 0) ) %>%
  gather(element, value, -area_code, -item_code, -year) %>%
  SUA_bal_adjust %>%  # Unit is converted to 1000 tonnes!
  left_join(FAO_items %>% select(item, item_code), by = "item_code") ->
  Bal_new_tier8

assert_FBS_balance(.DF = Bal_new_tier8)
rm(QCL_Coproduct)

## 3.9 Bal_new_tier9 ----
# Tier9 includes 16 meat equivalent items converted from live animals. Item code used 0
#  Note that these items were preprocessed based on mainly carcass yield implied by data for adjusting production and stocks
#  Note that for meat equivalent items only stock variation was accounted since 2010 when the data first available

### 3.9.1 Process the live animal meat equivalent data APE_live_an_MeatEQ ----

# read in QCL_AN_LIVEANIMAL_MEATEQ live animal meat equivalent
# Treat live animal as stock and adjust using production or other demand
# Milk cattle is not included
# Note that only stock variation is used
# Because live animal stock is a capital stock included elsewhere
# But accounting delta allows more accurate estimate of feed uses
# E.g., additional feed demand due to animal expansion

readRDS(file.path(DIR_PREBUILT_FAOSTAT,"QCL_AN_LIVEANIMAL_MEATEQ.rds")) -> QCL_AN_LIVEANIMAL_MEATEQ

QCL_AN_LIVEANIMAL_MEATEQ %>%
  select(area_code, item_code, year, value) %>%
  #mutate(item = gsub("Meat", "AnMeatEq", item)) %>%
  # convert units back to tonne!!! And adjust item_code
  mutate(value = value * 1000, unit = "tonnes",
         item_code = item_code * 10000) %>%
  group_by(item_code, area_code) %>%
  mutate(`Opening stocks` = value,
         `Closing stocks` = lead(value)) %>%
  # fill down missing Closing stocks due to lead value
  fill(`Closing stocks`) %>% ungroup() %>%
  # Adjust using meatEQ production or other uses
  mutate(`Stock Variation` = `Closing stocks` - `Opening stocks`,
         Production = if_else(`Stock Variation` > 0, `Stock Variation`, 0),
         `Other uses` = if_else(`Stock Variation` < 0, -`Stock Variation`, 0)
  ) %>% ungroup() %>%
  gather(element, value, c("Opening stocks", "Closing stocks", "Production", "Other uses")) %>%
  transmute(area_code, year, item_code, element, value) ->
  APE_live_an_MeatEQ

### 3.9.2 Process to get Bal_new_tier9 ----
Get_SUA_TEMPLATE(.ITEM_CODE = FAO_items %>% filter(tier == 9) %>% pull(item_code)) %>%
  SUA_TEMPLATE_LEFT_JOIN("APE_live_an_MeatEQ") %>%
  spread(element, value) %>%
  # only keep net openning stock in the study period
  group_by(item_code, area_code) %>%
  mutate(`Opening stocks` = `Opening stocks` - min(`Opening stocks`)) %>%
  ungroup() %>%
  gather(element, value, -area_code, -item_code, -year) %>%
  SUA_bal_adjust %>%  # Unit is converted to 1000 tonnes!
  left_join(FAO_items %>% select(item, item_code), by = "item_code") ->
  Bal_new_tier9

assert_FBS_balance(.DF = Bal_new_tier9)
rm(QCL_AN_LIVEANIMAL_MEATEQ, APE_live_an_MeatEQ)


# 4. Bind all to get Bal_new_all ----
lapply(paste0("Bal_new_tier", 1:9), get) %>% bind_rows() %>%
  # Add area_code
  left_join(QCL_PROD %>% distinct(area, area_code), by = "area_code") ->
  Bal_new_all

assert_FBS_balance(.DF = Bal_new_all)

rm(TCL_gross, TCL_TM, SCL, FBS, FBSH, CB, FAO_items)
rm(list = ls(pattern = "Bal_new_tier*"))

# 5. quick check and save ----
library(ggplot2)

FAOSTAT_check_count_plot(Bal_new_all) -> p; p #

ggsave(file.path(DIR_PLOTS, "SCL_Bal_new_all.png"),
       plot = p + scale_y_continuous(labels=scaleFUN) +
         scale_x_continuous(labels=scaleFUN) +
         ggtitle("gcamdata-FAOSTAT (SCL & FBS) new supply-utilization data over time"),
       dpi = 200, width = 9, height = 5 ); rm(p)



Bal_new_all %>%
  spread(year, value) ->
  GCAMDATA_FAOSTAT_SUA_195Regs_530Items_2010to2019

saveRDS(GCAMDATA_FAOSTAT_SUA_195Regs_530Items_2010to2019,
        file.path(DIR_PREBUILT_FAOSTAT,
                  "GCAMDATA_FAOSTAT_SUA_195Regs_530Items_2010to2019.rds"))

DIR_FAO_CSV <- "inst/extdata/aglu/FAO"

output_csv_data("GCAMDATA_FAOSTAT_SUA_195Regs_530Items_2010to2019",
                col_type_nonyear = "iiccc",
                title = "Supply_utilization_accounting for all FAO items in 2010 to 2019",
                unit = "1000 tonnes", code = "SCL",
                description = "Data is compiled and generated by gcamdata-FAOSTAT. Data is balanced in trade, supply_utilization, and storage",
                out_dir = DIR_FAO_CSV, GZIP = T)




## Consistency in production data----
assertthat::assert_that(
  QCL_PROD %>% filter(element == "Production", year <= 2019) %>%
    transmute(area_code, item_code, year, value_prod = value /1000) %>%
    left_join_error_no_match(Bal_new_all %>% filter(element == "Production"),
                             by = c("area_code", "item_code", "year")) %>%
    filter(abs(value_prod - value) >1) %>%
    # soy oil from other data see appendix 1
    filter(!item_code %in% c(237),
           # Sudan(former) has a filled fish item not in QCL_PROD for 2010 -2011
           !area_code %in% c(206)) %>% nrow() == 0
)
# Note that QCL_PROD does not include Live animal Meat EQ

rm(QCL_PROD)


#***************************************************************
# Well done ----

# P.S. Amendments log ----

# 1. Note that SCL is now used for prod of "Oil, soybean" 237;
#   Brazil 2017 - 2018 soy oil production data were wrong in QCL; need to change to SCL;
#   *cake/oil/substract rates would be affected otherwise
#   This was done in SUA_tier1_item processing (case_when)

# 2. Bilateral trade items
#   Three items only have bilateral trade (TM) before 2009
#   Including "Kapokseed in shell", "Ghee, buffalo milk", "Milk, whole fresh sheep"
#   With code (982, 953, 311)
#   Two more from Tier2 (313, 297) moved to Tier3
#
# 3. Area
#   Sudan South and Sudan are merged after 2012 So storage adjustments are consistent



