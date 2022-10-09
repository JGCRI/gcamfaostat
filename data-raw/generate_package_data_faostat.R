
# This script has two objectives:
# 1. Download FAOSTAT data from API
# 2. Read and generate FAOSTAT package data. Save data to RDS files

# Load libs ----
library(dplyr)
library(tidyr)

source("data-raw/generate_package_data_faostat_helper_funcs.R")

# Dir for FAOSTAT raw data ----
DIR_RAW_DATA_FAOSTAT <- "inst/extdata/aglu/FAO/FAOSTAT"
DIR_PREBUILT_FAOSTAT <- "data/PREBUILT_FAOSTAT"
dir.create(DIR_RAW_DATA_FAOSTAT, showWarnings = FALSE)
dir.create(DIR_PREBUILT_FAOSTAT, showWarnings = FALSE)




# ********************************----

# Download fao_metadata and set dataset_code ----

# Data code needed ----
FAO_dataset_code_required <-
  c("QCL",          # Ag production quantity and harvested area
    "PP",   "PD",   # Producer prices and regional deflator
    "TCL",  "TM",   # Gross and bilateral trade
    "SCL",          # Supply utilization accounting
    "FBS",  "FBSH", # New and old food balance sheet
    "CB",           # Old non food utilization accounting
    #"RFN",         # Fertilizer by nutrient
    #"RL",          # Land Use
    #"FO"           # Forest production and trade
    "OA"            # Population
  )


# Save a table includes all FAOSTAT data info and links
fao_metadata <- FAOSTAT_metadata() %>% filter(datasetcode %in% FAO_dataset_code_required)
readr::write_csv(fao_metadata, file.path(DIR_PREBUILT_FAOSTAT, "FAOSTAT_METADATA.csv"))

# All dataset will be downloaded
# warning: existing data will be overwritten
FAOSTAT_download_bulk(FAO_dataset_code_required, DATA_FOLDER = DIR_RAW_DATA_FAOSTAT)

rm(fao_metadata, FAO_dataset_code_required)



# ********************************----
# Preprocessing and save RDS ----
# Code below read downloaded csv files
# And preprocess to clean aggregated regions, keep only needed elements
# light missing-fill (e.g., for prices)
# Note that FAOSTAT recently added alternative item and area codes
# But stay with FAO codes for the processing here



# *[QCL] Production and area ----

FAOSTAT_load_raw_data("QCL")


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


# Other data uses OCL area for consistency
QCL_area_code <- QCL1 %>% distinct(area_code) %>% pull()

# save and clean
saveRDS(QCL1, file.path(DIR_PREBUILT_FAOSTAT, "QCL.rds")); rm(QCL, QCL1)


# *[PP] Producer price ----

FAOSTAT_load_raw_data("PP")
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
  select(area_code, area, item_code, item, element_code, element, year, value, unit) %>%
  # Not completing year and area here
  spread(element, value) %>%
  left_join(
    PP1 %>% filter(element_code %in% c(5532, 5539)) %>%
      select(area_code, area, item_code, item, element_code, element, year, value, unit) %>%
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


# save and clean
saveRDS(PP2, file.path(DIR_PREBUILT_FAOSTAT,"PP.rds")); rm(PP, PP1, PP2)



# *[PD] FAO_GDP_deflators ----

FAOSTAT_load_raw_data("PD")
# read in Taiwan values as FAO does not have Taiwan price data
GDP_deflator_Taiwan <-
  readr::read_csv(file.path(DIR_RAW_DATA_FAOSTAT,
                            "Other_supplementary/GDP_deflator_Taiwan.csv"), comment = "#")
PD %>% distinct(element, element_code, unit)
PD %>% distinct(item, item_code)


PD %>% filter(area_code < 350,
              item_code == 22024,
              element_code == 6179) %>% #keep US$
  rm_accent("item", "area") -> PD1


PD2 <-
  PD1 %>%
  filter(item == "GDP Deflator", grepl("US\\$", element) ) %>%
  select(area, area_code, item,item_code, element, element_code, year, value) %>%
  bind_rows(GDP_deflator_Taiwan %>%
              mutate(area = "China, Taiwan Province of",
                     area_code = 214,
                     item = "GDP Deflator",
                     item_code = 22024,
                     element = "Value US$, 2015 prices",
                     element_code = 6179,
                     value = round(100 * value / value[year == 2015], 2)) )


# save and clean
saveRDS(PD2, file.path(DIR_PREBUILT_FAOSTAT,"PD.rds")); rm(PD1, PD, PD2, GDP_deflator_Taiwan)



# Food balance and Supply-Utilization-Account

## *[FBS] new food balance sheet (2010-) ----

  FAOSTAT_load_raw_data("FBS") # New FBS  2010+
  FBS %>% distinct(element, element_code, unit)
  FBS %>% filter(item_code < 2901, item_code != 2501,
                 !element_code %in% c(511, 5301),
                 area_code %in% QCL_area_code) %>%
    select(area_code, area, item_code, item, element_code, element, year, value, unit) %>%
    rm_accent("item", "area") -> FBS1

  # save and clean
  saveRDS(FBS1, file.path(DIR_PREBUILT_FAOSTAT,"FBS.rds"));rm(FBS, FBS1)


## *[SCL] SUA: supply utilization accounting ----

  FAOSTAT_load_raw_data("SCL")   # SUA      2010+
  SCL %>% distinct(element, element_code, unit)
  # FAOSTAT accidentally used CPC code in SCL; safeguard here
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

  # save and clean
  saveRDS(SCL1, file.path(DIR_PREBUILT_FAOSTAT,"SCL.rds")); rm(SCL, SCL1)


  ## *[FBSH] old food balance sheet (-2013) ----

  FAOSTAT_load_raw_data("FBSH")  # Old FBS -2013
  FBSH %>% distinct(element, element_code, unit)
  # Keep population (old)
  FBSH %>% filter(item_code < 2901,
                  !element_code %in% c(5301),
                  area_code %in% QCL_area_code) %>%
    select(area_code, area, item_code, item, element_code, element, year, value, unit) %>%
    rm_accent("item", "area") -> FBSH1

  # save and clean
  saveRDS(FBSH1, file.path(DIR_PREBUILT_FAOSTAT,"FBSH.rds")); rm(FBSH, FBSH1)


  ## *[CB] Non-food Balance ----

  FAOSTAT_load_raw_data("CB")    # Old FBS-nonfood -2013
  CB %>% distinct(element, element_code, unit)
  # Keep population (old)
  CB %>% filter(item_code < 2901,
                !element_code %in% c(5300),
                area_code %in% QCL_area_code) %>%
    select(area_code, area, item_code, item, element_code, element, year, value, unit) %>%
    rm_accent("item", "area") -> CB1
  # save and clean
  saveRDS(CB1, file.path(DIR_PREBUILT_FAOSTAT,"CB.rds")); rm(CB, CB1)


  ## *FBSH_CB merge the two----
  # load processed data
  readRDS(file.path(DIR_PREBUILT_FAOSTAT,"CB.rds")) -> CB
  readRDS(file.path(DIR_PREBUILT_FAOSTAT,"FBSH.rds")) -> FBSH

  FBSH %>% distinct(item_code) %>% intersect(
    CB %>% distinct(item_code)
  ) -> dup_item_code


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
    FBSH_CB

  # save and clean
  saveRDS(FBSH_CB, file.path(DIR_PREBUILT_FAOSTAT, "FBSH_CB.rds"));
  rm(SHELL_RATE_groundnuts, Mill_RATE_rice); rm(FBSH_CB, FBSH, CB, dup_item_code)


  # *[OA]: Population ----

  FAOSTAT_load_raw_data("OA")    # Population
  OA %>% distinct(element, element_code)
  OA %>% distinct(item, item_code)

  OA %>% filter(element_code == 511, item_code == 3010)  %>%
    select(area_code, area, item_code, item, element_code, element, year, value, unit) %>%
    rm_accent("item", "area") -> OA1

  # save and clean
  saveRDS(OA1, file.path(DIR_PREBUILT_FAOSTAT,"OA.rds")); rm(OA, OA1)


  # *[TCL] Gross trade ----
  FAOSTAT_load_raw_data("TCL")   # Gross trade

  TCL %>% distinct(element, element_code, unit)
  TCL %>% distinct(item, item_code)

  TCL %>%
    filter(item_code <= 1700,
           # only keep quantity
           !element_code %in% c(5622 , 5922),
           area_code %in% QCL_area_code) %>%
    select(area_code, area, item_code, item, element_code, element, year, value, unit) %>%
    rm_accent("item", "area") -> TCL1

  # save and clean
  saveRDS(TCL1, file.path(DIR_PREBUILT_FAOSTAT,"TCL.rds")); rm(TCL, TCL1)



# *[TM] Bilateral trade ----
#*FAO has better quality bilateral data since 1992, covering most SUA items
  FAOSTAT_load_raw_data("TM")    # Bilateral trade



TM %>%
  # Only keep quantities for elements with a unit of tonnes
  filter(element_code %in% c(5910, 5610),
         item_code < 1700,
         # Bilateral trade year starts from 1986 but higher quality after 1992
         # Subset data also to shrink the size
         year >= 1992,
         partner_country_code %in% QCL_area_code,
         reporter_country_code %in% QCL_area_code) %>%
  select(reporter_country_code, reporter_countries,
         partner_country_code, partner_countries,
         item_code, item, element_code, element, year, value, unit)  ->
  TM1
rm(TM)


## **Reconcile export and import bilateral flow ----
# Full join export and import and use available import to fill missing and zero export
TM1 %>% filter(element %in% c("Export Quantity")) %>% spread(element, value) %>%
  select(exporter = reporter_country_code,
         importer = partner_country_code, item_code, year, expflow = `Export Quantity`) %>%
  full_join(
    TM1 %>% filter(element %in% c("Import Quantity")) %>% spread(element, value)%>%
      select(importer = reporter_country_code,
             exporter = partner_country_code, item_code, year, impflow = `Import Quantity`),
    by = c("exporter", "importer", "item_code", "year")
  )  %>%
  # replace na with zero but use import to replace zero export later
  replace_na(list(expflow = 0, impflow = 0)) %>%
  transmute(area_code = importer, year, item_code, source_code = exporter,
            value = if_else(expflow == 0, impflow, expflow)) %>%
  mutate(element = "Import Quantity") ->
  TM2


TM2 %>%
  # remove self-trade (per unaggregated area_code) which existed in FAO TM importing data and likely due to data processing mistakes.
  filter(area_code != source_code) %>%
  left_join(TM1 %>% distinct(item, item_code), by = c("item_code")) %>%
  left_join(TM1 %>% distinct(area = partner_countries, area_code = partner_country_code), by = c("area_code")) %>%
  left_join(TM1 %>% distinct(source = partner_countries, source_code = partner_country_code), by = c("source_code")) %>%
  rm_accent("item", "area", "source") %>%
  mutate(unit = "tonnes") ->
  TM3
rm(TM1, TM2)

TM3 %>% spread(year, value) -> TM4


# save and clean
saveRDS(TM4, file.path(DIR_PREBUILT_FAOSTAT,"TM_wide.rds")); rm(TM3, TM4)
rm(QCL_area_code)

# ********************************----
# Well done ----
