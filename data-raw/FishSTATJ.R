

FAOSTAT_load_raw_data(DATASETCODE = "RL", .Envir = Curr_Envir)


DIR_RAW_DATA_FAOSTAT <- "FishSTAT"

DATA_FOLDER = file.path("inst/extdata", DIR_RAW_DATA_FAOSTAT)

metadata <- FAOSTAT_metadata()
metadata %>% filter(datasetcode == "FO") -> metadata1


"Aquaculture_2024.1.0.zip" -> DF_Aq

zip_file_name <- file.path(DATA_FOLDER, DF_Aq)
assertthat::assert_that(file.exists(zip_file_name))
# assuming the csv in zip has the same base name
csv_file_name <- gsub(".zip$", ".csv", basename(zip_file_name))



csv_file_name <- "Aquaculture_Quantity.csv"

df <- readr::read_csv(unz(zip_file_name, csv_file_name), col_types = NULL)
# Lower case col names and use _ as delimiter
names(df) <- tolower(gsub("\\.| ", "_", names(df)))
# Assigned to .Envir
assign(paste(CODE, GET_MAPPINGCODE, sep = "_"), df, envir = .Envir)


df %>% distinct(area_code)
summary(df)


# There is no stable link (API) for downloading data from FAO FishSTAT.
# We we include data from FishSTAT in archived raw data and prebuilt data (when processed)
# but not in the function for driectly downloading.
# Also, they are not included in gcamfaostat_metadata()

# Load data from raw


# A function to load raw data downloaded from FishSTAT (FAO) ----
# This function load data stored in "extdata/FishSTAT" by code
# We have created a metadata table to summarize the data needed/available in the function

DIR_RAW_DATA_FISHSTAT <- "FishSTAT"

FISHSTAT_load_raw_data <- function(DATASETCODE,
                                   DATA_FOLDER = file.path("inst/extdata", DIR_RAW_DATA_FISHSTAT),
                                   .Envir = NULL){

  assertthat::assert_that(is.character(DATASETCODE))
  assertthat::assert_that(is.character(DATA_FOLDER))

  if (is.null(.Envir)) {.Envir = .GlobalEnv}

  ## A "metadata" is included here; there can be combined with the FAOSTAT metadata later ----

  data.frame(
    zip_file_name = paste0(c("Aquaculture_2024.1.0", "Aquaculture_2024.1.0", "Aquaculture_2024.1.0", "Aquaculture_2024.1.0", "Aquaculture_2024.1.0",
                             "Capture_2024.1.0",
                             "FI_Trade_2024.1.0",  "FI_Trade_2024.1.0", "FI_Trade_2024.1.0",
                             "FI_Trade_Partners_2024.1.0", "FI_Trade_Partners_2024.1.0",
                             "FI_Trade_PP_2024.1.0"
    ), ".zip"),
    csv_file_name = paste0(c("Aquaculture_Quantity", "Aquaculture_Value", "CL_FI_COUNTRY_GROUPS", "CL_FI_SPECIES_GROUPS", "CL_FI_WATERAREA_GROUPS",
                             "Capture_Quantity",
                             "TRADE_QUANTITY", "TRADE_VALUE", "CL_FI_COMMODITY_ISSCFC",
                             "TRADE_PARTNERS_QUANTITY", "TRADE_PARTNERS_VALUE",
                             "TRADE_PP_QUANTITY"
    ), ".csv"),
    datasetcode = c("QCL_aquaculture_quantity", "QCL_aquaculture_value", "QCL_CountryCode", "QCL_SpecieCode", "QCL_WaterAreaCode",
                    "QCL_capture_quantity",
                    "TCL_fish_quantity", "TCL_fish_value", "TCL_CommodityCode",
                    "TM_fish_quantity", "TM_fish_value",
                    "QCL_fishproc_quantity"
                    )
  ) -> metadata


  for (CODE in DATASETCODE) {

    metadata %>% filter(datasetcode == CODE) -> metadata1
    assertthat::assert_that(nrow(metadata1) > 0,
                            msg = paste0("Dataset code,", CODE, " doesn't exist in metadata!"))

    zip_file_name <- file.path(DATA_FOLDER, basename(metadata1$zip_file_name))
    assertthat::assert_that(file.exists(zip_file_name))
    csv_file_name <-  basename(metadata1$csv_file_name)

    df <- readr::read_csv(unz(zip_file_name, csv_file_name), col_types = NULL)
    # Lower case col names and use _ as delimiter
    names(df) <- tolower(gsub("\\.| ", "_", names(df)))
    # Assigned to .Envir
    assign(CODE, df, envir = .Envir)
  }

}

# Key mappings ----

file.path("FAOSTAT", "Mapping_gcamdata_FAO_iso_reg") %>%
  load_from_cache() %>% first %>%
  distinct(area_code, iso, FAO_country) ->
  Mapping_gcamdata_FAO_iso_reg
# note: Romania can be rou or rom for iso code
# Tanzania: eaz is a part of tza, but might be separated in stat.
# sun is USSR


## Load prepared species/group ISSCAAP mapping ----
readr::read_csv("inst/extdata/FishSTAT/Mapping_commodity_ISSCAAP.csv", comment = "#") ->
  Mapping_commodity_ISSCAAP


## Load and prepare species code to isscaap mapping ----
FISHSTAT_load_raw_data(DATASETCODE = "QCL_SpecieCode")
QCL_SpecieCode %>%
  distinct(species_code = `3a_code`, isscaap = isscaap_group_en) ->
  QCL_SpecieCode_isscaap

## Load and prepare UN country code to iso mapping ----
FISHSTAT_load_raw_data(DATASETCODE = "QCL_CountryCode")
QCL_CountryCode %>%
  distinct(un_code, iso = tolower(iso3_code), FAO_area_code = identifier) %>%
  filter(FAO_area_code < 350)->
  QCL_CountryCode_iso

## Load and prepare water area code ----
FISHSTAT_load_raw_data(DATASETCODE = "QCL_WaterAreaCode")
QCL_WaterAreaCode %>%
  distinct(water_area_code = code, water_area = name_en) %>%
  mutate(Inland_Marine = if_else(grepl("Inland", water_area), "Inland", "Marine")) ->
  QCL_WaterAreaCode_water

# Production ----

## Aquaculture ----

# QCL_aquaculture includes production in both quantity (tonnes) and values (nominal 1000 USD)

FISHSTAT_load_raw_data(DATASETCODE = "QCL_aquaculture_quantity")
FISHSTAT_load_raw_data(DATASETCODE = "QCL_aquaculture_value")

# connect the two and check
QCL_aquaculture_quantity %>%
  bind_rows(QCL_aquaculture_value) %>%
  transmute(un_code = country_un_code, water_area_code = area_code,
            year = period, environment = environment_alpha_2_code,
            species_code = species_alpha_3_code, measure, value) ->
  QCL_aquaculture_0

# data is pretty clean; all (most) values have conrresponding quantity for years after 1984
# no value before 1984
# a few na in value, but because quantity is zero; those are cleaned

QCL_aquaculture_0 %>%
  spread(measure, value) %>%
  # cleaning na values
  mutate(V_USD_1000 = if_else(Q_tlw == 0, 0, V_USD_1000)) %>%
  mutate(Q_na = if_else(is.na(Q_tlw), 1, 0),
         V_na = if_else(is.na(V_USD_1000), 1, 0)) %>%
  group_by(year) %>%
  summarize(n_total = n(),
            n_Q_na = sum(Q_na),
            n_V_na = sum(V_na)) -> check_count


QCL_aquaculture_0 %>%
  spread(measure, value) %>%
  # cleaning na values
  mutate(V_USD_1000 = if_else(Q_tlw == 0, 0, V_USD_1000)) %>%
  gather(measure, value, Q_tlw, V_USD_1000) %>%
  # remove na value where values were not available for before 1984
  filter(!is.na(value))->
  QCL_aquaculture_1

### more checks in the data ----

### water area ----
QCL_aquaculture_1 %>%
  left_join_error_no_match(QCL_WaterAreaCode_water, by = "water_area_code") %>%
  group_by(Inland_Marine, environment, measure, year) %>%
  summarize(value = sum(value), .groups = "drop") %>%
  filter(measure == "Q_tlw") %>%
  filter(year == 2022)
# not sure whe there are values (small) of MA (environment = marine) when
# area is Inland; Ocean but environment = IN (freshwater) is tiny
# We will priortize water_area_code for this definition as we won't need BW (Brackish water) at the moment
# so environment is dropped

### species and isscaap ----
QCL_aquaculture_1 %>%
  left_join_error_no_match(QCL_WaterAreaCode_water, by = "water_area_code") %>%
  left_join_error_no_match(QCL_SpecieCode_isscaap, by = "species_code") %>%
  group_by(Inland_Marine, isscaap, measure, year) %>%
  summarize(value = sum(value), .groups = "drop") %>%
  filter(measure == "Q_tlw") %>% filter(year >= 2015) %>%
  spread(year, value)
# species is tricky as it is more discontinous over time
# even aggregated to isscaap, there are still missing values in some years for some groups
# We will further aggregate them for our modeling

QCL_aquaculture_1 %>%
  left_join_error_no_match(QCL_WaterAreaCode_water, by = "water_area_code") %>%
  left_join_error_no_match(QCL_SpecieCode_isscaap, by = "species_code") %>%
  left_join_error_no_match(Mapping_commodity_ISSCAAP %>% rename(isscaap = ISSCAAP), by = "isscaap") %>%
  group_by(Inland_Marine, ISSCAAP_group, measure, year) %>%
  summarize(value = sum(value), .groups = "drop") %>%
  #filter(measure == "Q_tlw") %>%
  filter(year >= 2015) %>%
  spread(year, value)
# isscaap group seems to be a good aggregation with results at the global levels
# we will use isscaap_group as the main aggregation at the moment, mainly because
# it is consistent with FBS so we will have utilization results as well

### aggregation (map) to isscaap group, water area and FAO area_code ----

QCL_aquaculture_1 %>%
  left_join_error_no_match(QCL_WaterAreaCode_water, by = "water_area_code") %>%
  left_join_error_no_match(QCL_SpecieCode_isscaap, by = "species_code") %>%
  left_join_error_no_match(Mapping_commodity_ISSCAAP %>% rename(isscaap = ISSCAAP), by = "isscaap") %>%
  left_join_error_no_match(QCL_CountryCode_iso %>% distinct(un_code, FAO_area_code), by = "un_code") %>%
  group_by(FAO_area_code, water_area_code, water_area, Inland_Marine,
           species_code, ISSCAAP = isscaap, ISSCAAP_star, ISSCAAP_group, measure, year) %>%
  summarize(value = sum(value), .groups = "drop") ->
  QCL_aquaculture_2


## Capture ----

# QCL_capture includes production in both quantity (tonnes)

FISHSTAT_load_raw_data(DATASETCODE = "QCL_capture_quantity")

QCL_capture_quantity %>%
  transmute(un_code = country_un_code, water_area_code = area_code,
            year = period, species_code = species_alpha_3_code, measure, value) ->
  QCL_capture_0

### aggregation (map) to isscaap group, water area and FAO area_code ----

QCL_capture_0 %>%
  left_join_error_no_match(QCL_WaterAreaCode_water, by = "water_area_code") %>%
  left_join_error_no_match(QCL_SpecieCode_isscaap, by = "species_code") %>%
  left_join_error_no_match(Mapping_commodity_ISSCAAP %>% rename(isscaap = ISSCAAP), by = "isscaap") %>%
  left_join_error_no_match(QCL_CountryCode_iso %>% distinct(un_code, FAO_area_code), by = "un_code") %>%
  group_by(FAO_area_code, water_area_code, water_area, Inland_Marine,
           species_code, ISSCAAP = isscaap, ISSCAAP_star, ISSCAAP_group, measure, year) %>%
  summarize(value = sum(value), .groups = "drop") ->
  QCL_capture_1

QCL_capture_1 %>% distinct(ISSCAAP_group, measure)
# note that we only have numbers for some groups!

## Bind Aquaculture and Capture for prod quantity ----

QCL_aquaculture_2 %>%
  # values area removed
  filter(measure == "Q_tlw") %>%
  mutate(supply = "aquaculture") %>%
  bind_rows(
    QCL_capture_1 %>%
      # measures with Q_no_1 are removed
      filter(measure == "Q_tlw") %>%
      mutate(supply = "capture")) ->
  QCL_fisheries

QCL_fisheries %>%
  filter(ISSCAAP_star == F) %>%
  mutate(Inland_Marine = factor(Inland_Marine, levels = c("Inland", "Marine"),
                         labels = c("Inland waters", "Marine areas"))) %>%
  mutate(supply = factor(supply, levels = c("aquaculture", "capture"),
                         labels = c("Aquaculture", "Capture"))) %>%
  group_by(supply, Inland_Marine, year) %>%
  summarize(value = sum(value)/10^6) %>% #arrange(supply) %>%
  ggplot() +
  geom_area(aes(x = year, y = value, group = interaction(Inland_Marine, supply),
                fill = supply, alpha = Inland_Marine), stat = "identity",
            color = "black") +
  labs(fill = "Source", alpha = "Water area", x = "Year", y = "Million Tonnes") +
  guides(fill = guide_legend(order = 1),
         alpha = guide_legend(order = 2)) +
  scale_y_continuous(breaks = seq(0, 200, 40)) +
  scale_alpha_manual(values = c(.95, .55)) +
  theme_bw() -> p

ggsave("data-raw/Fig1_WorldProduction.png", plot = p, width = 8, height = 5)

# done so far ----


QCL_aquaculture_2 %>%
  left_join_error_no_match(QCL_WaterAreaCode_water) %>%
  group_by(ISSCAAP_group, Inland_Marine, water_area, measure, year) %>%
  summarize(value = sum(value)/10^6, .groups = "drop") %>%
  filter(year >= 2010) %>%
  spread(year, value) -> A

QCL_aquaculture_2 %>% filter(measure == "Q_tlw") %>%
  ggplot() +
  geom_bar(aes(x = year, y = value, fill = ISSCAAP_group), stat = "identity")

QCL_capture_1 %>%
  left_join_error_no_match(QCL_WaterAreaCode_water) %>%
  group_by(ISSCAAP_group, Inland_Marine, water_area, measure, year) %>%
  summarize(value = sum(value)/10^6, .groups = "drop") %>%
  filter(year >= 2010) %>%
  spread(year, value) -> A

QCL_capture_1 %>% filter(measure == "Q_tlw") %>%
  ggplot() +
  geom_bar(aes(x = year, y = value, fill = ISSCAAP_group), stat = "identity")



"FBSH_CBH_wide" %>% load_from_cache() %>% first -> FBSH_CBH_wide
FBSH_CBH_wide %>%
  gather_years() %>%
  FAOSTAT_AREA_RM_NONEXIST() %>%
  select(-element_code) %>%
  # merge Sudan and South Sudan
  FAO_AREA_DISAGGREGATE_HIST_DISSOLUTION_ALL(SUDAN2012_MERGE = T) ->
  FBSH_CB




library(ggplot2)

A %>%
  ggplot() +
  geom_bar(aes(x = year, y = Q_tlw, fill = environment), stat = "identity")

A %>%
  ggplot() +
  geom_bar(aes(x = year, y = Q_tlw, fill = un_code), stat = "identity")

A %>%
  group_by_at(vars(un_code, year)) %>%
  summarize(value = sum(Q_tlw, na.rm = T), .groups = "drop") %>%
  #filter(year == 2022) %>% arrange(-value) %>%
  left_join(QCL_CountryCode_iso, by = "un_code") %>%
  left_join(Mapping_gcamdata_FAO_iso_reg) %>%
  #filter(is.na(FAO_country)) %>% distinct(un_code, iso, area_code)
  group_by(year) %>%
  mutate(share = value / sum(value)) %>% ungroup() %>% arrange(-share) %>%
  filter(share >= 0.01) %>%
  ggplot() +
  geom_bar(aes(x = year, y = share, fill = FAO_country), color = "black", size = 0.4,
           stat = "identity")



A %>%
  group_by_at(vars(un_code, year)) %>%
  summarize(value = sum(Q_tlw, na.rm = T), .groups = "drop") %>%
  #filter(year == 2022) %>% arrange(-value) %>%
  left_join(QCL_CountryCode_iso, by = "un_code") %>%
  left_join(Mapping_gcamdata_FAO_iso_reg) %>%
  #filter(is.na(FAO_country)) %>% distinct(un_code, iso, area_code)
  group_by(year) %>%
  mutate(share = value / sum(value)) %>% ungroup() %>% arrange(-share) %>%
  filter(share >= 0.01) %>% filter(year %in% c(1990)) -> A1

A1

A %>%
  ggplot() +
  geom_bar(aes(x = year, y = Q_tlw, fill = un_code), stat = "identity")

  left_join_error_no_match(QCL_SpecieCode_isscaap, by = "species_code") %>%
  left_join(QCL_CountryCode_iso, by = "un_code") %>% filter(is.na(iso))








FISHSTAT_load_raw_data(DATASETCODE = "TCL_CommodityCode")

FISHSTAT_load_raw_data(DATASETCODE = "TCL_fish_quantity")





QCL_aquaculture_quantity %>% distinct(environment_alpha_2_code)



QCL_SpecieCode_isscaap %>% distinct(isscaap)
  distinct(species_code)
  filter(year == 2019, un_code == 840)  %>%


QCL_aquaculture_quantity %>%
  FF_check_count_plot(c()) +
  ggplot2::labs(title = "QCL summary")

.DF %>% group_by(year, element) %>%
  summarise(Country = length(unique(area_code)),
            Item = length(unique(item_code)), .groups = "drop") %>%
  gather(header, count, -year, -element) %>%
  filter(element %in% .ELEMENT) %>%
  ggplot() + facet_wrap(~header, scales = "free") +
  geom_line(aes(x = year, y = count, color = element), size = 1) +
  theme_bw()




  TCL_CommodityCode %>% distinct(identifier)

  QCL_WaterAreaCode %>%
    distinct(water_area_code = code, water_area = name_en)

  QCL_CountryCode

  distinct(area_code, environment_alpha_2_code) %>%
  distinct(environment_alpha_2_code)

  TCL_fish_quantity


  TCL_CommodityCode %>% distinct(name_en)
  QCL_SpecieCode %>% distinct(year)

QCL_aquaculture_quantity

# Sectoral aggregation

# Regional aggregation (model-specific)





QCL_CountryCode %>% distinct(un_code, area_code = identifier, iso0 = iso3_code) %>%
  left_join(Mapping_gcamdata_FAO_iso_reg) -> A


