Bal_new_all %>% filter(value != 0) %>%
  spread(year, value) %>%
  write.csv("B.csv")
GCAM_APE_after2010 %>%
  spread(year, value) %>%
  write.csv("A.csv")


# This module compares the new PCE data vs FAO FBS

MODULE_INPUTS <-
  c("GCAM_APE_after2010",
    "Bal_new_all",
    "FAO_Food_Macronutrient_All",
    FILE = file.path(DIR_RAW_DATA_FAOSTAT, "Mapping_SUA_PrimaryEquivalent"),
    FILE = file.path(DIR_RAW_DATA_FAOSTAT, "SUA_item_code_map"),
    FILE = file.path(DIR_RAW_DATA_FAOSTAT, "Mapping_FAO_iso_reg") )

MODULE_INPUTS %>% load_from_cache() -> all_data

get_data_list(all_data, MODULE_INPUTS, strip_attributes = TRUE)

GCAM_APE_after2010 %>% distinct(GCAM_commodity)

Bal_new_all %>% filter(value != 0)
GCAM_APE_after2010 %>% filter(value != 0)



SUA_item_code_map %>% select(item, item_code) -> SUA_item_code_map
FAO_Food_Macronutrient_All -> FAO_Food_Macronutrient_rate



Bal_new_all %>% filter(value != 0.0) %>%
  transmute(area_code, item_code, element, year, value) ->
  GCAMFAOSTAT_SUA

# Goal
# Compare SUA & FBS for wheat/corn at global & regional scales
# But adding details



Mapping_SUA_PrimaryEquivalent %>%
  left_join_error_no_match(SUA_item_code_map %>% select(item, item_code) %>%
                             rename(sink_item_code = item_code), by=c("sink_item" = "item")) %>%
  left_join_error_no_match(SUA_item_code_map %>%  select(item, item_code) %>%
                             rename(source_item_code = item_code), by=c("source_item" = "item")) %>%
  mutate(APE_comm = as.factor(APE_comm)) ->
  Mapping_SUA_PrimaryEquivalent_ID


Mapping_SUA_PrimaryEquivalent_ID %>%
  distinct(APE_comm) %>% pull

# china mainland 41
# usa 231
AC = 231
APE_COMM_NAME <- "Wheat"


Mapping_SUA_PrimaryEquivalent_ID %>%
  filter(APE_comm == APE_COMM_NAME) %>%
  distinct(sink_FBS_item) %>% pull ->
  FAO_FBS_COMM_NAME

Mapping_SUA_PrimaryEquivalent_ID %>%
  filter(APE_comm == APE_COMM_NAME) %>%
  select(sink_item_code, source_item_code) %>% unlist %>% unique() ->
  SUACode

GCAMFAOSTAT_SUA %>%
  filter(item_code %in% SUACode) ->
  GCAMFAOSTAT_SUA_sector

GCAMFAOSTAT_SUA_sector %>%
  filter(area_code== AC) %>%
  group_by_at(vars(-area_code, -value)) %>%
  summarize(value = sum(value), .groups = "drop") %>%
  left_join_error_no_match(SUA_item_code_map) %>% select(-item_code) %>%
  filter(year == 2020) %>% spread(item, value) ->
  GCAMFAOSTAT_SUA_sector1


FAO_Food_Macronutrient_rate %>%
  filter(area_code== AC) %>%
  filter(item_code %in% SUACode) %>%
  group_by_at(vars(year, item_code, element = macronutrient)) %>%
  summarize(value = sum(value), .groups = "drop") %>%
  left_join_error_no_match(SUA_item_code_map) %>% select(-item_code) %>%
  filter(year == 2020) %>%
  spread(item, value) %>%
  bind_rows(
    GCAMFAOSTAT_SUA_sector1
  ) ->
  GCAMFAOSTAT_SUA_sector1_2020





 "FBS_wide" %>% load_from_cache() %>% first() -> FBS_wide

 FBS_wide %>% gather_years() %>%
   filter(year >= min(FAOSTAT_Hist_Year_FBS)) %>%
   FAOSTAT_AREA_RM_NONEXIST() -> FBS


FBS %>%
  filter(year >= min(FAOSTAT_Hist_Year_FBS)) %>%
  # keep only balance items
  filter(!element_code %in% c(645, 664, 674, 684)) %>%
  # simplify elements and make them consistent with SUA
  mutate(element = gsub(" Quantity| supply quantity \\(tonnes\\)| \\(non-food\\)", "", element),
         element = replace(element, element == "Losses", "Loss"),
         element = replace(element, element == "Processing", "Processed")) %>%
  # convert units back to tonnes first since FBS originally used 1000 tons
  mutate(value = value) ->
  FBS1


FBS1 %>% filter(year == 2020) %>%
  filter(area_code== AC) %>%
  filter(item %in% FAO_FBS_COMM_NAME ) %>%
  group_by_at(vars(-area_code, -area, -value)) %>%
  summarize(value = sum(value), .groups = "drop") ->
  FAO_FBS_Old


GCAM_APE_after2010 %>%
  filter(year == 2020) %>%  filter(region_ID == AC) %>%
  filter(GCAM_commodity == APE_COMM_NAME) %>%
  group_by_at(vars(-region_ID, -value)) %>%
  summarize(value = sum(value)) %>%
  mutate(GCAM_commodity = "SUA") %>%
  spread(GCAM_commodity, value) ->
  SUA_FBS_New

FAO_FBS_Old %>%
  select(-element_code) %>%
  rename(FBS = value) %>%
  full_join(SUA_FBS_New) %>%
  full_join(
    GCAMFAOSTAT_SUA_sector1_2020, by = join_by(element, year)
  ) -> Compare


C %>% readr::write_csv("Maize2020_usa.csv")




#---- All region ----


# china mainland 41
# usa 231

APE_COMM_NAME <- "Wheat"


Mapping_SUA_PrimaryEquivalent_ID %>%
  filter(APE_comm == APE_COMM_NAME) %>%
  distinct(sink_FBS_item) %>% pull ->
  FAO_FBS_COMM_NAME

Mapping_SUA_PrimaryEquivalent_ID %>%
  filter(APE_comm == APE_COMM_NAME) %>%
  select(sink_item_code, source_item_code) %>% unlist %>% unique() ->
  SUACode

GCAMFAOSTAT_SUA %>%
  filter(item_code %in% SUACode) ->
  GCAMFAOSTAT_SUA_sector

GCAMFAOSTAT_SUA_sector %>%
  group_by_at(vars(-value)) %>%
  summarize(value = sum(value), .groups = "drop") %>%
  left_join_error_no_match(SUA_item_code_map) %>% select(-item_code) %>%
  filter(year == 2020) %>%
  spread(item, value, fill = 0) ->
  GCAMFAOSTAT_SUA_sector1


FAO_Food_Macronutrient_rate %>%
  filter(item_code %in% SUACode) %>%
  group_by_at(vars(area_code, year, item_code, element = macronutrient)) %>%
  summarize(value = sum(value), .groups = "drop") %>%
  left_join_error_no_match(SUA_item_code_map) %>% select(-item_code) %>%
  filter(year == 2020) %>%
  mutate(value = if_else(element %in% c("MtProtein", "MtFat"), value * 1000, value)) %>%
  mutate(element = replace(element, element == "MKcal", "Calorie"),
         element = replace(element, element == "MtProtein", "Protein"),
         element = replace(element, element == "MtFat", "Fat"))  %>%
  spread(item, value) %>%
  bind_rows(
    GCAMFAOSTAT_SUA_sector1
  ) -> GCAMFAOSTAT_SUA_sector1_2020





"FBS_wide" %>% load_from_cache() %>% first() -> FBS_wide

FBS_wide %>% gather_years() %>%
  filter(year >= min(FAOSTAT_Hist_Year_FBS)) %>%
  FAOSTAT_AREA_RM_NONEXIST() -> FBS


FBS %>%
  filter(year >= min(FAOSTAT_Hist_Year_FBS)) %>%
  # keep only balance items
  filter(!element_code %in% c(645, 664, 674, 684)) %>%
  # simplify elements and make them consistent with SUA
  mutate(element = gsub(" Quantity| supply quantity \\(tonnes\\)| \\(non-food\\)", "", element),
         element = replace(element, element == "Losses", "Loss"),
         element = replace(element, element == "Processing", "Processed")) %>%
  # convert units back to tonnes first since FBS originally used 1000 tons
  mutate(value = value) ->
  FBS1


FBS1 %>% filter(year == 2020) %>%
  filter(item %in% FAO_FBS_COMM_NAME ) %>%
  group_by_at(vars(-area, -value)) %>%
  summarize(value = sum(value), .groups = "drop") %>%
  mutate(element = replace(element, element == "Food supply (kcal)", "Calorie"),
         element = replace(element, element == "Protein supply quantity (t)", "Protein"),
         element = replace(element, element == "Fat supply quantity (t)", "Fat")) %>%
  mutate(value = if_else(element %in% c("Protein", "Fat"), value / 1000, value)) %>%
  mutate(unit = replace(unit, is.na(unit)|unit == "t", "1000 t")) ->
  FAO_FBS_Old


GCAM_APE_after2010 %>%
  filter(year == 2020) %>%
  filter(GCAM_commodity == APE_COMM_NAME) %>%
  group_by_at(vars(-value)) %>%
  summarize(value = sum(value)) %>% ungroup() %>%
  mutate(GCAM_commodity = "SUA") %>%
  spread(GCAM_commodity, value) %>%
  rename(area_code = region_ID)->
  SUA_FBS_New

GCAMFAOSTAT_SUA_sector1_2020 %>%
  gather(item, value, -area_code:-element) %>%
  group_by_at(vars(-value, -item)) %>%
  summarize(SUA_sum = sum(value), .groups = "drop") ->
  SUA_FBS_New_sum

FAO_FBS_Old %>%
  select(-element_code) %>% filter(unit == "1000 t") %>%
  spread(element, value, fill = 0) %>%
  mutate(`Regional supply` = Production + Import,
         `Regional demand` = Export + Food + Feed + Processed + Seed + Loss + `Other uses` + `Tourist consumption`,
          Residuals = `Regional supply` - `Regional demand` - `Stock Variation`) %>%
  gather(element, value, -area_code:-year) %>%
  bind_rows(
    FAO_FBS_Old %>%  select(-element_code) %>% filter(unit != "1000 t")
  ) %>%
  rename(FBS = value) %>%
  full_join(SUA_FBS_New) %>%
  full_join(SUA_FBS_New_sum) %>%
  mutate(SUA = if_else(element %in% c("Calorie", "Protein", "Fat"), SUA_sum, SUA)) %>%
  full_join(
    GCAMFAOSTAT_SUA_sector1_2020
  ) -> Compare

Compare %>% filter(area_code==203) %>% write.csv("A.csv")
Compare %>% filter(element == "Residuals") %>%
  select(area_code:SUA) %>%
  filter(area_code == AC)

Compare %>%
  filter(element == "Residuals") %>%
  select(area_code:SUA) -> A

Compare %>%
  filter(element == "Residuals") %>%
  select(area_code:SUA) %>%
  group_by(item, year) %>% filter(!is.na(item)) %>% #filter(is.na(SUA))
  summarize(FBS = sum(FBS), SUA = sum(SUA, na.rm = T))
  filter(area_code == AC)

Compare %>% filter(area_code == AC) -> C


C %>% readr::write_csv("Maize2020_usa.csv")




---
  -----

  Mapping_SUA_PrimaryEquivalent %>%
  left_join_error_no_match(SUA_item_code_map %>% rename(sink_item_code = item_code), by=c("sink_item" = "item")) %>%
  left_join_error_no_match(SUA_item_code_map %>% rename(source_item_code = item_code), by=c("source_item" = "item")) %>%
  mutate(APE_comm = as.factor(APE_comm)) ->
  Mapping_SUA_PrimaryEquivalent_ID

Mapping_SUA_PrimaryEquivalent_ID %>%
  filter(APE_comm == APE_COMM_NAME) %>%
  select(sink_item_code, source_item_code) %>% unlist %>% unique() -> SUACode

GCAMFAOSTAT_SUA %>%
  filter(item_code %in% SUACode) ->
  GCAMFAOSTAT_SUA_Wheat

GCAMFAOSTAT_SUA_Wheat %>% #filter(area_code== AC) %>%
  group_by_at(vars(-area_code, -value)) %>%
  summarize(value = sum(value), .groups = "drop") %>%
  left_join_error_no_match(SUA_item_code_map) %>% select(-item_code) %>%
  filter(year == 2020) %>% spread(item, value) ->
  GCAMFAOSTAT_SUA_Wheat1


FAO_Food_Macronutrient_rate %>% #filter(area_code== AC) %>%
  filter(item_code %in% SUACode) %>%
  group_by_at(vars(year, item_code, element = macronutrient)) %>%
  summarize(value = sum(value), .groups = "drop") %>%
  left_join_error_no_match(SUA_item_code_map) %>% select(-item_code) %>%
  filter(year == 2020) %>%
  spread(item, value) %>%
  bind_rows(
    GCAMFAOSTAT_SUA_Wheat1
  ) ->
  GCAMFAOSTAT_SUA_Wheat_2020









FBS %>%
  filter(year >= min(FAOSTAT_Hist_Year_FBS)) %>%
  # keep only balance items
  filter(!element_code %in% c(645, 664, 674, 684)) %>%
  # simplify elements and make them consistent with SUA
  mutate(element = gsub(" Quantity| supply quantity \\(tonnes\\)| \\(non-food\\)", "", element),
         element = replace(element, element == "Losses", "Loss"),
         element = replace(element, element == "Processing", "Processed")) %>%
  # convert units back to tonnes first since FBS originally used 1000 tons
  mutate(value = value) ->
  FBS1


FBS1 %>% filter(year == 2020) %>%
  filter(area_code== AC) %>%
  filter(item %in% FAO_FBS_COMM_NAME ) %>%
  group_by_at(vars(-area_code, -area, -value)) %>%
  summarize(value = sum(value), .groups = "drop") -> A


GCAM_APE_after2010 %>% filter(year == 2020) %>%  filter(region_ID == AC) %>%
  filter(GCAM_commodity == APE_COMM_NAME) %>%
           group_by_at(vars(-region_ID, -value)) %>%
           summarize(value = sum(value)) %>%
           mutate(GCAM_commodity = "SUA") %>%
           spread(GCAM_commodity, value) -> B

         A %>% select(-element_code) %>%
           rename(FBS = value) %>%
           full_join(B) %>%
           full_join(
             GCAMFAOSTAT_SUA_Wheat_2020, by = join_by(element, year)
           ) -> C


         C %>% readr::write_csv("Maize2020.csv")



