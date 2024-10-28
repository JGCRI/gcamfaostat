
# Compare GCAM vs. FAO forest area ----


FAO_forest <- readr::read_csv("data-raw/FAO_forest.csv")
GCAM_land <- readr::read_csv("data-raw/GCAM_aggregated_land_allocation.csv")

GCAM_land %>%
  gather_years() %>%
  filter(year == 2020, grepl("forest|shrub|grass", LandLeaf)) %>%
  mutate(LandLeaf = gsub("\\(|\\)", "", LandLeaf)) %>%
  spread(LandLeaf, value, fill = 0) %>%
  transmute(region, year, GCAM_FOR_MGMT = `forest managed`, GCAM_FOR_UnMGMT = `forest unmanaged`,
            GCAM_FOR = GCAM_FOR_MGMT + GCAM_FOR_UnMGMT,  GCAM_shrubs = shrubs, GCAM_grass = grass) %>%
  left_join(
    FAO_forest %>% filter(year == 2020) %>%
      transmute(region, year, FAO_FOR_Natural = `Naturally regenerating forest`, FAO_FAO_Planted = `Planted Forest`,
                FAO_FOR = FAO_FOR_Natural + FAO_FAO_Planted), by = c("region", "year")
  )  %>%
  mutate(Diff = GCAM_FOR - FAO_FOR) -> A


A %>%
  group_by() %>%
  summarize(FAO_FOR = sum(FAO_FOR, na.rm = T),
            GCAM_shrubs = sum(GCAM_shrubs, na.rm = T),
            GCAM_FOR = sum(GCAM_FOR))

library(ggplot2)
A %>%
  select(region, year, FAO_FOR, GCAM_FOR) %>%
  gather(source, value, FAO_FOR, GCAM_FOR) %>%
  ggplot() +
  geom_point(aes(x = region, y = value, fill = source), shape = 21, size = 2 )

A %>%
  select(region, year, FAO_FOR, GCAM_FOR) %>%
  mutate(Diff = GCAM_FOR - FAO_FOR) %>%
  #gather(source, value, FAO_FOR, GCAM_FOR) %>%
  ggplot() +
  geom_bar(aes(x = reorder(region, Diff), y = FAO_FOR, fill = "FAO"), stat = "identity", color = "black") +
  geom_point(aes(x = region, y = GCAM_FOR, fill = "GCAM"), shape = 21, size = 4 ) +
  theme_bw() +
  labs(x = "Region", y = "Thous km2",
       title = "Forest area: GCAM vs. FAO", subtitle = "Global: GCAM (3 Bha) vs. FAO (4 Bha) ",
       fill = "Source") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) -> p

ggsave(plot = p,filename =  "data-raw/ForestAreaCompare.png", width = 11, height = 10)


A %>%
  select(region, year, FAO_FOR, GCAM_FOR, GCAM_shrubs) %>%
  mutate(Diff = GCAM_FOR + GCAM_shrubs - FAO_FOR) %>%
  #gather(source, value, FAO_FOR, GCAM_FOR) %>%
  ggplot() +
  geom_bar(aes(x = reorder(region, Diff), y = FAO_FOR, fill = "FAO"), stat = "identity", color = "black") +
  geom_point(aes(x = region, y = GCAM_FOR + GCAM_shrubs, fill = "GCAM"), shape = 21, size = 4 ) +
  theme_bw() +
  labs(x = "Region", y = "Thous km2",
       title = "Forest area: GCAM (forest + shrub) vs. FAO", subtitle = "Global: GCAM (4.17 Bha) vs. FAO (4.05 Bha) ",
       fill = "Source") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) -> p

ggsave(plot = p,filename =  "data-raw/ForestShrubAreaCompare.png", width = 11, height = 10)







# ignore below ----
# Check FAO data and save [done and data read in above already] ----

FAOSTAT_load_raw_data(DATASETCODE = "RL")
FAOSTAT_load_raw_data(DATASETCODE = "LC")

"common/iso_GCAM_regID" %>% load_from_cache() %>% first -> iso_GCAM_regID
"aglu/AGLU_ctry" %>% load_from_cache() %>% first -> AGLU_ctry
"common/GCAM_region_names" %>% load_from_cache() %>% first -> GCAM_region_names




  RL %>%
    filter(element == "Area", unit == "1000 ha") %>%
    filter(grepl("forest", item, ignore.case = T)) %>%
    filter(area_code < 350) -> RL1

  RL1 %>%
    select(area_code,
           area,
           item_code,
           item,
           element_code,
           element,
           year,
           value,
           unit) %>%
    group_by(item, item_code) %>%
    # Complete all dimensions
    complete(
      nesting(area_code, area),
      nesting(item_code, item),
      nesting(element_code, element, unit),
      year
    ) %>% ungroup() %>%
    rm_accent("item", "area") -> RL2


  RL2 %>%
    group_by(element_code, area_code, area, item_code, item) %>%
    # linearly interpolate only forward!
    # then NA = 0
    mutate(value = approx_fun(year, value)) %>%
    FAOSTAT_AREA_RM_NONEXIST(RM_AREA_CODE = NULL) %>%
    ungroup() ->
    RL3

  aglu.MODEL_MEAN_PERIOD_LENGTH -> MA_period

  RL3 %>%
    FAO_AREA_DISAGGREGATE_HIST_DISSOLUTION_ALL %>%
    ungroup() %>%
    left_join(AGLU_ctry %>% distinct(area_code = FAO_country_code, iso), by = c("area_code"))  %>%
    filter(!is.na(iso)) %>%
    left_join_error_no_match(iso_GCAM_regID %>% select(iso, GCAM_region_ID), by = "iso") %>%
    # Adding moving average
    # dplyr::group_by_at(dplyr::vars(-year, -value)) %>%
    # mutate(value = if_else(is.na(Moving_average(value, periods = MA_period)),
    #                        value, Moving_average(value, periods = MA_period))) %>%
    ungroup() ->
    RL4

  RL4  %>%
    filter(year %in% c(2015, 2020)) %>%
    left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
    group_by(GCAM_region_ID, region, year, item) %>%
    #group_by(year, item) %>%
    summarize(value = sum(value)/100, .groups = "drop") %>%
    mutate(unit = "Thous km2") %>%
    filter(!grepl("irrigated|Primary", item)) %>%
    spread(item, value)  %>%
    readr::write_csv("data-raw/FAO_forest.csv")


  RL %>% filter(iso == "twn")








