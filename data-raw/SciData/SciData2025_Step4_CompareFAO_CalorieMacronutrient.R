
# nutrient compare ----
OA %>%
  filter(element_code == 511, item_code == 3010) %>%
  transmute(area, area_code, year, value) %>%
  filter(year %in% FAOSTAT_Hist_Year_FBS) %>%
  mutate(area_code = replace(area_code, area_code %in% c(276, 277), 206)) %>%
  group_by(area_code, year) %>%
  summarize(value = sum(value), .groups = "drop")->
  POP

"FBS_wide" %>% load_from_cache() %>% first() -> FBS_wide

FBS_wide %>% gather_years() %>%
  filter(year >= min(FAOSTAT_Hist_Year_FBS)) %>%
  FAOSTAT_AREA_RM_NONEXIST() -> FBS


FBS %>%
  # Change Sudan back to Sudan (former) 206
  mutate(area_code = replace(area_code, area_code %in% c(276, 277), 206)) %>%
  filter(year >= min(FAOSTAT_Hist_Year_FBS)) %>%
  # keep only balance items
  filter(!element_code %in% c(645, 664, 674, 684)) %>%
  # simplify elements and make them consistent with SUA
  mutate(element = gsub(" Quantity| supply quantity \\(tonnes\\)| \\(non-food\\)", "", element),
         element = replace(element, element == "Losses", "Loss"),
         element = replace(element, element == "Processing", "Processed")) %>%
  group_by_at(vars(-value, -area)) %>%
  summarize(value = sum(value, na.rm = T), .groups = "drop") ->
  FBS1


# In FAO-FBS, tourist consumption was separated while it is currently aggregated
# in the recompiled SUA and Traceable FBS. Thus, for consistency in comparison,
# we to the adjustments in FAO-FBS to include tourist consumption into food consumption
# and the corresponding calories and macronutrients.

# We only make this adjustment when `Tourist consumption` >0 & Food > 0
# because of low data quality for `Tourist consumption` >0 & Food = 0
# cases (not able to compute nutrient conversion ratio)

# Note that only 41 regions have data in tourist consumption


# compute the ratio between tourist consumption and food consumption
FBS1 %>%
  # keep
  filter(grepl("Tourist|Food", element)) %>%
  select(-element_code, -unit) %>%
  spread(element, value) %>%
  filter(`Tourist consumption` >0, Food > 0) %>%
  mutate(Ratio_Tourist_Food = `Tourist consumption` / Food) %>%
  select(area_code, item_code, year, Ratio_Tourist_Food) ->
  df_Ratio_Tourist_Food


FBS1 %>%
  left_join(df_Ratio_Tourist_Food,
            by = c("area_code", "item_code", "year")) %>%
  mutate(value = if_else(
    element %in% c("Food supply (kcal)",
                   "Protein supply quantity (t)",
                   "Fat supply quantity (t)",
                   "Food") & !is.na(Ratio_Tourist_Food),
    (1 + Ratio_Tourist_Food) * value, value)) %>%
  select(-Ratio_Tourist_Food) %>%
  filter(element != "Tourist consumption") ->
  FBS2


FBS2 %>%
  filter(unit %in% c("t", "Kcal")) %>%
  #filter(item %in% FAO_FBS_COMM_NAME ) %>%
  group_by_at(vars(unit, year, element, area_code)) %>%
  summarize(value = sum(value, na.rm = T), .groups = "drop") %>%
  mutate(element = replace(element, element == "Food supply (kcal)", "Dietary energy"),
         element = replace(element, element == "Protein supply quantity (t)", "Protein"),
         element = replace(element, element == "Fat supply quantity (t)", "Fat")) %>%
  mutate(value = if_else(element %in% c("Protein", "Fat"), value / 10^6, value)) %>%
  select(-unit) %>% rename(`FAO-FBS` = value) %>%
  full_join(
    L107.Traceable_FBS_Food_Calorie_Macronutrient_2010Plus %>%
      filter(year %in% FAOSTAT_Hist_Year_FBS) %>%
      group_by_at(vars(unit, year, element, area_code)) %>%
      summarize(TFBS = sum(value, na.rm = T), .groups = "drop"),
    by = c("year", "element", "area_code") ) %>%
  mutate(diff = TFBS - `FAO-FBS`, ratio = TFBS / `FAO-FBS`) %>%
  left_join_error_no_match(
    POP %>% select(year, area_code, value) %>% rename(pop = value), by = c("year", "area_code")
  ) ->
  FBS_EnergyNutrient_Compare

# regions not in FAO-FBS
FBS_EnergyNutrient_Compare %>%
  filter(is.na(`FAO-FBS`)) %>%
  distinct(area_code) %>%
  left_join_error_no_match(Mapping_gcamdata_FAO_iso_reg)


FBS_EnergyNutrient_Compare %>%
    #filter(!is.na(`FAO-FBS`)) %>%
    replace_na(list(`FAO-FBS` = 0)) %>%
    mutate(`FAO-FBS` = `FAO-FBS` / pop / 365*1000,
           TFBS = TFBS / pop / 365*1000) %>%
    mutate(`FAO-FBS` = if_else(element != "Dietary energy", `FAO-FBS` * 10^6, `FAO-FBS`),
           TFBS = if_else(element != "Dietary energy", TFBS * 10^6, TFBS) ) ->
    df

df %>%
  ggplot() + facet_wrap(~element, scales = "free") +
  geom_point(aes(x = `FAO-FBS`, y = TFBS))

  FBS_EnergyNutrient_Compare %>%
    filter(!is.na(`FAO-FBS`)) %>%
    #replace_na(list(`FAO-FBS` = 0)) %>%
    filter(`FAO-FBS`>0) %>% # distinct(area_code)
    ggplot() + facet_wrap(~element, scales = "free") +
    geom_abline(intercept = 0, slope = 1) +
    geom_point(aes(x = log(`FAO-FBS`), y = log(TFBS), color = year), alpha = 0.3)+
    scale_color_distiller(palette = "YlGnBu", direction = 1) +
    # scale_x_continuous(limits = c(-20, 20)) +
    # scale_y_continuous(limits = c(-20, 20)) +
    labs(x = "Natural log of FAO-FBS values",
         y = "Natural log of traceable FBS values",
         fill = "Year", color = "Year" ) +
    theme_bw() + theme0 -> p_balSD; p_balSD
  p_balSD %>% Write_png("Validation_FAOCompare_EnergyNutrient", w = 15, h = 6)

  FBS_EnergyNutrient_Compare %>%
    filter(!is.na(`FAO-FBS`)) %>%
    #replace_na(list(`FAO-FBS` = 0)) %>%
    #filter(`FAO-FBS`>0) %>%
    ggplot() + facet_wrap(~element, scales = "free") +
    geom_abline(intercept = 0, slope = 1) +
    geom_point(aes(x = (`FAO-FBS`), y = (TFBS), color = year), alpha = 0.3)+
    scale_color_distiller(palette = "YlGnBu", direction = 1) +
    labs(fill = "Year", color = "Year" ) +
    theme_bw() + theme0



  FBS_EnergyNutrient_Compare %>%
    filter(!is.na(`FAO-FBS`)) %>%
    filter(`FAO-FBS`>0) %>%  #distinct(area_code)
    mutate(per_diff = 100*(ratio-1)) %>%
    group_by(year, element) %>%
    summarize(`FAO-FBS` = sum(`FAO-FBS`),
              TFBS = sum(TFBS),
              n = sum(n()),
              sd = sd(per_diff),
              mean = mean(per_diff),
              Q00 = quantile(per_diff,probs = 0),
              Q05 = quantile(per_diff,probs = .05),
              Q25 = quantile(per_diff,probs = .25),
              median = median(per_diff),
              Q75 = quantile(per_diff,probs = .75),
              Q95 = quantile(per_diff,probs = .95),
              Q100 = quantile(per_diff,probs = 1),
              .groups = "drop") %>%
    mutate(`FAO-FBS` = if_else(element == "Dietary energy", `FAO-FBS` / 10^6, `FAO-FBS`),
           TFBS = if_else(element == "Dietary energy", TFBS / 10^6, TFBS)   ) %>%
    arrange(element, year) %>%
    mutate(unit = if_else(element == "Dietary energy", "Peta-kcal", "Mt")) %>%
    mutate(diff = TFBS - `FAO-FBS`) -> A

  A %>% readr::write_csv("output/gcamfaostat_SciData/Table_FAO_FBS_EnergyNutrient.csv")

  # 186 vs. 195

  # SUA compare ----
  # using FBS2 above


  FBS2 %>% #filter(!unit %in% c("Kcal", "t")) %>%
    select(-unit) %>%
    mutate(element = gsub(" quantity", "", element)) %>%
    rename(`FAO-FBS` = value) ->
    FBS3

  FAO_Compare_ele <-
    c(RegSupplyEle, "Regional supply", rev(RegDemendEle), "Regional demand",
      "Residuals", "Stock Variation", "Dietary energy", "Fat", "Protein")

Add_WLD_AGG <- function(.df){

    .df %>%
    group_by_at(vars(-iso, -value)) %>%
    summarize(value = sum(value, na.rm = T), .groups = "drop") %>%
    mutate(iso = "wld") -> .df1

  .df %>%
    bind_rows(.df1) ->
    .df2
  return(.df2)
}

FBS_compare <- function(){

    Nested_Mapping_SUA_To_Traceable_FBS %>%
      filter(aggregated_PCe_item == FAO_Compare_item) %>%
      select(source_item_code, sink_item_code) %>% unlist() %>% unique() ->
      SUA_itemcode0

    Nested_Mapping_SUA_To_Traceable_FBS %>%
      filter(aggregated_PCe_item == FAO_Compare_item) %>%
      select(source_item, sink_item) %>% unlist() %>% unique() ->
      SUA_item0


    Traceable_FBS_PCe_2010_2022 %>%
      gather_years() %>%
      Add_WLD_AGG %>%
      filter(iso %in% FAO_Compare_reg,
             year == FAO_Compare_yr,
             item == FAO_Compare_item) %>%
      spread(item, value) %>%
      left_join(
        SUA_2010_2022 %>%
          gather_years() %>%
          Add_WLD_AGG %>%
          filter(iso %in% FAO_Compare_reg,
                 year == FAO_Compare_yr) %>%
          group_by(item, item_code, year, element) %>%
          summarize(value = sum(value), .groups = "drop") %>%
          #mutate(value = value / 1000) %>%
          filter(item_code %in% SUA_itemcode0)%>%
          mutate(item = factor(item, levels = SUA_item0)) %>%
          select(-item_code) %>%
          spread(item, value) , by = c("element", "year")
      ) -> df_tFBS_SUA1

    Traceable_FBS_Food_Calorie_Macronutrient_2010_2022 %>%
      gather_years() %>%
      Add_WLD_AGG %>%
      mutate(value =if_else(element %in% c("Fat", "Protein"), value * 10^3, value)) %>%
      filter(iso == FAO_Compare_reg,
             year == FAO_Compare_yr,
             item == FAO_Compare_item) %>%
      select(-unit) %>%
      spread(item, value) %>%
      left_join(
        SUA_Food_Calorie_Macronutrient_2010_2022 %>%
          gather_years() %>%
          Add_WLD_AGG %>%
          mutate(value = if_else(element %in% c("Fat", "Protein"), value * 10^3, value)) %>%
          filter(iso == FAO_Compare_reg,
                 year == FAO_Compare_yr,
                 item_code %in% SUA_itemcode0) %>%
          select(-item) %>%
          left_join(SUA_2010_2022 %>% distinct(item_code, item), by = "item_code") %>%
          select(-item_code) %>%
          mutate(unit = if_else(element %in% c("Fat", "Protein"), gsub("^M", "1000 ", unit), unit) ) %>%
          spread(item, value),
        by = c("iso", "element", "year")
      ) -> df_tFBS_SUA2

    df_tFBS_SUA <-
      df_tFBS_SUA1 %>%
      bind_rows(df_tFBS_SUA2)


    FBS3 %>%
      filter(year == FAO_Compare_yr,
             item == FAO_Compare_item) %>%
      left_join_error_no_match(Mapping_gcamdata_FAO_iso_reg, by = "area_code") %>%
      select(iso, item, element, year, value = `FAO-FBS`) %>%
      Add_WLD_AGG %>%
      filter(iso == FAO_Compare_reg) %>%
      rename(`FAO-FBS` = value) %>%
      mutate(element = replace(element, element == "Food supply (kcal)", "Dietary energy"),
             element = replace(element, element == "Protein supply (t)", "Protein"),
             element = replace(element, element == "Fat supply (t)", "Fat")) %>%
      mutate(`FAO-FBS` = if_else(element %in% c("Fat", "Protein"), `FAO-FBS` / 10^3, `FAO-FBS`)) %>%
      transmute(iso, year, `FAO-FBS`, element) ->
      df_FAO_FBS

    df_tFBS_SUA %>%
      full_join(df_FAO_FBS, by = c("iso", "element", "year")) %>%
      select(
        iso, year, element, unit, c("FAO-FBS", FAO_Compare_item, all_of(SUA_item0))
      ) %>%
      mutate(element = factor(element, levels = FAO_Compare_ele)) %>%
      arrange(element) ->
      df
    return(df)
  }



FAO_Compare_reg <- "ind"
FAO_Compare_item <- "Rice and products"
FAO_Compare_yr <- 2022

FBS_compare() %>%
  mutate_all(~replace(., is.numeric(.) &  is.na(.), 0)) -> A

A %>% readr::write_csv(paste0("output/gcamfaostat_SciData/Table_FAO_compare",
                              FAO_Compare_reg, "_", FAO_Compare_yr, "_", gsub(" ", "",FAO_Compare_item),
                              ".csv"))


FAO_Compare_reg <- "usa"
FAO_Compare_reg <- "wld"
FAO_Compare_item <- "Maize and products"
FAO_Compare_item <- "Wheat and products"
FAO_Compare_item <- "Seed cotton and products"


FBS_compare() %>%
  mutate_all(~replace(., is.numeric(.) &  is.na(.), 0)) -> A

A %>% readr::write_csv(paste0("output/gcamfaostat_SciData/Table_FAO_compare",
                              FAO_Compare_reg, "_", FAO_Compare_yr, "_", gsub(" ", "",FAO_Compare_item),
                              ".csv"))






