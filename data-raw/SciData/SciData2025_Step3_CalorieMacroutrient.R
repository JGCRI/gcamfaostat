

# Technical Validation ----

# 1 Overview of data traceable FBS, dietary energy, and macronutrient

OA %>%
  filter(element_code == 511, item_code == 3010) %>%
  transmute(area, area_code, year, value) %>%
  filter(year %in% FAOSTAT_Hist_Year_FBS) %>%
  mutate(area_code = replace(area_code, area_code %in% c(276, 277), 206)) %>%
  group_by(area_code, year) %>%
  summarize(value = sum(value), .groups = "drop")->
  POP


L107.Traceable_FBS_Food_Calorie_Macronutrient_2010Plus %>%
  filter(year %in% FAOSTAT_Hist_Year_FBS) %>%
  # Aggregate to region and GCAM commodity
  dplyr::group_by_at(vars(item, APE_comm_Agg, element, unit)) %>%
  summarise(value = sum(value) / length(FAOSTAT_Hist_Year_FBS), .groups = "drop") %>%
  select(-unit) %>%
  spread(element, value) %>%
  left_join_error_no_match(
    POP %>%
      summarize(POP = sum(value)/1000/ length(FAOSTAT_Hist_Year_FBS), .groups = "drop"),
    by = character()
  ) %>%
  left_join_error_no_match(
    FBS_treemap , by = "APE_comm_Agg"
  ) %>%
  mutate(PcPdKcal = `Dietary energy` / POP / 365,
         PcPdgFat = Fat /POP / 365 * 10^6,
         PcPdgProtein = Protein /POP / 365 * 10^6 )  ->
  DF_Macronutrient_FoodItem


DF_Macronutrient_FoodItem %>%
  summarize(PKcal = sum(`Dietary energy`)/10^6,
            BtFat = sum(Fat) / 1000,
            BtProtein = sum(Protein) / 1000) %>%
  left_join(POP %>% summarize(BPOP = sum(value) / 1000000/ length(FAOSTAT_Hist_Year_FBS)), by = character()) %>%
  mutate(PcPdKcal = PKcal / BPOP / 365 * 1000,
         PcPdgFat = BtFat /BPOP / 365 * 10^6 ,
         PcPdgProtein = BtProtein /BPOP / 365 * 10^6 ) -> WorldSTAT


DF_Macronutrient_FoodItem %>%
  summarize(PKcal = sum(`Dietary energy`)/10^6,
            MtFat = sum(Fat),
            MtProtein = sum(Protein),
            PcPdKcal = sum(PcPdKcal),
            PcPdgFat = sum(PcPdgFat),
            PcPdgProtein = sum(PcPdgProtein)) -> WorldSTAT

# World level by commodity
library(treemap)

paste0("Dietary energy: ",
       round(WorldSTAT$PKcal,0), " Peta-kcal or ", round(WorldSTAT$PcPdKcal, 0)," kcal/ca/d" ) ->
  title
treemap_wrapper(
  .DF = DF_Macronutrient_FoodItem %>%
    mutate(APE_comm_Agg = gsub(" and products", "",APE_comm_Agg)) %>%
    select(Food, APE_comm_Agg, item, value = `Dietary energy`),
  .Depth = 3,
  .FigTitle = "", .FigTitleSize = 14,.Palette = "Set2",
  .SaveDir = "output/gcamfaostat_SciData/",
  .SaveName = "0_Fig3_WorldFoodCalories",
  .SaveScaler = 1, .SVG = TRUE, .SVG_w = 10, .SVG_h = 4
)

treemap_wrapper(
  .DF = DF_Macronutrient_FoodItem %>%
    mutate(APE_comm_Agg = gsub(" and products", "",APE_comm_Agg)) %>%
    select(APE_comm_Agg, item, value = `Dietary energy`),
  .Depth = 2,
  .FigTitle = "", .FigTitleSize = 14,.Palette = "Dark2",
  #.LastLabelCol = "blue",
  .SaveDir = "output/gcamfaostat_SciData/",
  .SaveName = "0_Fig3_WorldFoodCalories1",
  .SaveScaler = 1, .SVG = TRUE, .SVG_w = 10, .SVG_h = 4
)



paste0("Fat: ",
       round(WorldSTAT$MtFat,0), " Mt or ", round(WorldSTAT$PcPdgFat, 0)," g/ca/d" ) ->
  title
treemap_wrapper(
  .DF = DF_Macronutrient_FoodItem %>%
    mutate(APE_comm_Agg = gsub(" and products", "",APE_comm_Agg)) %>%
    select(Food, APE_comm_Agg, item, value = Fat),
  .Depth = 3,
  .FigTitle = "", .FigTitleSize = 14, .Palette = "Dark2",
  .SaveDir = "output/gcamfaostat_SciData/",
  .SaveName = "0_Fig3_WorldFoodFat",
  .SaveScaler = 1, .SVG = TRUE, .SVG_w = 5, .SVG_h = 6
)
treemap_wrapper(
  .DF = DF_Macronutrient_FoodItem %>%
    mutate(APE_comm_Agg = gsub(" and products", "",APE_comm_Agg)) %>%
    select(APE_comm_Agg, item, value = Fat),
  .Depth = 2,
  .FigTitle = "", .FigTitleSize = 14, .Palette = "Dark2",
  #.LastLabelCol = "blue",
  .SaveDir = "output/gcamfaostat_SciData/",
  .SaveName = "0_Fig3_WorldFoodFat1",
  .SaveScaler = 1, .SVG = TRUE, .SVG_w = 5, .SVG_h = 6
)



paste0("Protein: ",
       round(WorldSTAT$MtProtein,0), " Mt or ", round(WorldSTAT$PcPdgProtein, 0)," g/ca/d" ) ->
  title
treemap_wrapper(
  .DF = DF_Macronutrient_FoodItem %>%
    mutate(APE_comm_Agg = gsub(" and products", "",APE_comm_Agg)) %>%
    select(Food, APE_comm_Agg, item, value = Protein),
  .Depth = 3,
  .FigTitle = "", .FigTitleSize = 14, .Palette = "Dark2",
  .SaveDir = "output/gcamfaostat_SciData/",
  .SaveName = "0_Fig3_WorldFoodProtein",
  .SaveScaler = 1, .SVG = TRUE, .SVG_w = 5, .SVG_h = 6
)

treemap_wrapper(
  .DF = DF_Macronutrient_FoodItem %>%
    mutate(APE_comm_Agg = gsub(" and products", "",APE_comm_Agg)) %>%
    select(APE_comm_Agg, item, value = Protein),
  .Depth = 2,
  .FigTitle = "", .FigTitleSize = 14, .Palette = "Dark2",
  #.LastLabelCol = "blue",
  .SaveDir = "output/gcamfaostat_SciData/",
  .SaveName = "0_Fig3_WorldFoodProtein1",
  .SaveScaler = 1, .SVG = TRUE, .SVG_w = 5, .SVG_h = 6
)




# dietary energy and macronutrient




Traceable_FBS_Food_Calorie_Macronutrient_2010_2022 %>%
  gather_years() %>% select(-unit) %>%
  filter(value >0) %>%
  spread(element, value, fill = 0) -> FoodCal

Traceable_FBS_PCe_2010_2022 %>%
  gather_years() %>%
  filter(element == "Food", value >0) %>%
  select(-unit) %>%
  spread(element, value) %>%
  mutate(Food = Food / 1000) %>%
  right_join(FoodCal, by = c("iso", "item", "year")) ->
  FoodCal1

FoodCal1 %>% filter(is.na(Food)) -> A

FoodCal1 %>% filter(item == "Apples and products") %>%
  group_by(year) %>%
  summarize(value = sum(`Dietary energy`))

FoodCal1 %>% filter(!item %in% c("NEC")) %>%
  mutate(`Dietary energy` = `Dietary energy`/10^6) %>%
  mutate(value = `Dietary energy`/Food)  %>%
  ggplot(aes(x = Food, y = `Dietary energy`)) +
  facet_wrap(~item, ncol = 6, scales = "free") +
  geom_smooth(method='lm', formula= y~x) +
  geom_point(alpha = 0.5, size = 1) +
  scale_color_distiller(palette = "YlGn") +
  labs(x= "Food (Mt or 10^12 g)", y = "Dietary energy (tera-kilocalorie or 10^12 kcal)", color = "Year") +
  theme_bw() + theme0 + theme(panel.spacing = unit(0.8, "lines")) -> p;p

p %>% Write_png("Traceable_FBS_DietaryEnergy_All", w = 26, h = 30)


# other checks ----

SUA_Food_Calorie_Macronutrient_2010_2022 %>%
  gather_years() %>%
  filter(iso == "bra", item_code %in% c(SUA_itemcode0), year == 2020  )

SUA_2010_2022 %>%
  filter(iso == "bra", item_code %in% c(512) )

Traceable_FBS_PCe_2010_2022 %>%
  gather_years() %>% filter(item == "Other citrus and products", iso == "bra", year == 2020)
