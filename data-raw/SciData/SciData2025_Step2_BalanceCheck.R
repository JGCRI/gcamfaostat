
# Get population and iso code ----
OA %>%
  filter(element_code == 511, item_code == 3010) %>%
  transmute(area, area_code, year, value) %>%
  filter(year %in% FAOSTAT_Hist_Year_FBS) %>%
  left_join_error_no_match(
    Mapping_gcamdata_FAO_iso_reg %>% distinct(iso, area_code), by = "area_code"
  )->
  POP



# Balance checks ----

## Trade bal ----
Traceable_FBS_PCe_2010_2022 %>%
  gather_years() %>%
  filter(element %in% c("Export", "Import", "Production")) %>%
  group_by_at(vars(-iso, -value)) %>%
  summarize(value = sum(value)) %>% ungroup() %>%
  spread(element, value, fill = 0) ->
  Bal_trade

Bal_trade %>% mutate(diff = abs(Export - Import)) %>%
  summarize(max = max(diff))

# "Livestock equivalent" trade was not included

Bal_trade %>%
  mutate(Export = log(Export),
         Import = log(Import),
         size = (Production/1000)) %>%
  arrange(-year) %>%
  ggplot() +
  geom_abline(intercept = 0, slope = 1) +
  geom_point(aes(x = Export, y = Import, color = year, size = size),
             alpha = 0.3) +
  scale_size_binned(breaks = c(10, 100, 1000),
                    labels=function(x) as.character(round(x, 0))) +
  scale_color_distiller(palette = "YlGnBu", direction = 1) +
  #scale_color_viridis_c() +
  labs(x = "Natural log of export (in thousand tonnes)",
       y = "Natural log of import (in thousand tonnes)",
       fill = "Year", color = "Year", size = "World\nproduction \n(Mt)" ) +
  theme_bw() + theme0 -> p_baltrade; p_baltrade


## Stock Bal----
Traceable_FBS_PCe_2010_2022 %>%
  gather_years() %>%
  filter(element %in% c("Opening stocks", "Closing stocks", "Stock Variation")) %>%
  mutate(value =  replace(value, value < 10^-7, 0 )) %>%
  spread(element, value, fill = 0) %>%
  group_by(iso, item) %>% arrange(year) %>%
  mutate(`Lag closing stocks` = lag(`Closing stocks`)) %>% ungroup() %>%
  filter(!is.na(`Lag closing stocks`)) %>%
  left_join_error_no_match(
    Bal_trade %>% transmute(item, year, size = Production/1000))->
  Bal_stocks

Bal_stocks %>% mutate(diff = abs(`Opening stocks` - `Lag closing stocks`)) %>%
  filter(diff > 0.001)

Bal_stocks %>%
  mutate(`Opening stocks` = log(`Opening stocks`),
         `Lag closing stocks` = log(`Lag closing stocks`))  %>%
  # add a dummy for 2010 value
  bind_rows(
    tibble(iso = "usa", item = "Maize and products", year = 2010,
           `Opening stocks` = 0, `Lag closing stocks` = 0, size = 0)
  ) %>% arrange(item, year) %>%
  ggplot() +
  geom_abline(intercept = 0, slope = 1) +
  geom_point(aes(x = `Lag closing stocks`, y = `Opening stocks`, color = year, size = size),
            alpha = 0.3) +
  scale_color_distiller(palette = "YlGnBu", direction = 1) +
  scale_size_binned(breaks = c(10, 100, 1000),
                    labels=function(x) as.character(round(x, 0))) +
   labs(x = "Natural log of lagged closing stocks (in thousand tonnes)",
        y = "Natural log of opening stocks (in thousand tonnes)",
        fill = "Year", color = "Year", size = "World\nproduction \n(Mt)" ) +
   theme_bw() + theme0 -> p_balstock; p_balstock

library(patchwork)
(p_baltrade + labs(title = "(A) Balance check in world trade") )+
  (p_balstock + labs(title = "(B) Balance check in stocks"))+
  plot_layout(guides = "collect") -> p
p %>% Write_png("Validation_Bal_trade_stocks", w = 14, h = 7)

###* Balance check fig----


## Supply demand bal ----

Traceable_FBS_PCe_2010_2022 %>%
  #filter(!item %in% c("Livestock meat equivalent", "NEC")) %>%
  gather_years() %>%
  filter(element %in% c("Regional demand", "Regional supply",
                        "Production", "Residuals")) %>%
  spread(element, value, fill = 0)  %>%
  filter(!(`Regional demand` == 0 &`Regional supply` ==0)) %>%
  mutate(size = log(Production), size = if_else(Production ==0, -10, size)) %>%
  left_join_error_no_match(
    POP %>%
      select(iso, year, pop = value), by = c("iso", "year")
  ) ->
  Bal_SupplyDemand



Bal_SupplyDemand %>%
  #mutate(Residuals = Residuals / 10^3) %>%
  mutate(ResShare = (Residuals) / pmax(`Regional demand`, `Regional supply`)) %>%
  #mutate(group = if_else(Residuals > 0, "Above", "Below")) %>%
  #filter(item != "Livestock meat equivalent") %>%
  group_by(item) %>%
  summarize(#p_shapiro = shapiro.test(ResShare)$p.value,
            #p_MoodMedianTest = mood.test(Residuals ~ group)$p.value,
            n = n(),
            p_Ttest = t.test(Residuals, mu = 0)$p.value,
            rmse  = sqrt(mean((Residuals)^2)),
            mae = mean(abs(Residuals)),
            mean = mean(Residuals),
            sd = sd(Residuals),
            Q05 = quantile(Residuals,probs = .05),
            Q25 = quantile(Residuals,probs = .25),
            median = median(Residuals),
            Q75 = quantile(Residuals,probs = .75),
            Q95 = quantile(Residuals,probs = .95),
            PearsonSkew = 3*(mean - median)/sd,
            SI = abs(mean - median)/sd) ->
  p_STAT_BySector

p_STAT_BySector %>%
  transmute(Commodity = item, Count = n, `5th` = Q05, `25th` = Q25,
            `50th (median)` = median, `75th` = Q75, `95th` = Q95, Mean = mean, sd,
            `p-value (t-test)` = p_Ttest, MAE = mae, `Symmetry Index` = SI) %>%
  arrange(`Symmetry Index`) %>% readr::write_csv("output/gcamfaostat_SciData/TableBalCheck_SupplyDemand.csv")


Bal_SupplyDemand %>%
  mutate(Residuals = (Residuals) / pmax(`Regional demand`, `Regional supply`)) %>%
  group_by(item) %>%
  summarize(
    n = n(),
    p_Ttest = t.test(Residuals, mu = 0)$p.value,
    rmse  = sqrt(mean((Residuals)^2)),
    mean = mean(Residuals),
    sd = sd(Residuals),
    Q10 = quantile(Residuals,probs = .10),
    Q25 = quantile(Residuals,probs = .25),
    median = median(Residuals),
    Q75 = quantile(Residuals,probs = .75),
    Q90 = quantile(Residuals,probs = .90),
    PearsonSkew = 3*(mean - median)/sd ) ->
  p_STAT_BySector_ResShare

Bal_SupplyDemand %>%
  mutate(Residuals = (Residuals) / pmax(`Regional demand`, `Regional supply`)) %>%
  filter(iso %in% c("usa", "bra", "chn", "ind"), year == 2022) -> ResShare_usa_2022

Bal_SupplyDemand %>%
  group_by(item) %>%
  summarize(`Regional demand` = sum(`Regional demand`),
            `Regional supply` = sum(`Regional supply`),
            Residuals = sum(abs(Residuals) )) %>%
  mutate(Residuals = (Residuals) / pmax(`Regional demand`, `Regional supply`)) ->
  ResShare_world_weighted_abs


p_STAT_BySector_ResShare %>%
  mutate(item = gsub(" and products", "", item)) %>%
  ggplot() +
  geom_boxplot(
    aes(x = reorder(item, sd), ymin = Q10, lower = Q25,  middle = median,  upper = Q75, ymax = Q90),
    stat = "identity") +
  geom_jitter(data = ResShare_usa_2022 %>% mutate(item = gsub(" and products", "", item)),
              aes(x = item, y = Residuals , fill = iso),
              alpha = .7, width = .25, shape = 21, size = 2) +
  geom_text(aes(x = 1, y = -1, label = "World weighted absolute residual share (%)"),
            hjust = 0, size = 5, color = "blue", fontface = 3) +
  geom_text(data = ResShare_world_weighted_abs %>% mutate(item = gsub(" and products", "", item)),
            aes(x = item, y = -1.1, label = round(Residuals*100, 0)),
            hjust = 0.5, size = 4, color = "blue", fontface = 3 ) +
  geom_text(aes(x = 1, y = 1, label = "Median residual share (%)"),
            hjust = 0, size = 5, color = "black", fontface = 3) +
  geom_text(data = p_STAT_BySector_ResShare %>% mutate(item = gsub(" and products", "", item)),
            aes(x = item, y = 1.1, label = round(median*100, 0)),
            hjust = 0.5, size = 4, color = "black", fontface = 3 ) +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_brewer(palette = "Set1") +
  labs(x = "Commodity", y = "Share") +
  theme_bw() + theme0 +
  theme(axis.text.x = element_text(angle = 50, hjust = 1, vjust = 1, size = 12)) -> p;p

p %>% Write_png("Validation_Bal_ResShare_Comm", w = 14, h = 8)




Bal_SupplyDemand %>%
  mutate(Residuals = abs(Residuals) / pmax(`Regional demand`, `Regional supply`)) %>%
  group_by(year) %>%
  summarize(
    n = n(),
    p_Ttest = t.test(Residuals, mu = 0)$p.value,
    rmse  = sqrt(mean((Residuals)^2)),
    mean = mean(Residuals),
    sd = sd(Residuals),
    Q10 = quantile(Residuals,probs = .10),
    Q25 = quantile(Residuals,probs = .25),
    median = median(Residuals),
    Q75 = quantile(Residuals,probs = .75),
    Q90 = quantile(Residuals,probs = .90),
    PearsonSkew = 3*(mean - median)/sd ) ->
  p_STAT_ByYear_ResShare

Bal_SupplyDemand %>%
  mutate(Residuals = abs(Residuals) / pmax(`Regional demand`, `Regional supply`)) %>%
  filter(iso %in% c("usa", "bra", "chn", "ind")) %>%
  filter(item %in% c("Maize and products",
                     "Rice and products",
                     "Wheat and products")) -> ResShare_example

Bal_SupplyDemand %>%
  group_by(year) %>%
  summarize(`Regional demand` = sum(`Regional demand`),
            `Regional supply` = sum(`Regional supply`),
            Residuals = sum(abs(Residuals) )) %>%
  mutate(Residuals = (Residuals) / pmax(`Regional demand`, `Regional supply`)) ->
  ResShare_world_weighted_abs


p_STAT_ByYear_ResShare %>%
  select(year, Q10, Q25, median, Q75, Q90) %>%
  tidyr::gather(stat, value, -year) %>%
  group_by(stat) %>% #filter(stat == "Q90") %>%
  summarize(regression_results  = list(broom::tidy(lm(value ~ year, data =cur_data())))) %>%
  tidyr::unnest(regression_results)

p_STAT_ByYear_ResShare %>%
  mutate(year = as.integer(year)) %>%
  ggplot() +
  geom_hline(yintercept = 0) +
  geom_boxplot(
    aes(x = year, ymin = Q10, lower = Q25,  middle = median,  upper = Q75, ymax = Q90, group = year),
    stat = "identity") +
  geom_smooth(data = p_STAT_ByYear_ResShare %>%
                select(year, Q75, Q90) %>%
                tidyr::gather(stat, value, -year) %>%
                mutate(stat = factor(stat,
                                     levels = c("Q90", "Q75"),
                                     labels = c("90th quantile",
                                                "75th quantile") )),
              aes(x = year, y = value, group = stat, color = stat),
              method = "lm", level = 0.95, alpha = 0.2) +
  geom_jitter(data = ResShare_example,
              aes(x = year, y = Residuals , fill = item),
              alpha = .7, width = .25, shape = 21, size = 2) +
  geom_text(aes(x = 2010, y = .94, label = "World weighted absolute residual share (%)"),
            hjust = 0, size = 5, color = "blue", fontface = 3) +
  geom_text(data = ResShare_world_weighted_abs,
            aes(x = year, y = .89, label = round(Residuals*100, 0)),
            hjust = 0.5, size = 5, color = "blue", fontface = 3 ) +
  geom_text(aes(x = 2010, y = .83, label = "Median residual share (%)"),
            hjust = 0, size = 5, color = "black", fontface = 3) +
  geom_text(data = p_STAT_ByYear_ResShare,
            aes(x = year, y = .78, label = round(median*100, 0)),
            hjust = 0.5, size = 5, color = "black", fontface = 3 ) +
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(n.breaks = 10) +
  scale_fill_brewer(palette = "Set3") +
  labs(x = "Year", y = "Share", fill = "Commodity", color = "Fitted line") +
  theme_bw() + theme0  -> p;p

p %>% Write_png("Validation_Bal_ResShare_Year", w = 11, h = 7)





Bal_SupplyDemand %>%
  # mutate(`Regional demand` = `Regional demand` / pop * 10^6,
  #        `Regional supply` = `Regional supply` / pop * 10^6) %>%
  mutate(`Regional demand` = log(`Regional demand`),
         `Regional supply` = log(`Regional supply`))  %>%
  ggplot() +
  geom_abline(intercept = 0, slope = 1) +
  geom_point(aes(x = `Regional supply`, y = `Regional demand`, color = year, #size = pop/1000
                 ), alpha = 0.3) +
  scale_color_distiller(palette = "YlGnBu", direction = 1) + coord_fixed() +
  scale_x_continuous(limits = c(-15, 15)) +
  scale_y_continuous(limits = c(-15, 15)) +
  # scale_size_binned(breaks = c(0, 50, 500),
  #                   labels=function(x) as.character(round(x, 0)),
  #                   range = c(0.2, 5) ) +
  labs(x = "Natural log of regional supply (in thousand tonnes)",
       y = "Natural log of regional demand (in thousand tonnes)",
       fill = "Year", color = "Year", size = "Population \n(Million)" ) +
  theme_bw() + theme0 -> p_balSD; p_balSD
p_balSD %>% Write_png("Validation_Bal_supply_demand", w = 8, h = 7)





Bal_SupplyDemand %>%
  mutate(`Regional demand` = log(`Regional demand`),
         `Regional supply` = log(`Regional supply`))  %>%
  ggplot() + facet_wrap(~item, ncol = 6) +
  geom_abline(intercept = 0, slope = 1) +
  geom_point(aes(x = `Regional supply`, y = `Regional demand`, color = year,
  ), alpha = 0.9) +
  scale_color_distiller(palette = "YlGnBu", direction = 1) + #coord_fixed() +
    # scale_x_continuous(limits = c(-20, 20)) +
    # scale_y_continuous(limits = c(-20, 20)) +
  labs(x = "Natural log of regional supply (in thousand tonnes)",
       y = "Natural log of regional demand (in thousand tonnes)",
       fill = "Year", color = "Year" ) +
  theme_bw() + theme0 -> p_balSD; p_balSD
p_balSD %>% Write_png("Validation_Bal_supply_demand_ByItem", w = 26, h = 28)



Bal_SupplyDemand %>%
  mutate(`Regional demand` = log(`Regional demand`),
         `Regional supply` = log(`Regional supply`))  %>%
  ggplot() + facet_wrap(~iso, ncol = 14) +
  geom_abline(intercept = 0, slope = 1) +
  geom_point(aes(x = `Regional supply`, y = `Regional demand`,
                 color = year), alpha = 0.9) +
  scale_color_distiller(palette = "YlGn", direction = 1) +
  labs(x = "Natural log of regional supply (in thousand tonnes)",
       y = "Natural log of regional demand (in thousand tonnes)",
       fill = "Year", color = "Year" ) +
  theme_bw() + theme0 -> p_balSD; p_balSD

p_balSD %>% Write_png("Validation_Bal_supply_demand_ByISO", w = 28, h = 30)


