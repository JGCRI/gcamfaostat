


# 1. How many data points ----

Nested_Mapping_SUA_To_Traceable_FBS %>%
  select(aggregated_PCe_item, item = source_item) %>%
  bind_rows(Nested_Mapping_SUA_To_Traceable_FBS %>%
              select(aggregated_PCe_item, item = sink_item)) %>%
  distinct() ->
  SUA_Items_APE


SUA_Items_APE %>% filter(aggregated_PCe_item != "NEC") %>%
  group_by(aggregated_PCe_item) %>%
  summarize(n = n()) %>% arrange(n) %>%
  ggplot() +
  geom_bar(aes(y = reorder(aggregated_PCe_item, n), x = n,  fill = n),
           color = "black", size = 0.5,
           stat = "identity") +
  scale_x_continuous(expand = c(0, 0)) +
  scale_fill_distiller(palette = "BuPu", direction = 1) +
  labs(x = "Count", y = "PCe Commodity", fill = "Count") +
  theme_bw() + theme0 + theme_leg +
  theme( legend.position = c(.85, .25))-> p;p

# * PCe_COMM_mapping ----
p %>% Write_png("0_Fig5_PCe_COMM_mapping", w = 8, h = 14)

# 2. Traceable FBS; What are the datasets we publish? ----

## 2.0 All PCe Commodities ----
Traceable_FBS_PCe_2010_2022 %>%
  filter(!item %in% c("NEC", "Livestock meat equivalent", "Other fiber crops and products" )) %>%
  gather_years() %>%
  group_by(item, year, element) %>%
  summarize(value = sum(value), .groups = "drop") %>%
  mutate(value = value / 1000) %>%
  mutate(value = if_else(
    element %in% c("Opening stocks", "Production", "Import"),
    -value, value)) -> df0

  df0 %>%
    filter(!element %in% c("Residuals", "Regional supply", "Regional demand", "Stock Variation")) %>%
    mutate(element = factor(element, levels = c(RegDemendEle, rev(RegSupplyEle)) )) ->
  df

df %>%
  ggplot() +
  facet_wrap(~item, scales = "free_y", ncol = 5) +
  geom_bar(aes(x = year, y = value, fill = element),
           stat = "identity", size = 0.3, color = "black") +
  geom_hline(yintercept = 0, color = "grey20", linetype = 1, size = 1) +
  geom_point(data = df0 %>% filter(element == "Residuals"),
             aes(x = year, y = value, color = element, shape = element),
             size = 1, fill = "blue", color = "black") +
  scale_fill_manual(values = Bar_Col) +
  scale_shape_manual(values = 21) +
  scale_x_continuous(breaks = seq(2010, 2022, 4)) +
  scale_y_continuous(n.breaks = 6) +
  labs(x= "Year", y = "Mt", fill = "SUA element", shape = "") +
  theme_bw() + theme0 +
  guides(fill = guide_legend(order = 1, ncol = 2),
         shape = guide_legend(order = 2)) -> p_AllItem


reposition_legend(p_AllItem +
                    theme(legend.spacing.y = unit(0.5, 'lines'),
                          legend.box.margin=margin(30,10,0,10)) ,
                  'center',
                  panel = 'panel-5-11') -> p_AllItem1

## * World FBS ----
p_AllItem1 %>% Write_png("0_Fig2_tFBS_PCe_AllItems", w = 24, h = 31)


## 2.1 Rice ----
df0 %>% filter(item == "Rice and products") -> df0_1
df %>% filter(item == "Rice and products") -> df_1

df_1 %>%
  filter(year == max(df_1$year)) %>%
  mutate(element = factor(element, levels = c(rev(RegDemendEle), RegSupplyEle))) %>%
  mutate(Pos = if_else(value >0, 1, -1)) %>% arrange(element) %>%
  group_by(Pos) %>%
  mutate(ycum = cumsum(value), ycum_lag = lag(ycum)) %>% replace_na(list(ycum_lag = 0)) %>%
  mutate(y = 0.5* (ycum + ycum_lag), yend = cumsum(Pos) * 120 - Pos * 50) %>%
  mutate(x = max(df_1$year) + 0.5, xend = max(df_1$year) + 1.2) %>% ungroup() %>%
  mutate(diff_y = abs(yend - y),
         curvature =  (1- (diff_y / max(diff_y)) + 0.1)^2 * .2,
         curvature = 0.1,
         curvature = if_else(y < 0, -curvature, curvature)) ->
  df_2

df_1 %>%
  ggplot() +
  guides(shape = guide_legend(order = 1),
         fill = "none",
         color = "none") +
  geom_bar(aes(x = year, y = value, fill = element),
           stat = "identity", size = 0.6, color = "black") +
  geom_hline(yintercept = 0, color = "grey20", linetype = 1, size = 1) +
  geom_point(data = df0_1 %>% filter(element == "Residuals"),
             aes(x = year, y = value, color = element, shape = element),
             size = 4, fill = "blue", color = "black") +
  geom_text(aes(x= unique(df_2$xend), y = max(df_2$yend) + 120, label = "SUA elements"),
            hjust = 0, size = 6, fontface = "bold") +
  geom_text(data = df_2,
            aes(x = xend, y = yend, label = element, color = element
            ),  #position = position_stack(vjust = 0.5),
            hjust = 0, size = 6) +
  lapply(split(df_2, 1:nrow(df_2)), function(dat) {
    geom_curve(data = dat, aes(x = x, y = y, xend = xend-0.05, yend = yend, color = element
    ),
    curvature = dat["curvature"],
    ncp = 10, angle = 90, size = 0.8) } ) +
  scale_fill_manual(values = Bar_Col) +
  scale_color_manual(values = Bar_Col, guide = "none") +
  scale_shape_manual(values = 21) +
  labs(x= "Year", y = "Mt", fill = "SUA element", shape = "",
       title = "(A) T-FBS: Rice and products") +
  theme_bw() + theme0 +
  scale_x_continuous(breaks = seq(2010, 2022, 2)) +
  scale_y_continuous(n.breaks = 8) +
  coord_cartesian(
    clip = "off", xlim = c(2010, 2025.5)
  ) +
  theme_leg +
  theme(
    #legend.position = "none",
    legend.position = c(.955, .28),
    legend.justification = c("right", "top"),
    legend.key.size = unit(1, "cm"),
    #legend.key.height=unit(1.5,"line"),
    legend.spacing.x = unit(1, 'cm'), #legend.spacing.y = unit(5, 'cm'),
    legend.text = element_text(margin = margin(l = -25,t=0, b=0), size = 16),
    legend.box.margin=margin(-10, 10,-8,10),
    legend.background = element_blank(),
    plot.margin =  margin(t = 10, r = 25, b = 10, l = 10)
  ) -> p_RICE_PCe;p_RICE_PCe

## * Rice PCe ----
# p_RICE_PCe %>% Write_png("PCe_Rice_World", w = 12, h = 8)



## 2.2 Seed cotton ----
df0 %>% filter(item == "Seed cotton and products") -> df0_1
df %>% filter(item == "Seed cotton and products") -> df_1

df_1 %>%
  filter(year == max(df_1$year)) %>%
  mutate(element = factor(element, levels = c(rev(RegDemendEle), RegSupplyEle))) %>%
  mutate(Pos = if_else(value >0, 1, -1)) %>% arrange(element) %>%
  group_by(Pos) %>%
  mutate(ycum = cumsum(value), ycum_lag = lag(ycum)) %>% replace_na(list(ycum_lag = 0)) %>%
  mutate(y = 0.5* (ycum + ycum_lag), yend = cumsum(Pos) * 8 - Pos * 3) %>%
  mutate(x = max(df_1$year) + 0.5, xend = max(df_1$year) + 1.2) %>% ungroup() %>%
  mutate(diff_y = abs(yend - y),
         curvature =  (1- (diff_y / max(diff_y)) + 0.1)^2 * .2,
         curvature = 0.1,
         curvature = if_else(y < 0, -curvature, curvature)) ->
  df_3

df_1 %>%
  ggplot() +
  guides(shape = guide_legend(order = 1),
         fill = "none",
         color = "none") +
  geom_bar(aes(x = year, y = value, fill = element),
           stat = "identity", size = 0.6, color = "black") +
  geom_hline(yintercept = 0, color = "grey20", linetype = 1, size = 1) +
  geom_point(data = df0_1 %>% filter(element == "Residuals"),
             aes(x = year, y = value, color = element, shape = element),
             size = 4, fill = "blue", color = "black") +
  geom_text(aes(x= unique(df_3$xend), y = max(df_3$yend) + 12, label = "SUA elements"),
            hjust = 0, size = 6, fontface = "bold") +
  geom_text(data = df_3,
            aes(x = xend, y = yend, label = element, color = element
            ),  #position = position_stack(vjust = 0.5),
            hjust = 0, size = 6) +
  lapply(split(df_3, 1:nrow(df_3)), function(dat) {
    geom_curve(data = dat, aes(x = x, y = y, xend = xend-0.05, yend = yend, color = element
    ),
    curvature = dat["curvature"],
    ncp = 10, angle = 90, size = 0.8) } ) +
  scale_fill_manual(values = Bar_Col) +
  scale_color_manual(values = Bar_Col, guide = "none") +
  scale_shape_manual(values = 21) +
  labs(x= "Year", y = "Mt", fill = "SUA element", shape = "",
       title = "(A) T-FBS: Seed cotton and products") +
  theme_bw() + theme0 +
  scale_x_continuous(breaks = seq(2010, 2022, 2)) +
  scale_y_continuous(n.breaks = 8) +
  coord_cartesian(
    clip = "off", xlim = c(2010, 2025.5)
  ) +
  theme_leg +
  theme(
    #legend.position = "none",
    legend.position = c(.955, .28),
    legend.justification = c("right", "top"),
    legend.key.size = unit(1, "cm"),
    #legend.key.height=unit(1.5,"line"),
    legend.spacing.x = unit(1, 'cm'), #legend.spacing.y = unit(5, 'cm'),
    legend.text = element_text(margin = margin(l = -25,t=0, b=0), size = 16),
    legend.box.margin=margin(-10, 10,-8,10),
    legend.background = element_blank(),
    plot.margin =  margin(t = 10, r = 25, b = 10, l = 10)
  ) -> p_COTTON_PCe;p_COTTON_PCe

## * Cotton PCe ----
#p_COTTON_PCe %>% Write_png("PCe_SeedCotton_World", w = 12, h = 8)


## 2.3 Maize ----
df0 %>% filter(item == "Maize and products") -> df0_1
df %>% filter(item == "Maize and products") -> df_1

df_1 %>%
  filter(year == max(df_1$year)) %>%
  mutate(element = factor(element, levels = c(rev(RegDemendEle), RegSupplyEle))) %>%
  mutate(Pos = if_else(value >0, 1, -1)) %>% arrange(element) %>%
  group_by(Pos) %>%
  mutate(ycum = cumsum(value), ycum_lag = lag(ycum)) %>% replace_na(list(ycum_lag = 0)) %>%
  mutate(y = 0.5* (ycum + ycum_lag), yend = cumsum(Pos) * 150 - Pos * 50) %>%
  mutate(x = max(df_1$year) + 0.5, xend = max(df_1$year) + 1.2) %>% ungroup() %>%
  mutate(diff_y = abs(yend - y),
         curvature =  (1- (diff_y / max(diff_y)) + 0.1)^2 * .2,
         curvature = 0.1,
         curvature = if_else(y < 0, -curvature, curvature)) ->
  df_4

df_1 %>%
  ggplot() +
  guides(shape = guide_legend(order = 1),
         fill = "none",
         color = "none") +
  geom_bar(aes(x = year, y = value, fill = element),
           stat = "identity", size = 0.6, color = "black") +
  geom_hline(yintercept = 0, color = "grey20", linetype = 1, size = 1) +
  geom_point(data = df0_1 %>% filter(element == "Residuals"),
             aes(x = year, y = value, color = element, shape = element),
             size = 4, fill = "blue", color = "black") +
  geom_text(aes(x= unique(df_4$xend), y = max(df_4$yend) + 180, label = "SUA elements"),
            hjust = 0, size = 6, fontface = "bold") +
  geom_text(data = df_4,
            aes(x = xend, y = yend, label = element, color = element
            ),  #position = position_stack(vjust = 0.5),
            hjust = 0, size = 6) +
  lapply(split(df_4, 1:nrow(df_4)), function(dat) {
    geom_curve(data = dat, aes(x = x, y = y, xend = xend-0.05, yend = yend, color = element
    ),
    curvature = dat["curvature"],
    ncp = 10, angle = 90, size = 0.8) } ) +
  scale_fill_manual(values = Bar_Col) +
  scale_color_manual(values = Bar_Col, guide = "none") +
  scale_shape_manual(values = 21) +
  labs(x= "Year", y = "Mt", fill = "SUA element", shape = "",
       title = "(A) T-FBS: Maize and products") +
  theme_bw() + theme0 +
  scale_x_continuous(breaks = seq(2010, 2022, 2)) +
  scale_y_continuous(n.breaks = 8) +
  coord_cartesian(
    clip = "off", xlim = c(2010, 2025.5)
  ) +
  theme_leg +
  theme(
    #legend.position = "none",
    legend.position = c(.955, .28),
    legend.justification = c("right", "top"),
    legend.key.size = unit(1, "cm"),
    #legend.key.height=unit(1.5,"line"),
    legend.spacing.x = unit(1, 'cm'), #legend.spacing.y = unit(5, 'cm'),
    legend.text = element_text(margin = margin(l = -25,t=0, b=0), size = 16),
    legend.box.margin=margin(-10, 10,-8,10),
    legend.background = element_blank(),
    plot.margin =  margin(t = 10, r = 25, b = 10, l = 10)
  ) -> p_MAIZE_PCe;p_MAIZE_PCe

## * Maize PCe ----
# p_MAIZE_PCe %>% Write_png("PCe_Maize_World", w = 12, h = 8)


## 2.4 Wheat PCe ----
df0 %>% filter(item == "Wheat and products") -> df0_1
df %>% filter(item == "Wheat and products") -> df_1

df_1 %>%
  filter(year == max(df_1$year)) %>%
  mutate(element = factor(element, levels = c(rev(RegDemendEle), RegSupplyEle))) %>%
  mutate(Pos = if_else(value >0, 1, -1)) %>% arrange(element) %>%
  group_by(Pos) %>%
  mutate(ycum = cumsum(value), ycum_lag = lag(ycum)) %>% replace_na(list(ycum_lag = 0)) %>%
  mutate(y = 0.5* (ycum + ycum_lag), yend = cumsum(Pos) * 150 - Pos * 50) %>%
  mutate(x = max(df_1$year) + 0.5, xend = max(df_1$year) + 1.2) %>% ungroup() %>%
  mutate(diff_y = abs(yend - y),
         curvature =  (1- (diff_y / max(diff_y)) + 0.1)^2 * .2,
         curvature = 0.1,
         curvature = if_else(y < 0, -curvature, curvature)) ->
  df_5

df_1 %>%
  ggplot() +
  guides(shape = guide_legend(order = 1),
         fill = "none",
         color = "none") +
  geom_bar(aes(x = year, y = value, fill = element),
           stat = "identity", size = 0.6, color = "black") +
  geom_hline(yintercept = 0, color = "grey20", linetype = 1, size = 1) +
  geom_point(data = df0_1 %>% filter(element == "Residuals"),
             aes(x = year, y = value, color = element, shape = element),
             size = 4, fill = "blue", color = "black") +
  geom_text(aes(x= unique(df_5$xend), y = max(df_5$yend) + 180, label = "SUA elements"),
            hjust = 0, size = 6, fontface = "bold") +
  geom_text(data = df_5,
            aes(x = xend, y = yend, label = element, color = element
            ),  #position = position_stack(vjust = 0.5),
            hjust = 0, size = 6) +
  lapply(split(df_5, 1:nrow(df_5)), function(dat) {
    geom_curve(data = dat, aes(x = x, y = y, xend = xend-0.05, yend = yend, color = element
    ),
    curvature = dat["curvature"],
    ncp = 10, angle = 90, size = 0.8) } ) +
  scale_fill_manual(values = Bar_Col) +
  scale_color_manual(values = Bar_Col, guide = "none") +
  scale_shape_manual(values = 21) +
  labs(x= "Year", y = "Mt", fill = "SUA element", shape = "",
       title = "(A) T-FBS: Wheat and products") +
  theme_bw() + theme0 +
  scale_x_continuous(breaks = seq(2010, 2022, 2)) +
  scale_y_continuous(n.breaks = 8) +
  coord_cartesian(
    clip = "off", xlim = c(2010, 2025.5)
  ) +
  theme_leg +
  theme(
    #legend.position = "none",
    legend.position = c(.955, .28),
    legend.justification = c("right", "top"),
    legend.key.size = unit(1, "cm"),
    #legend.key.height=unit(1.5,"line"),
    legend.spacing.x = unit(1, 'cm'), #legend.spacing.y = unit(5, 'cm'),
    legend.text = element_text(margin = margin(l = -25,t=0, b=0), size = 16),
    legend.box.margin=margin(-10, 10,-8,10),
    legend.background = element_blank(),
    plot.margin =  margin(t = 10, r = 25, b = 10, l = 10)
  ) -> p_WHEAT_PCe;p_WHEAT_PCe

## * Wheat PCe ----
# p_WHEAT_PCe %>% Write_png("PCe_Wheat_World", w = 12, h = 8)




# 3 SUA ----

SUA_2010_2022 %>%
  gather_years() %>%
  group_by(item, item_code, year, element) %>%
  summarize(value = sum(value), .groups = "drop") %>%
  mutate(value = value / 1000) %>%
  mutate(value = if_else(
    element %in% c("Opening stocks", "Production", "Import"),
    -value, value)) -> df0

  df0 %>%
    filter(!element %in% c("Residuals", "Regional supply", "Regional demand", "Stock Variation")) %>%
    mutate(element = factor(element, levels = c(RegDemendEle, rev(RegSupplyEle)) )) ->
    df


  # SUA: Rice and products ----

  Nested_Mapping_SUA_To_Traceable_FBS %>%
    filter(aggregated_PCe_item == "Rice and products") %>%
    select(source_item_code, sink_item_code) %>% unlist() %>% unique() ->
    SUA_itemcode0

  Nested_Mapping_SUA_To_Traceable_FBS %>%
    filter(aggregated_PCe_item == "Rice and products") %>%
    select(source_item, sink_item) %>% unlist() %>% unique() ->
    SUA_item0

  df0 %>% filter(item_code %in% SUA_itemcode0)%>%
    filter(item != "Rice") %>%
    mutate(item = factor(item, levels = SUA_item0)) -> df0_1
  df %>% filter(item_code %in% SUA_itemcode0) %>%
    filter(item != "Rice") %>%
    mutate(item = factor(item, levels = SUA_item0))-> df_1

  df_1 %>%
    ggplot() +
    facet_wrap(~item, scales = "free_y", ncol = 4) +
    geom_bar(aes(x = year, y = value, fill = element),
             stat = "identity", size = 0.3, color = "black") +
    geom_hline(yintercept = 0, color = "grey20", linetype = 1, size = 1) +
    geom_point(data = df0_1 %>% filter(element == "Residuals"),
               aes(x = year, y = value, color = element, shape = element),
               size = 3, fill = "blue", color = "black") +
    scale_fill_manual(values = Bar_Col) +
    scale_shape_manual(values = 21) +
    scale_x_continuous(breaks = seq(2010, 2022, 4)) +
    scale_y_continuous(n.breaks = 8) +
    labs(x= "Year", y = "Mt", fill = "SUA element", shape = "",
         title = "(C) SUA: Processed products") +
    theme_bw() + theme0 + theme(panel.spacing = unit(0.8, "lines")) +
    guides(fill = guide_legend(order = 1),
           shape = guide_legend(order = 2, title = NULL)) -> p_RICE_SUA


  df0 %>% filter(item_code %in% SUA_itemcode0)%>%
    filter(item == "Rice") %>%
    mutate(item = factor(item, levels = SUA_item0)) -> df0_1
  df %>% filter(item_code %in% SUA_itemcode0) %>%
    filter(item == "Rice") %>%
    mutate(item = factor(item, levels = SUA_item0))-> df_1

  df_1 %>%
    ggplot() +
    facet_wrap(~item) +
    geom_bar(aes(x = year, y = value, fill = element),
             stat = "identity", size = 0.6, color = "black") +
    geom_hline(yintercept = 0, color = "grey20", linetype = 1, size = 1) +
    geom_point(data = df0_1 %>% filter(element == "Residuals"),
               aes(x = year, y = value, color = element, shape = element),
               size = 4, fill = "blue", color = "black") +
    scale_fill_manual(values = Bar_Col) +
    scale_shape_manual(values = 21) +
    scale_x_continuous(breaks = seq(2010, 2022, 2)) +
    scale_y_continuous(n.breaks = 8) +
    labs(x= "Year", y = "Mt", fill = "SUA element", shape = "",
         title = "(B) SUA: Primary commodity") +
    theme_bw() + theme0 +
    #theme(panel.border = element_rect(colour = "black", size=2))+
    guides(fill = guide_legend(order = 1),
           shape = guide_legend(order = 2)) -> p_RICE_Primary;p_RICE_Primary




  # Arrange the top row (side-by-side)
  top_row <- plot_grid(p_RICE_PCe +
                         theme(legend.position = "none",
                               plot.title = element_text(size = 22, margin = margin(b = 15))),
                       p_RICE_Primary +
                         theme(legend.position = "none",
                               plot.title = element_text(size = 22, margin = margin(b = 15))),
                       rel_widths = c(1.45, 1),
                       ncol = 2)

  reposition_legend(p_RICE_SUA +
                      theme(plot.title = element_text(size = 22, margin = margin(b = 15)),
                            legend.spacing.y = unit(0.5, 'lines'),
                            legend.box.margin=margin(30,10,0,10),
                            #legend.box.background = element_rect(color = "black", size = 0.6 )
                            ) +
                      facet_wrap(~item, scales = "free_y", ncol = 4),
                    'center',
                    panel = 'panel-4-3') -> p_RICE_SUA1


  # Add the bottom plot
  combined_plot <- plot_grid(top_row,
                             p_RICE_SUA1,
                             ncol = 1, rel_heights = c(1, 1.8))

  # * Rice FBS SUA ----
  combined_plot %>% Write_png("0_Fig1_tFBS_SUA_Rice", w = 18, h = 22)




  # SUA: Seed cotton and products ----
  Nested_Mapping_SUA_To_Traceable_FBS %>%
    filter(aggregated_PCe_item == "Seed cotton and products") %>%
    select(source_item_code, sink_item_code) %>% unlist() %>% unique() ->
    SUA_itemcode0

  Nested_Mapping_SUA_To_Traceable_FBS %>%
    filter(aggregated_PCe_item == "Seed cotton and products") %>%
    select(source_item, sink_item) %>% unlist() %>% unique() ->
    SUA_item0

  df0 %>% filter(item_code %in% SUA_itemcode0)%>%
    filter(item != "Seed cotton, unginned") %>%
    mutate(item = factor(item, levels = SUA_item0)) -> df0_1
  df %>% filter(item_code %in% SUA_itemcode0) %>%
    filter(item != "Seed cotton, unginned") %>%
    mutate(item = factor(item, levels = SUA_item0))-> df_1


  df_1 %>%
    ggplot() +
    facet_wrap(~item, scales = "free_y", ncol = 4) +
    geom_bar(aes(x = year, y = value, fill = element),
             stat = "identity", size = 0.3, color = "black") +
    geom_hline(yintercept = 0, color = "grey20", linetype = 1, size = 1) +
    geom_point(data = df0_1 %>% filter(element == "Residuals"),
               aes(x = year, y = value, color = element, shape = element),
               size = 3, fill = "blue", color = "black") +
    scale_fill_manual(values = Bar_Col) +
    scale_shape_manual(values = 21) +
    scale_x_continuous(breaks = seq(2010, 2022, 4)) +
    scale_y_continuous(n.breaks = 8) +
    labs(x= "Year", y = "Mt", fill = "SUA element", shape = "",
         title = "(C) SUA: Processed products") +
    theme_bw() + theme0 + theme(panel.spacing = unit(0.8, "lines")) +
    guides(fill = guide_legend(order = 1),
           shape = guide_legend(order = 2, title = NULL)) -> p_COTTON_SUA



     df0 %>% filter(item_code %in% SUA_itemcode0)%>%
       filter(item == "Seed cotton, unginned") %>%
       mutate(item = factor(item, levels = SUA_item0)) -> df0_1
     df %>% filter(item_code %in% SUA_itemcode0) %>%
       filter(item == "Seed cotton, unginned") %>%
       mutate(item = factor(item, levels = SUA_item0))-> df_1

     df_1 %>%
       ggplot() +
       facet_wrap(~item) +
       geom_bar(aes(x = year, y = value, fill = element),
                stat = "identity", size = 0.6, color = "black") +
       geom_hline(yintercept = 0, color = "grey20", linetype = 1, size = 1) +
       geom_point(data = df0_1 %>% filter(element == "Residuals"),
                  aes(x = year, y = value, color = element, shape = element),
                  size = 4, fill = "blue", color = "black") +
       scale_fill_manual(values = Bar_Col) +
       scale_shape_manual(values = 21) +
       scale_x_continuous(breaks = seq(2010, 2022, 2)) +
       scale_y_continuous(n.breaks = 8) +
       labs(x= "Year", y = "Mt", fill = "SUA element", shape = "",
            title = "(B) SUA: Primary commodity") +
       theme_bw() + theme0 +
       #theme(panel.border = element_rect(colour = "black", size=2))+
       guides(fill = guide_legend(order = 1),
              shape = guide_legend(order = 2)) -> p_COTTON_Primary;p_COTTON_Primary

  # * Cotton SUA ----

     # Arrange the top row (side-by-side)
     top_row <- plot_grid(p_COTTON_PCe +
                            theme(legend.position = "none",
                                  plot.title = element_text(size = 22, margin = margin(b = 15))),
                          p_COTTON_Primary +
                            theme(legend.position = "none",
                                  plot.title = element_text(size = 22, margin = margin(b = 15))),
                          rel_widths = c(1.45, 1),
                          ncol = 2)

     reposition_legend(p_COTTON_SUA +
                         theme(plot.title = element_text(size = 22, margin = margin(b = 15)),
                               legend.spacing.y = unit(0.5, 'lines'),
                               legend.box.margin=margin(20,10,0,10),
                               #legend.box.background = element_rect(color = "black", size = 0.6 )
                         ) +
                         facet_wrap(~item, scales = "free_y", ncol = 3),
                       'center',
                       panel = 'panel-3-2') -> p_COTTON_SUA1


     # Add the bottom plot
     combined_plot <- plot_grid(top_row,
                                p_COTTON_SUA1,
                                ncol = 1, rel_heights = c(1, 1.6))


     combined_plot %>% Write_png("0_Fig1S_tFBS_SUA_Cotton", w = 18, h = 20)


     # SUA: Maize and products ----
     Nested_Mapping_SUA_To_Traceable_FBS %>%
       filter(aggregated_PCe_item == "Maize and products") %>%
       select(source_item_code, sink_item_code) %>% unlist() %>% unique() ->
       SUA_itemcode0

     Nested_Mapping_SUA_To_Traceable_FBS %>%
       filter(aggregated_PCe_item == "Maize and products") %>%
       select(source_item, sink_item) %>% unlist() %>% unique() ->
       SUA_item0

     df0 %>% filter(item_code %in% SUA_itemcode0)%>%
       filter(item != "Maize (corn)") %>%
       mutate(item = factor(item, levels = SUA_item0)) -> df0_1
     df %>% filter(item_code %in% SUA_itemcode0) %>%
       filter(item != "Maize (corn)") %>%
       mutate(item = factor(item, levels = SUA_item0))-> df_1


     df_1 %>%
       ggplot() +
       facet_wrap(~item, scales = "free_y", ncol = 4) +
       geom_bar(aes(x = year, y = value, fill = element),
                stat = "identity", size = 0.3, color = "black") +
       geom_hline(yintercept = 0, color = "grey20", linetype = 1, size = 1) +
       geom_point(data = df0_1 %>% filter(element == "Residuals"),
                  aes(x = year, y = value, color = element, shape = element),
                  size = 3, fill = "blue", color = "black") +
       scale_fill_manual(values = Bar_Col) +
       scale_shape_manual(values = 21) +
       scale_x_continuous(breaks = seq(2010, 2022, 4)) +
       scale_y_continuous(n.breaks = 8) +
       labs(x= "Year", y = "Mt", fill = "SUA element", shape = "",
            title = "(C) SUA: Processed products") +
       theme_bw() + theme0 + theme(panel.spacing = unit(0.8, "lines")) +
       guides(fill = guide_legend(order = 1),
              shape = guide_legend(order = 2, title = NULL)) -> p_MAIZE_SUA



     df0 %>% filter(item_code %in% SUA_itemcode0)%>%
       filter(item == "Maize (corn)") %>%
       mutate(item = factor(item, levels = SUA_item0)) -> df0_1
     df %>% filter(item_code %in% SUA_itemcode0) %>%
       filter(item == "Maize (corn)") %>%
       mutate(item = factor(item, levels = SUA_item0))-> df_1

     df_1 %>%
       ggplot() +
       facet_wrap(~item) +
       geom_bar(aes(x = year, y = value, fill = element),
                stat = "identity", size = 0.6, color = "black") +
       geom_hline(yintercept = 0, color = "grey20", linetype = 1, size = 1) +
       geom_point(data = df0_1 %>% filter(element == "Residuals"),
                  aes(x = year, y = value, color = element, shape = element),
                  size = 4, fill = "blue", color = "black") +
       scale_fill_manual(values = Bar_Col) +
       scale_shape_manual(values = 21) +
       scale_x_continuous(breaks = seq(2010, 2022, 2)) +
       scale_y_continuous(n.breaks = 8) +
       labs(x= "Year", y = "Mt", fill = "SUA element", shape = "",
            title = "(B) SUA: Primary commodity") +
       theme_bw() + theme0 +
       #theme(panel.border = element_rect(colour = "black", size=2))+
       guides(fill = guide_legend(order = 1),
              shape = guide_legend(order = 2)) -> p_MAIZE_Primary;p_MAIZE_Primary

     # * Maize SUA ----

     # Arrange the top row (side-by-side)
     top_row <- plot_grid(p_MAIZE_PCe +
                            theme(legend.position = "none",
                                  plot.title = element_text(size = 22, margin = margin(b = 15))),
                          p_MAIZE_Primary +
                            theme(legend.position = "none",
                                  plot.title = element_text(size = 22, margin = margin(b = 15))),
                          rel_widths = c(1.45, 1),
                          ncol = 2)

     reposition_legend(p_MAIZE_SUA +
                         theme(plot.title = element_text(size = 22, margin = margin(b = 15)),
                               legend.spacing.y = unit(0.5, 'lines'),
                               legend.box.margin=margin(30,10,0,10),
                               #legend.box.background = element_rect(color = "black", size = 0.6 )
                         ) +
                         facet_wrap(~item, scales = "free_y", ncol = 3),
                       'center',
                       panel = 'panel-3-3') -> p_MAIZE_SUA1


     # Add the bottom plot
     combined_plot <- plot_grid(top_row,
                                p_MAIZE_SUA1,
                                ncol = 1, rel_heights = c(1, 1.8))


     combined_plot %>% Write_png("0_Fig1S_tFBS_SUA_Maize", w = 18, h = 22)




     # SUA: Wheat and products ----
     Nested_Mapping_SUA_To_Traceable_FBS %>%
       filter(aggregated_PCe_item == "Wheat and products") %>%
       select(source_item_code, sink_item_code) %>% unlist() %>% unique() ->
       SUA_itemcode0

     Nested_Mapping_SUA_To_Traceable_FBS %>%
       filter(aggregated_PCe_item == "Wheat and products") %>%
       select(source_item, sink_item) %>% unlist() %>% unique() ->
       SUA_item0

     df0 %>% filter(item_code %in% SUA_itemcode0)%>%
       filter(item != "Wheat") %>%
       #mutate(item = factor(item, levels = SUA_item0)) %>%
       mutate(item = if_else(grepl("Communion wafers", item), "Communion wafers ...", item),
              item = if_else(grepl("Food preparations", item), "Food preparations ...", item),
              item = if_else(grepl("Mixes and doughs", item), "Mixes and doughs ...", item),
              item = if_else(grepl("Uncooked pasta", item), "Uncooked pasta ...", item)) -> df0_1
     df %>% filter(item_code %in% SUA_itemcode0) %>%
       filter(item != "Wheat") %>%
       #mutate(item = factor(item, levels = SUA_item0)) %>%
       mutate(item = if_else(grepl("Communion wafers", item), "Communion wafers ...", item),
              item = if_else(grepl("Food preparations", item), "Food preparations ...", item),
              item = if_else(grepl("Mixes and doughs", item), "Mixes and doughs ...", item),
              item = if_else(grepl("Uncooked pasta", item), "Uncooked pasta ...", item))-> df_1


     df_1 %>%
       ggplot() +
       facet_wrap(~item, scales = "free_y", ncol = 4) +
       geom_bar(aes(x = year, y = value, fill = element),
                stat = "identity", size = 0.3, color = "black") +
       geom_hline(yintercept = 0, color = "grey20", linetype = 1, size = 1) +
       geom_point(data = df0_1 %>% filter(element == "Residuals"),
                  aes(x = year, y = value, color = element, shape = element),
                  size = 3, fill = "blue", color = "black") +
       scale_fill_manual(values = Bar_Col) +
       scale_shape_manual(values = 21) +
       scale_x_continuous(breaks = seq(2010, 2022, 4)) +
       scale_y_continuous(n.breaks = 8) +
       labs(x= "Year", y = "Mt", fill = "SUA element", shape = "",
            title = "(C) SUA: Processed products") +
       theme_bw() + theme0 + theme(panel.spacing = unit(0.8, "lines")) +
       guides(fill = guide_legend(order = 1),
              shape = guide_legend(order = 2, title = NULL)) -> p_WHEAT_SUA


     df0 %>% filter(item_code %in% SUA_itemcode0)%>%
       filter(item == "Wheat") %>%
       mutate(item = factor(item, levels = SUA_item0)) -> df0_1
     df %>% filter(item_code %in% SUA_itemcode0) %>%
       filter(item == "Wheat") %>%
       mutate(item = factor(item, levels = SUA_item0))-> df_1

     df_1 %>%
       ggplot() +
       facet_wrap(~item) +
       geom_bar(aes(x = year, y = value, fill = element),
                stat = "identity", size = 0.6, color = "black") +
       geom_hline(yintercept = 0, color = "grey20", linetype = 1, size = 1) +
       geom_point(data = df0_1 %>% filter(element == "Residuals"),
                  aes(x = year, y = value, color = element, shape = element),
                  size = 4, fill = "blue", color = "black") +
       scale_fill_manual(values = Bar_Col) +
       scale_shape_manual(values = 21) +
       scale_x_continuous(breaks = seq(2010, 2022, 2)) +
       scale_y_continuous(n.breaks = 8) +
       labs(x= "Year", y = "Mt", fill = "SUA element", shape = "",
            title = "(B) SUA: Primary commodity") +
       theme_bw() + theme0 +
       #theme(panel.border = element_rect(colour = "black", size=2))+
       guides(fill = guide_legend(order = 1),
              shape = guide_legend(order = 2)) -> p_WHEAT_Primary;p_WHEAT_Primary

     # * Wheat SUA ----

     # Arrange the top row (side-by-side)
     top_row <- plot_grid(p_WHEAT_PCe +
                            theme(legend.position = "none",
                                  plot.title = element_text(size = 22, margin = margin(b = 15))),
                          p_WHEAT_Primary +
                            theme(legend.position = "none",
                                  plot.title = element_text(size = 22, margin = margin(b = 15))),
                          rel_widths = c(1.45, 1),
                          ncol = 2)

     reposition_legend(p_WHEAT_SUA +
                         theme(plot.title = element_text(size = 22, margin = margin(b = 15)),
                               legend.spacing.y = unit(0.5, 'lines'),
                               legend.box.margin=margin(30,10,0,10),
                               #legend.box.background = element_rect(color = "black", size = 0.6 )
                         ) +
                         facet_wrap(~item, scales = "free_y", ncol = 4),
                       'center',
                       panel = 'panel-4-4') -> p_WHEAT_SUA1


     # Add the bottom plot
     combined_plot <- plot_grid(top_row,
                                p_WHEAT_SUA1,
                                ncol = 1, rel_heights = c(1, 2.2))


     combined_plot %>% Write_png("0_Fig1S_tFBS_SUA_Wheat", w = 19, h = 23)




