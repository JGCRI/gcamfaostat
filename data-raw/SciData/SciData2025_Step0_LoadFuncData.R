
# load gcamfaostat v1.1 and build data system ----
devtools::load_all()
driver_drake(write_csv_model = "Traceable_FBS")

## load data from cache ----

# output of module_yfaostat_Traceable_FBS_DataExport
Traceable_FBS_Data <-
  # output data
  c(CSV = "Nested_Mapping_SUA_To_Traceable_FBS",
    CSV = "SUA_2010_2022",
    CSV = "SUA_Food_Calorie_Macronutrient_2010_2022",
    CSV = "Traceable_FBS_PCe_2010_2022",
    CSV = "Traceable_FBS_Food_Calorie_Macronutrient_2010_2022",
    # more input data
    "OA",
    "L107.Traceable_FBS_Food_Calorie_Macronutrient_2010Plus",
    FILE = file.path(DIR_RAW_DATA_FAOSTAT, "Mapping_gcamdata_FAO_iso_reg") )


all_data <- Traceable_FBS_Data %>% load_from_cache()
get_data_list(all_data, Traceable_FBS_Data, strip_attributes = TRUE)


# Load package and ggplot themes ----
library(ggplot2)
require(patchwork)
require(RColorBrewer)
require(lemon)
require(cowplot)

## visualization for SciData paper ----
fontfamily = "sans"
theme0 <- theme(
  panel.grid.minor = element_line(size = 0.2, linetype = 2, colour = "grey75"),
  panel.grid.major = element_line(size = 0.2, linetype = 2, colour = "grey75"),
  panel.border = element_rect(colour = "black", size=1),
  text = element_text(family= fontfamily, size = 15),
  axis.text.y = element_text(angle = 0, color = "black", size = 15, margin = margin(r = 10)),
  axis.text.x = element_text(angle = 0, color = "black", size = 15, margin = margin(t = 10), vjust= 0.5),
  axis.title.y = element_text(size = 15, margin = margin(t = 0, r = 10, b = 0, l = 0)),
  axis.title.x = element_text(size = 15, margin = margin(t = 10, r = 0, b = 0, l = 0)),
  axis.text.x.top =  element_blank(),  axis.title.x.top = element_blank(),
  strip.background = element_rect(fill="grey95"),
  strip.text = element_text(size = 16),
  plot.title = element_text(hjust = 0, face = "bold", margin = margin(t = 10, r = 0, b = 10, l = 0)),
  plot.margin = margin(t = 10, r = 15, b = 10, l = 10) #panel.spacing = unit(1, "lines"),
)
theme_leg <- theme(legend.justification = "center",
                   legend.key.size = unit(1.5, "cm"),
                   legend.key.height=unit(1.5,"line"),
                   legend.spacing.x = unit(1, 'cm'), #legend.spacing.y = unit(5, 'cm'),
                   legend.text = element_text(margin = margin(l = -25,t=0, b=0), size = 15),
                   legend.box.margin=margin(-10, 10,-8,10),
                   legend.background = element_blank())

## fill element colors ----
RegDemendEle <- c("Closing stocks", "Export", "Other uses", "Processed", "Loss", "Seed", "Feed", "Food")
RegSupplyEle <- c("Production", "Import", "Opening stocks")
Ele <- c(RegDemendEle, RegSupplyEle)

#colorRampPalette( brewer.pal(11,"RdYlBu")[c(1:4,8:11)] )(length(Ele)) -> Bar_Col
colorRampPalette( brewer.pal(11,"Spectral")[c(1:5,10:11)] )(length(Ele)) -> Bar_Col

c(Bar_Col[1:length(RegDemendEle)], (Bar_Col[(length(RegDemendEle)+1) :length(Ele)] )) -> Bar_Col
names(Bar_Col) <- c(RegDemendEle, RegSupplyEle)

## help func ----
Write_png <- function(.plot, .name, .DIR_MODULE = "gcamfaostat_SciData",
                      w = 10, h = 10, r = 300){

  DIR_OUTPUT <- "output"
  # create a fig data output folder
  dir.create(file.path(DIR_OUTPUT, .DIR_MODULE), showWarnings = F)

  ggsave(file.path(DIR_OUTPUT, .DIR_MODULE, paste0(.name,".png")),
         plot = .plot,
         width = w, height = h, dpi = r)

  ggsave(file.path(DIR_OUTPUT, .DIR_MODULE, paste0(.name,".svg")),
         plot = .plot,
         width = w, height = h, dpi = r)
}


readr::read_csv("data-raw/SciData/FBS_treemap.csv", comment = "#") %>%
  select(LivestockCrop, Food,APE_comm_Agg  = aggregated_PCe_item) %>%
  distinct()->
  FBS_treemap


# Source visualization code by steps----
## Step 1 ----
source("data-raw/SciData/SciData2025_Step1_SUABalance.R")
## Step 2 ----
source("data-raw/SciData/SciData2025_Step2_BalanceCheck.R")
## Step 3 ----
source("data-raw/SciData/SciData2025_Step3_CalorieMacroutrient.R")
## Step 4 ----
source("data-raw/SciData/SciData2025_Step4_CompareFAO_CalorieMacronutrient.R")



# Save ISO region mapping ----

L107.Traceable_FBS_Food_Calorie_Macronutrient_2010Plus %>%
  distinct(area_code) %>%
  left_join_error_no_match(Mapping_gcamdata_FAO_iso_reg, by = "area_code") %>%
  transmute(ISO = iso, `FAO Area Code` = area_code, Name = FAO_country) %>%
  readr::write_csv("output/gcamfaostat_SciData/Table_ISO_AreaCode.csv")


