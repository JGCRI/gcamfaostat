# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

# ***gcamfaostat constants  ----

# Key parameters
# If TRUE, process raw FAO data
# If FALSE, use prebuilt data to load FAO data
Process_Raw_FAO_Data <- TRUE

# OUTPUT_Export_CSV option moved to driver_drake as write_csv_model
# The default is NULL
# If "GCAM", CSV will be generated and saved to DIR_OUTPUT_CSV


DISABLED_MODULES <- c("yextension_L100_FoodBalanceSheet")

# gcamdata specific modules are disabled ----
DISABLED_MODULES <-
  c("aglu_L100.FAO_SUA_PrimaryEquivalent",
    "aglu_L100.FAO_SUA_connection",
    "aglu_L100.FAO_preprocessing_OtherData",
    "aglu_L110.For_FAO_R_Y")



# Directories ----

## Fao raw data folder
DIR_RAW_DATA_FAOSTAT <- "FAOSTAT"


# Historical years of focus ----
#*******************************************
FAOSTAT_Hist_Year <- seq(1973, 2022)
#Bilateral trade year starts from 1986 but higher quality after 1992
#FAOSTAT_Hist_Year_Bilateral <- seq(1992, 2020)
FAOSTAT_Hist_Year_TMBilateral <- seq(2010, 2022)
FAOSTAT_Hist_Year_TCL <- seq(1973, 2022)
FAOSTAT_Hist_Year_FBSH <- seq(1973, 2013)
FAOSTAT_Hist_Year_FBS <- seq(2010, 2022) # New FBS years
MIN_HIST_PP_YEAR = 2010 # first producer price year
Hist_MEAN_Year_NUTRIENT_MASS_CONV <- 2010:2022 # average cal per g



# Balance elements ----

# used in Get_SUA_TEMPLATE and SUA_bal_adjust

c("Opening stocks", "Production", "Import",
  "Export", "Processed", "Food", "Feed", "Seed", "Other uses", "Loss", "Closing stocks",
  "Residuals", "Regional supply", "Regional demand", "Stock Variation") ->
  Bal_element_new



# Data processing assumptions ----

# Forest trade data adjustment
# Adjust Export when Demand = Production + Import - Export < 0
# Adjust Export Production * Export_Production_ratio
For_Export_Production_Ratio_Adj = 0.9

# Boundary used for correct regional value with world of the conversion from mass to macro-nutrient
# Used in xfaostat_L106_FoodMacroNutrient.R.R
REGIONAL_NUTRIENT_MASS_CONV_OUTLIER_BOUNDARY <- 0.15


# Other utils ----

# decimal places in ggplot
scaleFUN <- function(x) sprintf("%.0f", x)

xml.XML_SUFFIX <- NULL




# **************************************----
# ***Default constants in gcamdata ----


# General behavior constants ======================================================================

OUTPUTS_DIR              <- "outputs/"
XML_DIR                  <- "xml/"
COMMENT_CHAR             <- "#"
UNDER_TIMESHIFT          <- FALSE
YEAR_PATTERN             <- "^(1|2)[0-9]{3}$"   # a 1 or 2 followed by three digits, and nothing else
LOGIT_TYPE_COLNAME       <- "logit.type"        # will be removed by test code before old-new comparison



# Flags ======================================================================

FLAG_INPUT_DATA      <- "FLAG_INPUT_DATA"       # input data, don't output
FLAG_NO_OUTPUT       <- "FLAG_NO_OUTPUT"        # don't output
FLAG_NO_TEST         <- "FLAG_NO_TEST"          # don't test
FLAG_XML             <- "FLAG_XML"              # xml data


# Time constants ======================================================================

# Historical years for level 1 data processing. All chunks that produce historical data
# for model calibration are required to produce annual data covering this entire span.
HISTORICAL_YEARS        <- 1971:2015
# Future years for level 1 data processing, for the few chunks that
# produce future data (e.g., population projections)
FUTURE_YEARS            <- 2016:2100
# Calibrated periods in the model. Only level 2 chunks should reference these
MODEL_BASE_YEARS        <- c(1975, 1990, 2005, 2010, 2015)
# Future (not calibrated) model periods. Only level 2 chunks should reference these
MODEL_FUTURE_YEARS      <- seq(2020, 2100, 5)
MODEL_YEARS             <- c(MODEL_BASE_YEARS, MODEL_FUTURE_YEARS)
MODEL_FINAL_BASE_YEAR   <- 2015



# Driver constants ======================================================================

driver.MAKE            <- "MAKE"
driver.DECLARE_OUTPUTS <- "DECLARE_OUTPUTS"
driver.DECLARE_INPUTS  <- "DECLARE_INPUTS"
driver.DECLARE_MODIFY  <- "DECLARE_MODIFY"

# Data and utility constants ======================================================================

data.SEPARATOR <- "; "
data.PRECURSOR <- "Precursor"
data.DEPENDENT <- "Dependent"
data.USER_MOD_POSTFIX <- "__0"


# Modeltime constants ======================================================================
# The number of years encompased in the first model period, currently hard coded in the C++
# Note, this is different than the number of years between period 0 and period 1
# The value typically does not matter but does come up for calculating resource depletion
modeltime.PERIOD0_TIMESTEP <- 15

# MAGICC model assumptions
modeltime.MAGICC_LAST_HISTORICAL_YEAR <- 2005
modeltime.MAGICC_BC_UNIT_FORCING      <- 0
modeltime.MAGICC_DEFAULT_EMISS_FILE   <- "../input/magicc/Historical Emissions/Default Emissions Module/Hist_to_2008_Annual.csv"
modeltime.MAGICC_C_START_YEAR         <- 1705

# Hector model assumptions
modeltime.HECTOR_END_YEAR        <- 2300
modeltime.HECTOR_EMISSIONS_YEAR  <- 2005
modeltime.HECTOR_INI_FILE        <- "../input/climate/hector-gcam.ini"



# AgLU constants ======================================================================

# Time
aglu.MODEL_MEAN_PERIOD_LENGTH <- 5       # AgLU data use a moving average over this period length in LA.100
aglu.MODEL_PRICE_YEARS      <- 2013:2017 # consistent with aglu.MODEL_SUA_MEAN_PERIODS
aglu.MODEL_MACRONUTRIENT_YEARS <- 2013:2017   # consistent with aglu.MODEL_SUA_MEAN_PERIODS; FAO only has data for after 2010
aglu.MODEL_COST_YEARS       <- 2008:2016
aglu.DEFLATOR_BASE_YEAR     <- 2015      # year used as the basis for computing regional price deflators
aglu.FALLOW_YEARS           <- 2013:2017 # Years used for calculating the % of fallow land
aglu.AGLU_HISTORICAL_YEARS  <- 1973:2015
aglu.BASE_YEAR_IFA          <- 2006      # Base year of International Fertilizer Industry Association (IFA) fertilizer application data
aglu.BIO_START_YEAR         <- 2025      # Also set in aglu/A_bio_ghost_share
aglu.CROSIT_HISTORICAL_YEAR <- 2005      # Historical year from the CROSIT data
aglu.FAO_LDS_YEARS          <- 1998:2002  # Years for which FAO harvested area data is averaged over for use in the land data system (LDS)
aglu.GTAP_HISTORICAL_YEAR   <- 2000      # Is the year that the GTAP data is based on.
aglu.LAND_HISTORY_YEARS     <- c(1700, 1750, 1800, 1850, 1900, 1950, 1975)
aglu.LAND_COVER_YEARS       <- sort(unique(c(aglu.LAND_HISTORY_YEARS, aglu.AGLU_HISTORICAL_YEARS)))
aglu.PREAGLU_YEARS          <- c(1700, 1750,1800, 1850, 1900, 1950)          # Cropland cover years prior to first aglu historical year to use in climate model component
aglu.SPEC_AG_PROD_YEARS     <- seq(max(aglu.AGLU_HISTORICAL_YEARS), 2050, by = 5) # Specified ag productivity years, KD i think this might need a better comment
aglu.SSP_DEMAND_YEARS       <- seq(2015, 2100, 5) # food demand in the SSPs is calculated at 5-yr intervals
# aglu.TRADED_* regional market commodities
aglu.TRADED_CROPS           <- c("Corn", "FiberCrop", "Fruits", "Legumes", "MiscCrop", "NutsSeeds", "OilCrop", "OtherGrain", "OilPalm", "Rice", "RootTuber", "Soybean", "SugarCrop", "Vegetables", "Wheat")
aglu.BIO_TRADE_SSP4_YEAR_FILLOUT       <- 2025 # year.fillout for SSP4 in L243.bio_trade_input
aglu.BIO_TRADE_SSP3_YEAR_FILLOUT       <- 2020 # year.fillout for SSP4 in L243.bio_trade_input
aglu.TRADED_MEATS           <- c("Beef", "Dairy", "Pork", "Poultry", "SheepGoat")
aglu.TRADED_FORESTS         <- c("Forest")
# Integrated world market and non-trade commodities
aglu.IWM_TRADED_COMM        <- c("FodderHerb", "OtherMeat_Fish") # Integrated World Market (IWM)commodities
aglu.NONTRADED_COMM         <- c("DDGS and feedcakes", "FodderGrass", "Pasture", "Residue", "Scavenging_Other") # non-traded commodities; "Pasture" is modeled as a crop produced from pasture land.


aglu.LAND_TOLERANCE    <- 0.005
aglu.MIN_PROFIT_MARGIN <- 0.15  # Unitless and is used to ensure that Agricultural Costs (units 1975USD/kg) don't lead to profits below a minimum profit margin.
aglu.MAX_FAO_LDS_SCALER <- 5   # Unitless max multiplier in reconciling LDS harvested area with FAO harvested area by country and crop. Useful for preventing bad allocations of N fert in AFG, TWN, several others
aglu.TREECROP_MATURE_AGE <- 10 # Number of years for vegetation carbon to reach peak, for tree crops

aglu.Min_Share_PastureFeed_in_PastureFodderGrass <- 0.1 # minimum share of pasture in Pasture_FodderGrass for feed uses to avoid negative or zero (not including Japan now); USA has ~30%
aglu.Zero_Min_PastureFeed_Share_iso <- c("jpn")         # mapped to GCAM_region_ID of Japan; Japan has zero unmanaged and protected pasture

# GLU (Geographic Land Unit) settings - see module_aglu_LA100.0_LDS_preprocessing
aglu.GLU <- "GLU"
aglu.GLU_NAME_DELIMITER <- ""  # delimiter between the GLU name and number

# Ratio of alfalfa price to grass hay used in the price conversion from alfalfa to grass hay.
# Alfalfa price source: USDA. 2011. Prices Received for Alfalfa Hay, Baled, Washington. National Agricultural Statistics Service, U.S. Department of Agriculture.
# Grass price source: Baker, A., and H. Lutman. 2008. Feed Year in Review (Domestic): Record Demand Drives U.S. Feed Grain Prices Higher in 2007/2008.
# FDS-2008-01, Economic Research Service, United States Department of Agriculture. Available at http://usda.mannlib.cornell.edu/usda/ers/FDS-yearbook/2000s/2008/FDS-yearbook-05-23-2008_Special_Report.pdf
aglu.PRICERATIO_GRASS_ALFALFA <- 0.7

# Pasture (forage) prices are equal to the hay (foddergrass) price times this exogenous multiplier. Equal to the hay price minus mowing, bundling, and transport.
aglu.PRICERATIO_PASTURE_HAY <- 0.5

# Carbon content of all cellulose
aglu.CCONTENT_CELLULOSE    <- 0.45

# Conversion from peak biomass to average biomass integrated over the course of the year
aglu.CCONV_PEAK_AVG <- 0.5

# Constraints for the minimum and maximum harvested:cropped ratios
# Source: Dalrymple, D.G. 1971, Survey of Multiple Cropping in Less Developed Nations, Foreign Econ. Dev. Serv., U.S. Dep. of Agricul., Washington, D.C.
# Cited in: Monfreda et al. 2008, Farming the Planet: 2., Global Biogeochemical Cycles 22, GB1022, http://dx.doi.org/10.1029/2007GB002947
aglu.MIN_HA_TO_CROPLAND <- 1  # minimum harvested:cropped ratios
aglu.MAX_HA_TO_CROPLAND <- 3  # maximum harvested:cropped ratios

# Minimum non-input costs of animal production technologies, in $/kg
aglu.MIN_AN_NONINPUT_COST <- 0.05

# Production constraints
aglu.MAX_MGDPAST_FRAC <- 0.95 # Maximum percentage of any region/GLUs pasture that is allowed to be in managed production.
aglu.MAX_MGDFOR_FRAC  <- 1    # Maximum percentage of any region/GLUs forest that is allowed to be in managed production.

# GDP constraints
aglu.HIGH_GROWTH_PCGDP <- 12.275   # GDP per capita high threshold for SSP4 region groupings, thousand 2010$ per person
aglu.LOW_GROWTH_PCGDP  <- 2.75     # GDP per capita low threshold for SSP4 region groupings, thousand 2010$ per person
aglu.PCGDP_YEAR <- 2010            # Year to compare to PCGDP thresholds

# AgLu mulitpliers
aglu.MGMT_YIELD_ADJ <- 0.2       # Yield multiplier that goes from the observed yield to the "high" and "low" yields: observed plus or minus observed times this number.
aglu.HI_PROD_GROWTH_MULT <- 1.5  # Multipliers for high ag prod growth scenarios
aglu.LOW_PROD_GROWTH_MULT <- 0.5 # Multipliers for low ag prod growth scenarios

# AgLU cost constants
aglu.BIO_GRASS_COST_75USD_GJ <- 0.75   # Production costs of biomass (from Patrick Luckow's work)
aglu.BIO_TREE_COST_75USD_GJ  <- 0.67   # Production costs of biomass (from Patrick Luckow's work)
aglu.FERT_PRICE              <- 596    # Price of fertilizer, 2010$ per ton NH3
aglu.FERT_PRICE_YEAR         <- 2010   # Year corresponding to the above price/cost
aglu.FOR_COST_SHARE          <- 0.59   # Non-land forestry cost share (from 2011 GTAP data base)

# Price at which base year bio frac produced is used.
# The share of residue biomass production in each region,
# defined as the energy produced divided by the total
# waste biomass produced, is read in by A_bio_frac_prod_R.csv.
# This price, in 1975$/GJ, indicates the biomass price at
# the given shares. It should be close to the model's actual
# (endogenous) biomass prices in the final calibration year.
aglu.PRICE_BIO_FRAC <- 1.2

# Fertilizer application rate for biomass, and carbon yields. Values from Adler et al. 2007 (doi:10.1890/05-2018)
aglu.BIO_GRASS_FERT_IO_GNM2 <- 5.6
aglu.BIO_GRASS_YIELD_KGCM2  <- 0.34
aglu.BIO_TREE_FERT_IO_GNM2  <- 3.36
aglu.BIO_TREE_YIELD_KGCM2   <- 0.345

# Water characteristics for biomass
# Reference: Chaturvedi et al. 2015, Climate mitigation policy implications for global irrigation water demand, Mitig Adapt Strateg Glob Change (2015) 20:389-407. DOI 10.1007/s11027-013-9497-4
aglu.BIO_GRASS_WATER_IO_KM3EJ <- 25
aglu.BIO_TREE_WATER_IO_KM3EJ  <- 25

# Maximum bioenergy (switchgrass) yield allowable, in tons per hectare from Wullschleger doi:10.2134/agronj2010.0087
aglu.MAX_BIO_YIELD_THA <- 20

# Energy content of biomass, GJ/ton
aglu.BIO_ENERGY_CONTENT_GJT <- 17.5


# XML-related constants
aglu.CROP_GLU_DELIMITER   <- "_"  # delimiter between the crop name and GLU name
aglu.GLU_NDIGITS          <- 3    # number of digits in the geographic land unit identifier codes
aglu.IRR_DELIMITER        <- "_"  # delimiter between the appended crop x GLU and irrigation level
aglu.LT_GLU_DELIMITER     <-      # delimiter between the land use type name and GLU name. should be the same as the crop-glu delimiter
aglu.MGMT_DELIMITER       <- "_"  # delimiter between appended tech name and management level


#*******************************************

# Detailed forest CMP constants

aglu.FOREST_commodities <- c("sawnwood","woodpulp")
aglu.FOREST_demand_sectors <- c("NonFoodDemand_sawnwood","NonFoodDemand_woodpulp")
aglu.FOREST_supply_sector <- "Forest"
#Below is a default amount of roundwood required to produce sawnwood.The model will calculate the IO using data. This will get used if and only if
# the IO calculated by the model is an NA. This is taken as an everage across countries from a UNECE report on forest products. Available here- https://unece.org/fileadmin/DAM/timber/publications/DP-49.pdf
aglu.FOREST_sawtimber_conversion <- 2.17

#90% of pulp processing is chemical which has an IO of 5.44 and 10% is mechanical which is 2.55. Taking weighted average of the two,
# we get 5.15. These are calculated as averages across countries.
#Source- https://unece.org/fileadmin/DAM/timber/publications/DP-49.pdf
aglu.FOREST_pulp_conversion <- 5.15

aglu.FOREST_max_price <- 165

#*******************************************



# Conversion constants ======================================================================
# The naming convention is CONV_(FROM-UNIT)_(TO-UNIT).

# Numeric (unitless)
CONV_BIL_MIL    <- 1000
CONV_MIL_BIL    <- 1 / CONV_BIL_MIL
CONV_BIL_THOUS  <- 1e6
CONV_THOUS_BIL  <- 1 / CONV_BIL_THOUS
CONV_MIL_THOUS  <- 1000
CONV_ONES_THOUS <- 0.001

# Mass
CONV_TON_MEGATON <- 1e-6
CONV_T_KG <- 1e3
CONV_KG_T <- 1 / CONV_T_KG
CONV_T_METRIC_SHORT <- 1000 / 908  # Ratio between metric ton and short ton
CONV_HA_BM2 <- 1e-5
CONV_HA_M2 <- 10000
CONV_THA_KGM2 <- 0.1   # tons C/ha -> kg C/m2
CONV_G_TG <- 1e-12
CONV_GG_TG <- 0.001 # gigagrams to tegagrams
CONV_TST_TG <- 0.000907 # thousand short tons to Tg
CONV_KG_TO_TG <- 1e-9
CONV_KT_MT <- 0.001 # kt to Mt
CONV_T_MT <- 1e-6 # t to Mt
CONV_G_KG <- 1e-3 # kilograms to grams
CONV_NH3_N <- 14/17 # Nitrogen to Ammonia
CONV_KBBL_BBL <- 1000 # thousand barrels to barrels
CONV_BBL_TONNE_RFO <- 1 / 6.66 # barrels to tons residual fuel oil
CONV_TONNE_GJ_RFO <- 40.87 # tons to GJ residual fuel oil
CONV_BBL_TONNE_DISTILLATE <- 1 / 7.46 # barrels to tons distillate
CONV_BBL_TONNE_RFO  <- 1 / 6.66       # barrels to tons residual fuel oil
CONV_G_KG           <- 1e-3           # kilograms to grams
CONV_GG_TG          <- 0.001          # gigagrams to teragrams
CONV_HA_BM2         <- 1e-5
CONV_HA_M2          <- 10000
CONV_KBBL_BBL       <- 1000           # thousand barrels to barrels
CONV_KG_TO_TG       <- 1e-9
CONV_KT_MT          <- 0.001          # kt to Mt
CONV_NH3_N          <- 14/17          # Nitrogen to Ammonia
CONV_T_KG           <- 1e3
CONV_KG_T           <- 1 / CONV_T_KG
CONV_T_METRIC_SHORT <- 1000 / 908     # Ratio between metric ton and short ton
CONV_T_MT           <- 1e-6           # t to Mt
CONV_THA_KGM2       <- 0.1            # tons C/ha -> kg C/m2
CONV_TON_MEGATON    <- 1e-6
CONV_TONNE_GJ_DISTILLATE  <- 42.91    # tons to GJ distillate
CONV_TONNE_GJ_RFO   <- 40.87          # tons to GJ residual fuel oil

# Time
CONV_YEAR_HOURS <- 24 * 365.25
CONV_DAYS_YEAR <- 1 / 365.25
CONV_DAY_HOURS <- 24

# Energy
CONV_MWH_GJ <- 3.6 # Megawatt hours to Gigajoules
CONV_MWH_EJ <- 3.6e-9 # Megawatt hours to Exajoules
CONV_GWH_EJ <- 3.6e-6
CONV_TWH_EJ <- 3.6e-3
CONV_KWH_GJ <- 3.6e-3
CONV_GJ_EJ <- 1e-9
CONV_MJ_EJ <- 1e-12
CONV_EJ_GJ <- 1 / CONV_GJ_EJ
CONV_MBLD_EJYR <- 6.119 * 365.25 * 1e-3 # million barrels a day to EJ per year
CONV_KBTU_EJ <- 1.0551e-12 # KiloBTU to EJ
CONV_TBTU_EJ <- 0.0010551 # TeraBTU to EJ
CONV_MJ_BTU <- 947.777
CONV_BTU_KJ <- 1.0551
CONV_MMBTU_KGH2 <- 0.113939965425114 # MMBTU/kg H2 - LHV Source: H2 CCTP Workbook.xls (Used for older GCAM assumptions)
CONV_GJ_KGH2 <- 0.12021 #GJ/kg H2 - LHV

# Distance
CONV_MILE_KM <- 1.60934 # Mile to km
CONV_NMILE_KM <- 1.852 # Nautical mile to km

# Other
CONV_MCAL_PCAL <- 1e-9
CONV_M3_BM3 <- 1e-09 # Cubic meters (m3) to billion cubic meters (bm3)
CONV_MILLION_M3_KM3 <- 1e-03
CONV_M2_ACR <- 0.0002471058
CONV_HA_M2 <- 1e4 # ha to m2
CONV_BM2_M2 <- 1e9
CONV_MILFT2_M2 <- 92900 # Million square feet to square meters
CONV_FT2_M2 <- 0.0929 # Square feet to square meters
CONV_GAL_M3 <- 0.00378541 #gallons to m3
CONV_MI_KM <- 1.60934
CONV_PERS_MILPERS <- 1000000 #Person to million-passengers

# SO2 related conversion factors
RESID_BTU_PER_BBL <- 6.29 # Source EIA (Note HHV)
RESID_BBLS_PER_TONNE <- 6.66 # Source EIA (Note HHV)
RESID_ENERGY_DENSITY_BTU <- RESID_BTU_PER_BBL * RESID_BBLS_PER_TONNE * 0.95 # Btu/tonne net
RESID_ENERGY_DENSITY_JOULES <- RESID_ENERGY_DENSITY_BTU * 1055 # TJ/Tg
RESID_ENERGY_CONTENT <- RESID_ENERGY_DENSITY_JOULES/1E6 # Tg/EJ
SO2_SHIP_LIMIT_POLICY_MULTIPLIER <- 0.001 * 2



