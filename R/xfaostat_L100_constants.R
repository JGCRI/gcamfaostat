

  # Directories ----

  #DIR_RAW_DATA_FAOSTAT <- system.file("extdata", "aglu/FAO/FAOSTAT", package = "gcamdata")

  DIR_RAW_DATA_FAOSTAT <- "inst/extdata/aglu/FAO/FAOSTAT"
  OUTPUT_Export_CSV = T
  # Output GCAM csv
  DIR_OUTPUT_CSV <- "inst/extdata/aglu/FAO/temp"
  dir.create(file.path(DIR_OUTPUT_CSV), showWarnings = FALSE)



  # Historical years of focus ----
  #*******************************************
  FAOSTAT_Hist_Year <- seq(1970, 2020)
  #Bilateral trade year starts from 1986 but higher quality after 1992
  FAOSTAT_Hist_Year_Bilateral <- seq(1992, 2020)
  FAOSTAT_Hist_Year_FBSH <- seq(1973, 2013)
  FAOSTAT_Hist_Year_FBS <- seq(2010, 2019) # New FBS years
  MIN_HIST_PP_YEAR = 2010 # first producer price year





  # Balance elements ----
  #*******************************************
  # used in Get_SUA_TEMPLATE and SUA_bal_adjust

  c("Opening stocks", "Production", "Import",
    "Export", "Processed", "Food", "Feed", "Seed", "Other uses", "Loss", "Closing stocks",
    "Residuals", "Regional supply", "Regional demand", "Stock Variation") ->
    Bal_element_new


  # Assumed parameters for data processing or interpolation ----
  #*******************************************
  # Forest trade data adjustment
  # Adjust Export when Demand = Production + Import - Export < 0
  # Adjust Export Production * Export_Production_ratio
  For_Export_Production_Ratio_Adj = 0.9

  # Boundary used for correct regional value with world of the conversion from mass to macro-nutrient
  # Used in FAOSTAT_S1D_Food_Kcal.R
  REGIONAL_NUTRIENT_MASS_CONV_OUTLIER_BOUNDARY <- 0.15
  Hist_MEAN_Year_NUTRIENT_MASS_CONV <- 2010:2019 # average cal per g



  # Other utils ----
  #*******************************************
  # decimal places in ggplot
  scaleFUN <- function(x) sprintf("%.0f", x)

  #*******************************************
