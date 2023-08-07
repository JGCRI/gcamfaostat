# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

# General behavior constants ======================================================================

# having issues with package check here
# comment this line out when building package
  DIR_RAW_DATA_FAOSTAT <- system.file("extdata", "aglu/FAO/FAOSTAT", package = "gcamdata")





  # Balance elements; used in Get_SUA_TEMPLATE and SUA_bal_adjust

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

