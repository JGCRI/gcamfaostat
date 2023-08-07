# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

# General behavior constants ======================================================================

# having issues with package check here
# comment this line out when building package
  DIR_RAW_DATA_FAOSTAT <- system.file("extdata", "aglu/FAO/FAOSTAT", package = "gcamdata")





  # used in Get_SUA_TEMPLATE and SUA_bal_adjust

  c("Opening stocks", "Production", "Import",
    "Export", "Processed", "Food", "Feed", "Seed", "Other uses", "Loss", "Closing stocks",
    "Residuals", "Regional supply", "Regional demand", "Stock Variation") ->
    Bal_element_new
