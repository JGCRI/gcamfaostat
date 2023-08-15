library(usethis)
library(devtools)

# We could potentially use drake to speed up the process of updating the package
# data which otherwise requires multiple runs of driver.  However, given drake
# is optional we default to not use it.
USE_DRIVER_DRAKE <- TRUE


# It is frequently the case that we need to refresh the LEVEL2_DATA_NAMES in order to have
# a successful driver() run which is required to update the following so we will re-load
# the package now so the updated LEVEL2_DATA_NAMES can take effect.
devtools::load_all()

if(USE_DRIVER_DRAKE) {
  # do an initial call to ensure all targets are up to date
  driver_drake()
}

#' GCAM_DATA_MAP
#'
#' There are two levels of information available from the GCAM data system:
#' chunk dependencies, which are available for "free", i.e. with a fast query to
#' each chunk on the part of \link{\code{chunk_inputs}} and \link{\code{chunk_outputs}},
#' and detailed information on data object-level dependencies. This function is
#' used to generate this latter data, i.e. a tibble of chunk-output-precursor information,
#' which is used by \link{\code{dstrace}} and various other graphing and diagnostic utilities.
#' @author BBL
# Note: the above text is not used for package documentation and is instead
# replicated in data.R for that purpose.
if(USE_DRIVER_DRAKE) {
  # we will need to drake "plan" to construct the GCAM_DATA_MAP from cache
  # note: calling driver_drake with return_plan_only = TRUE does not actually run the driver
  gcamdata_plan <- driver_drake(return_plan_only = TRUE)
  GCAM_DATA_MAP <- create_datamap_from_cache(gcamdata_plan)
} else {
  GCAM_DATA_MAP <- driver(return_data_map_only = TRUE)
}
# Save these objects as external data (i.e. requires explicit call to `data()` to load)
usethis::use_data(GCAM_DATA_MAP, overwrite = TRUE, internal = FALSE)

prebuilt_data_names <- c(

  # outputs of module_xfaostat_L101_RawDataPreProc1_QCL
  c("QCL_wide",           # Ag production quantity and harvested area
    "QCL_area_code_map"), # Country code

  # outputs of module_xfaostat_L101_RawDataPreProc2_PP_PD_OA
  c("PP_wide",            # Producer prices
    "PD",                 # GDP deflator
    "OA"),                # Population

  # outputs of module_xfaostat_L101_RawDataPreProc3_SCL_FBS
  c("SCL_wide",           # Supply utilization accounting
    "FBS_wide"),          # New food balance sheet

  # outputs of module_xfaostat_L101_RawDataPreProc4_FBSH_CB
  c("FBSH_CB_wide"),      # Combined FBSH and CB

  # outputs of module_xfaostat_L101_RawDataPreProc5_TCL
  c("TCL_wide"),          # Gross trade

  # outputs of  module_xfaostat_L101_RawDataPreProc6_TM
  c("TM_bilateral_wide"), # Bilateral trade

  # outputs of  module_xfaostat_L101_RawDataPreProc7_FO
  c("FO_Roundwood"),      # Forestry data

  # outputs of module_xfaostat_L101_RawDataPreProc8_RL_RFN
  c("RL",                 # Land
    "RFN")                # Fertilizer

)

#' PREBUILT_DATA
#'
#' A list of prebuilt data objects.
#' Its immediate downstream dependencies (currently, four chunks) then use the
#' prebuilt versions of their outputs stored in this object.
#' @author BBL
# Note: the above text is not used for package documentation and is instead
# replicated in data.R for that purpose.
if(USE_DRIVER_DRAKE) {
  PREBUILT_DATA <- load_from_cache(prebuilt_data_names)
} else {
  PREBUILT_DATA <- driver(write_outputs = FALSE,
                          write_xml = FALSE,
                          return_data_names = prebuilt_data_names)
}
# Save these objects as external data (i.e. requires explicit call to `data()` to load)
usethis::use_data(PREBUILT_DATA, overwrite = TRUE, internal = FALSE)

