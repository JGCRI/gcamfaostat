



# Capital Stock
FF_download_FAOSTAT(DATASETCODE = "CS")

# Modity gcamfaostat_metadata to include "CS" in FAO_dataset_code_required

gcamfaostat_metadata(OnlyReturnDatasetCodeRequired = T)

# Run
gcamfaostat_metadata()

# adding preprocessing to module_xfaostat_L101_RawDataPreProc2_PP_PD_OA

## FAOSTAT_FILE = "aglu/FAO/FAOSTAT/Investment_CapitalStock_E_All_Data_(Normalized)" to MODULE_INPUTS
## CS related processing


# adding export to module_xfaostat_L999_CSVExport

## "CS" to MODULE_INPUTS
## "GCAMFAOSTAT_CapitalStock" to MODULE_OUTPUTS

## add CS to prebuilt data in generate_package_data
