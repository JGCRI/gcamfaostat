

# gcamfaostat
**gcamfaostat** is an R package to prepare, process, and synthesize FAOSTAT data for global agroeconomic and multisector dynamic modeling. The Food and Agriculture Organization Statistical Database ([FAOSTAT](https://www.fao.org/faostat/en/#data)) provdes open access data on country-level agricultural production, trade, food, nutrients, prices, land use, etc, servering as the most important data source for global agroeconomic and multisector dynamic models. **gcamfaostat** aims to shorten the distance between the FAOSTAT raw data to economic modeling.

# gcamfaostat and gcamdata
**gcamfaostat** is built based on an existing R package, **[gcamdata](https://jgcri.github.io/gcamdata/index.html)**, which has similar functions to**gcamfaostat** though **gcamdata** includes broader aspects of data inputs and is designed for the global multisector dynamic model **GCAM**. **gcamfaostat** utilizes the robust, reproducible and transparent data processing systems built in **[gcamdata](https://github.com/JGCRI/gcam-core)**. The two packages are consistent, while **gcamfaostat** focuses on agroeconomic data processing and can provide input data for **gcamdata** (and thus GCAM) and other models that rely on FAOSTAT data.



The goals of this version are:
(1) Check FAOSTAT data updates and download necessary datasets
(2) Develop a new method of primary equivalent aggregation to aggregate supply-utilization-accounting (SUA) data for items along the supply chain (e.g., wheat flour, bran, and germ to wheat-primary-equivalent). The method preserves balance across space (trade balance), time (storage carryover), supply-utilization, and the combination of these dimensions with minimal adjustments. 
(3) Apply the new method of primary equivalent aggregation to aggregating FAO ~500 SUA (SCL) items to ~100 primary equivalent items in FAO Food Balance Sheet (FBS).
(4) Compare the balanced data compiled using different methods and visualize the difference.      
    
# User Guide
The package is documented in the [online manual](https://realxinzhao.github.io/gcamfaostat/index.html)


# Download and install:

```r
install.packages("devtools")
devtools::install_github("realxinzhao/gcamfaostat")
```
# Loading and run the gcamdata package

Open the `gcamfaostat.Rproj` file in the `gcamfaostat` folder. RStudio should open the project.

To load the `gcamdata` package, enter:

`devtools::load_all()`

## Run the driver
There are two ways to run the driver:
1. `driver_drake()`  

`driver_drake()` runs the driver and stores the outputs in a hidden cache. When you run `driver_drake()` again it will skip steps that are up-to-date. This is useful if you will be adjusting the data inputs and code and running the data system multiple times. For this reason, we almost always recommend using `driver_drake()`. More details can be found in the [vignette](https://jgcri.github.io/gcamdata/articles/driverdrake_vignette.html).

2. `driver()`  

See [the documentation](https://jgcri.github.io/gcamdata/reference/driver.html) for more options when running the driver, such as what outputs to generate or when to stop.

## Output files  
Users can specify the output directory (`DIR_OUTPUT_CSV`) that stores the output csv files in `constants.R`. The default directory is `outputs/CSV`. The the file will be exported when `OUTPUT_Export_CSV == TRUE` (an option in `constants.R`).  
Users can also make use of the functions to trace the processing by step, when`driver_drake()` is employed.  


# Related publications  
- Bond-Lamberty, Ben, Kalyn Dorheim, Ryna Cui, Russell Horowitz, Abigail Snyder, Katherine Calvin, Leyang Feng et al. "gcamdata: An R package for preparation, synthesis, and tracking of input data for the GCAM integrated human-earth systems model." Journal of Open Research Software 7, no. 1 (2019). DOI: 10.5334/jors.232
- Calvin, Katherine V., Abigail Snyder, Xin Zhao, and Marshall Wise. "Modeling land use and land cover change: using a hindcast to estimate economic parameters in gcamland v2. 0." Geoscientific Model Development 15, no. 2 (2022): 429-447. https://doi.org/10.5194/gmd-15-429-2022
- Chepeliev, Maksym. "Incorporating nutritional accounts to the GTAP Data Base." Journal of Global Economic Analysis 7, no. 1 (2022): 1-43. https://doi.org/10.21642/JGEA.070101AF 
- Narayan et al., (2021). ambrosia: An R package for calculating and analyzing food demand that is responsive to changing incomes and prices. Journal of Open Source Software, 6(59), 2890. https://doi.org/10.21105/joss.02890
- Zhao, Xin, Katherine V. Calvin, Marshall A. Wise, and Gokul Iyer. "The role of global agricultural market integration in multiregional economic modeling: Using hindcast experiments to validate an Armington model." Economic Analysis and Policy 72 (2021): 1-17. https://doi.org/10.1016/j.eap.2021.07.007
- Zhao, Xin, and Marshall Wise. "Core Model Proposal# 360: GCAM agriculture and land use (AgLU) data and method updates: connecting land hectares to food calories." PNNL https://jgcri.github.io/gcam-doc/cmp/CMP_360-AgLU_data_method_updates.pdf 




Copyright 2019 Battelle Memorial Institute; see the LICENSE file.
