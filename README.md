

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

```{r eval = FALSE}
devtools::load_all()
```

## Run the driver
There are two ways to run the driver:
1. 
```{r eval = FALSE}
driver_drake()
```
`driver_drake()` runs the driver and stores the outputs in a hidden cache. When you run `driver_drake()` again it will skip steps that are up-to-date. This is useful if you will be adjusting the data inputs and code and running the data system multiple times. For this reason, we almost always recommend using `driver_drake()`. More details can be found in the [vignette](https://jgcri.github.io/gcamdata/articles/driverdrake_vignette.html).

2. 
```{r eval = FALSE}
driver()
```
See [the documentation](https://jgcri.github.io/gcamdata/reference/driver.html) for more options when running the driver, such as what outputs to generate or when to stop.

## Output files

Users can specify the output directory (`DIR_OUTPUT_CSV`) that stores the output csv files in `constants.R`. The default directory is `outputs/CSV`. The the file will be exported when `OUTPUT_Export_CSV == TRUE` (an option in `constants.R`).  
Users can also make use of the functions to trace the processing by step, when`driver_drake()` is employed.





Copyright 2019 Battelle Memorial Institute; see the LICENSE file.
