
# `gcamfaostat`: An R package to prepare, process, and synthesize FAOSTAT data for global agroeconomic and multisector dynamic modeling

## Summary

The `gcamfaostat` R package is developed to prepare, process, and synthesize [FAOSTAT](https://www.fao.org/faostat/en/#data) agroeconomic dataset for global economic and multisector dynamic modeling, in a traceable, transparent, and reproducible manner. Here, we demonstrate the use of the `gcamfaostat` framework for generating and updating agroeconomic data needed for the Global Change Analysis Model ([GCAM](https://jgcri.github.io/gcam-doc/toc.html)). However, our initiative seeks to enhance the quality and accessibility of data for the global agroeconomic modeling community, with the aim of fostering more robust and harmonized outcomes in a collaborative, efficient, open-source manner. The processed data and visualizations in `gcamfaostat` can be valuable to a broader range of users interested in understanding global agriculture.  

This tool bridges a crucial gap in the literature by offering several key features and capabilities.  

1.	**Transparency and Reproducibility**: `gcamfaostat` incorporates functions for downloading, cleaning, synthesizing, and balancing agroeconomic datasets in a traceable, transparent, and reproducible manner. This enhances the credibility of the processing and allows for better scrutiny of the methods. We have documented and demonstrated the use of the package in generating and updating agroeconomic data needed for the GCAM.  
2.	**Expandability and Consistency**: gcamfaostat can be used to flexibly process and update agroeconomic data for any agroeconomic model. The package framework is also easy to be expanded to include new modules to consistently process new data.     
3.	**Community Collaboration and Efficiency**: The package provides an open-source platform for researchers to continually enhance the processing methods. This collaborative approach, which establishes a standardized and streamlined process for data preparation and processing, carries benefits that extend to all modeling groups. By reducing the efforts required for data processing and fostering harmonized base calibration data, it contributes to a reduction in modeling uncertainty and enhances the overall research efficiency.  
4.	**User Accessibility**: Where applicable, the processed data can be mapped and aggregated to user-specified regions and sectors for agroeconomic modeling. However, beyond the modeling community, `gcamfaostat` can be valuable to a broader range of users interested in understanding global agriculture trends and dynamics, as it provides accessible and processed data and visualization functions.

## User Guide
The package is documented in the [online manual](https://jgcri.github.io/gcamfaostat/index.html)

To contribute, see [contribution guidance](https://jgcri.github.io/gcamfaostat/CONTRIBUTE.html)
## Contributing

Please read our [Contributing Guidelines](CONTRIBUTING.md) for information on how to contribute to this project.








![Figure 2. Structure of gcamfaostat](man/figures/Fig_data_processing_flow.jpg)  
**Figure 2. Structure of gcamfaostat**
  
    



## Download and install:

```r
install.packages("devtools")
devtools::install_github("jgcri/gcamfaostat")
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

# Contributing

Please read the [Contribution Guidelines](CONTRIBUTING.md) for details on how to contribute to this project.

# Related publications  
- Bond-Lamberty, Ben, Kalyn Dorheim, Ryna Cui, Russell Horowitz, Abigail Snyder, Katherine Calvin, Leyang Feng et al. "gcamdata: An R package for preparation, synthesis, and tracking of input data for the GCAM integrated human-earth systems model." Journal of Open Research Software 7, no. 1 (2019). DOI: 10.5334/jors.232
- Calvin, Katherine V., Abigail Snyder, Xin Zhao, and Marshall Wise. "Modeling land use and land cover change: using a hindcast to estimate economic parameters in gcamland v2. 0." Geoscientific Model Development 15, no. 2 (2022): 429-447. https://doi.org/10.5194/gmd-15-429-2022
- Chepeliev, Maksym. "Incorporating nutritional accounts to the GTAP Data Base." Journal of Global Economic Analysis 7, no. 1 (2022): 1-43. https://doi.org/10.21642/JGEA.070101AF 
- Narayan et al., (2021). ambrosia: An R package for calculating and analyzing food demand that is responsive to changing incomes and prices. Journal of Open Source Software, 6(59), 2890. https://doi.org/10.21105/joss.02890
- Zhao, Xin, Katherine V. Calvin, Marshall A. Wise, and Gokul Iyer. "The role of global agricultural market integration in multiregional economic modeling: Using hindcast experiments to validate an Armington model." Economic Analysis and Policy 72 (2021): 1-17. https://doi.org/10.1016/j.eap.2021.07.007
- Zhao, Xin, and Marshall Wise. "Core Model Proposal# 360: GCAM agriculture and land use (AgLU) data and method updates: connecting land hectares to food calories." PNNL https://jgcri.github.io/gcam-doc/cmp/CMP_360-AgLU_data_method_updates.pdf 




Copyright 2019 Battelle Memorial Institute; see the LICENSE file.
