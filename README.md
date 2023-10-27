<!-- badges: start -->
[![pages-build-deployment](https://github.com/JGCRI/gcamfaostat/actions/workflows/pages/pages-build-deployment/badge.svg)](https://github.com/JGCRI/gcamfaostat/actions/workflows/pages/pages-build-deployment)
[![docs](https://github.com/JGCRI/gcamfaostat/actions/workflows/docs.yaml/badge.svg)](https://github.com/JGCRI/gcamfaostat/actions/workflows/docs.yaml)
<!-- badges: end -->

### `gcamfaostat`: An R package to prepare, process, and synthesize FAOSTAT data for global agroeconomic and multisector dynamic modeling

### Summary

The `gcamfaostat` R package is developed to prepare, process, and synthesize [FAOSTAT](https://www.fao.org/faostat/en/#data) agroeconomic dataset for global economic and multisector dynamic modeling, in a traceable, transparent, and reproducible manner. Here, we demonstrate the use of the `gcamfaostat` framework for generating and updating agroeconomic data needed for the Global Change Analysis Model ([GCAM](https://jgcri.github.io/gcam-doc/toc.html)). However, our initiative seeks to enhance the quality and accessibility of data for the global agroeconomic modeling community, with the aim of fostering more robust and harmonized outcomes in a collaborative, efficient, open-source manner. The processed data and visualizations in `gcamfaostat` can be valuable to a broader range of users interested in understanding global agriculture.  

This tool bridges a crucial gap in the literature by offering several key features and capabilities.  

1.	**Transparency and Reproducibility**: `gcamfaostat` incorporates functions for downloading, cleaning, synthesizing, and balancing agroeconomic datasets in a traceable, transparent, and reproducible manner. This enhances the credibility of the processing and allows for better scrutiny of the methods. We have documented and demonstrated the use of the package in generating and updating agroeconomic data needed for the GCAM.  
2.	**Expandability and Consistency**: gcamfaostat can be used to flexibly process and update agroeconomic data for any agroeconomic model. The package framework is also easy to be expanded to include new modules to consistently process new data.     
3.	**Community Collaboration and Efficiency**: The package provides an open-source platform for researchers to continually enhance the processing methods. This collaborative approach, which establishes a standardized and streamlined process for data preparation and processing, carries benefits that extend to all modeling groups. By reducing the efforts required for data processing and fostering harmonized base calibration data, it contributes to a reduction in modeling uncertainty and enhances the overall research efficiency.  
4.	**User Accessibility**: Where applicable, the processed data can be mapped and aggregated to user-specified regions and sectors for agroeconomic modeling. However, beyond the modeling community, `gcamfaostat` can be valuable to a broader range of users interested in understanding global agriculture trends and dynamics, as it provides accessible and processed data and visualization functions.

***

### User Guide
The package is documented in the [online manual](https://jgcri.github.io/gcamfaostat/index.html).

* [Getting Started](https://jgcri.github.io/gcamfaostat/articles/vignette_getting_started.html)
  * [Preparing Data](https://jgcri.github.io/gcamfaostat/articles/vignette_preparing_data.html)
  * [Processing Flow](https://jgcri.github.io/gcamfaostat/articles/vignette_processing_flow.html)
  * [Update Sources](https://jgcri.github.io/gcamfaostat/articles/vignette_updating_sources.html)
  * [Other Use Cases](https://jgcri.github.io/gcamfaostat/articles/vignette_use_cases.html) 
* [Functions](https://jgcri.github.io/gcamfaostat/reference/index.html)
* [Visualization](https://jgcri.github.io/gcamfaostat/articles/vignette_visualization.html)
* [News](https://jgcri.github.io/gcamfaostat/articles/vignette_news.html)


***

### Quick Start in R (> 4.0) & Rstudio

#### 1. Download and install:

* Using `devtools` to download and install (The size < 1 GB):
* `devtools::install_github("jgcri/gcamfaostat")`

#### 2. Load and run the gcamdata package

* Open the `gcamfaostat.Rproj` file in the `gcamfaostat` folder using RStudio.
* Load the `gcamdata` package:
* `devtools::load_all()`

#### 3. Modify configurations
* To export csv output files, in `constants.R`, 
  * set `OUTPUT_Export_CSV` to `TRUE`
  * specify the directory path (`DIR_OUTPUT_CSV`) for output files.

#### 4. Run the driver
* `driver_drake()` 

#### 5. Use data and package functions
* Data saved in `DIR_OUTPUT_CSV` can be used in downstream models.
* Once `drive_drake` has been run, all the intermediate data are saved and can be explored (see examples in [Use Cases](https://jgcri.github.io/gcamfaostat/articles/vignette_use_cases.html) and [Visualization](https://jgcri.github.io/gcamfaostat/articles/vignette_visualization.html).

***

### Package structure


* `gcamfaostat` processes [input data](https://jgcri.github.io/gcamfaostat/articles/vignette_preparing_data.html#metadata) to output data in a format that is needed for downstream processing and modeling, e.g., [data used in gcamdata-aglu-FAO](https://github.com/JGCRI/gcam-core/tree/master/input/gcamdata/inst/extdata/aglu/FAO) (see the schematic below).
* Input data was stored in the [Prebuilt Data](https://github.com/JGCRI/gcamfaostat/blob/main/data/PREBUILT_DATA.rda) of the package. The raw data is archived on Zenodo (see Zhao (2022) and URL in the [`FF_download_RemoteArchive`](https://github.com/JGCRI/gcamfaostat/blob/main/R/xfaostat_helper_funcs.R#L144) function) to ensure the processing is 100% replicable. Users can also download the latest data using [`FF_download_FAOSTAT`](https://github.com/JGCRI/gcamfaostat/blob/main/R/xfaostat_helper_funcs.R#90). 
* All intermediate processing and data flows are transparent and traceable. See [Processing Flow](https://jgcri.github.io/gcamfaostat/articles/vignette_processing_flow.html) for data-tracing examples. 

![](man/figures/Fig_data_processing_flow.jpg){width=90%}  
Schmatic: module (data processing chunk) structure of gcamfaostat

    
### Contributing
Please read our [Contributing Guidelines](CONTRIBUTING.md) for information on how to contribute to this project.

### Related publications  
- Bond-Lamberty, Ben, Kalyn Dorheim, Ryna Cui, Russell Horowitz, Abigail Snyder, Katherine Calvin, Leyang Feng et al. "gcamdata: An R package for preparation, synthesis, and tracking of input data for the GCAM integrated human-earth systems model." Journal of Open Research Software 7, no. 1 (2019). DOI: 10.5334/jors.232
- Calvin, Katherine V., Abigail Snyder, Xin Zhao, and Marshall Wise. "Modeling land use and land cover change: using a hindcast to estimate economic parameters in gcamland v2. 0." Geoscientific Model Development 15, no. 2 (2022): 429-447. https://doi.org/10.5194/gmd-15-429-2022
- Chepeliev, Maksym. "Incorporating nutritional accounts to the GTAP Data Base." Journal of Global Economic Analysis 7, no. 1 (2022): 1-43. https://doi.org/10.21642/JGEA.070101AF 
- Narayan et al., (2021). ambrosia: An R package for calculating and analyzing food demand that is responsive to changing incomes and prices. Journal of Open Source Software, 6(59), 2890. https://doi.org/10.21105/joss.02890
- Zhao, Xin, Katherine V. Calvin, Marshall A. Wise, and Gokul Iyer. "The role of global agricultural market integration in multiregional economic modeling: Using hindcast experiments to validate an Armington model." Economic Analysis and Policy 72 (2021): 1-17. https://doi.org/10.1016/j.eap.2021.07.007
- Zhao, Xin, and Marshall Wise. "Core Model Proposal# 360: GCAM agriculture and land use (AgLU) data and method updates: connecting land hectares to food calories." PNNL https://jgcri.github.io/gcam-doc/cmp/CMP_360-AgLU_data_method_updates.pdf 
- Zhao, Xin (2022). FAOSTAT AgLU data Archive GCAMv7 (1.0) [Data set]. Zenodo. https://doi.org/10.5281/zenodo.8260225




Copyright 2019 Battelle Memorial Institute; see the LICENSE file.
