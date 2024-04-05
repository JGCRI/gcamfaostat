---
title: 'gcamfaostat: An R package to prepare, process, and synthesize FAOSTAT data for global agroeconomic and multisector dynamic modeling'
authors:
- affiliation: 1
  name: Xin Zhao
  orcid: 0000-0002-1801-4393
- affiliation: 2
  name: Maksym Chepeliev
  orcid: 0000-0001-8585-2314
- affiliation: 1
  name: Pralit Patel
  orcid: 0000-0003-3992-1061
- affiliation: 1
  name: Marshall A. Wise
  orcid: 0000-0002-2718-0051
- affiliation: 1
  name: Katherine V. Calvin 
  orcid: 0000-0003-2191-4189
- affiliation: 1
  name: Kanishka Narayan
  orcid: 0000-0001-8483-6216
- affiliation: 1
  name: Christopher R. Vernon
  orcid: 0000-0002-3406-6214
date: "04 April 2024"
output: pdf_document
bibliography: vignettes/references.bib
tags:
- R
- GCAM
- faostat
- global economic modeling
affiliations:
- index: 1
  name: Joint Global Change Research Institute, Pacific Northwest National Laboratory, College Park, MD, USA
- index: 2
  name: Center for Global Trade Analysis, Department of Agricultural Economics, Purdue University, West Lafayette, IN, USA
---

# Summary

The **`gcamfaostat`** R package is designed for the preparation, processing, and synthesis of the Food and Agriculture Organization (FAO) Statistics ([FAOSTAT](https://www.fao.org/faostat/en/#data)) agroeconomic data. The primary purpose is to facilitate FAOSTAT data use in global economic and multisector dynamic models while ensuring transparency, traceability, and reproducibility. Here, we provide an overview of the development of **`gcamfaostat v1.0.0`** and demonstrate its capabilities in generating and maintaining agroeconomic data required for the Global Change Analysis Model ([GCAM](https://jgcri.github.io/gcam-doc/toc.html)). Our initiative seeks to enhance the quality and accessibility of data for the global agroeconomic modeling community, with the aim of fostering more robust and harmonized outcomes in a collaborative, efficient, and open-source framework. The processed data and visualizations offered by **`gcamfaostat`** can also be valuable to a broader audience interested in gaining insights into the intricacies of global agriculture.

# Statement of need

Global economic and multisector dynamic models have become pivotal tools for investigating complex interactions between human activities and the environment, as evident in recent research [@Doelman2022Quantifying;@Fujimori2022Land-based;@Ven2023multimodel]. Agriculture and land use (AgLU) plays a critical role in these models, particularly when used to address key agroeconomic questions [@Graham2023Agricultural;@Yarlagadda2023Trade;@Zhang2023Agriculture;@zhao2024nc]. Sound economic modeling hinges significantly upon the accessibility and quality of data [@Bruckner2019FABIO;@Calvin2022GMD;@Chepeliev2022JGEA]. The FAOSTAT serves as one of the key global data sources, offering open-access data on country-level agricultural production, land use, trade, food consumption, nutrient content, prices, and more [@FAOSTAT2023FAOSTAT]. However, the raw data from FAOSTAT requires cleaning, balancing, and synthesis, involving assumptions such as interpolation and mapping, which can introduce uncertainties. In addition, some of the core datasets reported by FAOSTAT, such as FAO’s Food Balance Sheets (FBS), are compiled at a specific level of aggregation, combining together primary and processed commodities (e.g., wheat and flour), which creates additional data processing challenges for the agroeconomic modeling community [@Chepeliev2022JGEA]. It is noteworthy that each agroeconomic modeling team typically develops its own assumptions and methods to prepare and process FAOSTAT data. While largely overlooked, the uncertainty in the base data calibration approach likely contribute to the disparities in model outcomes [@Lampe2014AgMIP;@zhao2021role]. Hence, our motivation is to create an open-source tool (**`gcamfaostat`**) for the preparation, processing, and synthesis of FAOSTAT data for global agroeconomic modeling. To the best of our knowledge, such a tool has not been developed yet. `gcamfaostat` bridges a crucial gap in the literature by offering several key features and capabilities.

1.	**Transparency and Reproducibility**: **`gcamfaostat`** incorporates functions for downloading, cleaning, synthesizing, and balancing agroeconomic datasets in a traceable, transparent, and reproducible manner [@wilkinson_fair_2016]. This enhances the credibility of the processing and allows for better scrutiny of the methods. Here we document and demonstrate the use of the package in generating and updating agroeconomic data needed for GCAM v7.0 [@bond_lamberty_2023].  
2.	**Expandability and Consistency**: **`gcamfaostat`** can be used to flexibly process and update agroeconomic data for any agroeconomic model. The package framework can be also easily expanded to include new modules for consistently processing new data.          
3.	**Community Collaboration and Efficiency**: The package provides an open-source platform for researchers to continually enhance the processing methods. This collaborative approach, which establishes a standardized and streamlined process for data preparation and processing, carries benefits that extend to all modeling groups. By reducing the effort required for data processing and fostering harmonized base data calibration, it contributes to a reduction in modeling uncertainty and enhances the overall research efficiency.    
4.	**User Accessibility**: Where applicable, the processed data can be mapped and aggregated to user-specified regions and sectors for agroeconomic modeling. However, beyond the modeling community, **`gcamfaostat`** can be valuable to a broader range of users interested in understanding global agriculture trends and dynamics, as it provides user-friendly data processing and visualization tools. 

# Design and Functionality

## Bridging the gap between FAOSTAT and global economic modeling

GCAM is a widely recognized global economic and multisector dynamic model complemented by the `gcamdata` R package, which serves as its data processing system [@bond2019gcamdata]. Particularly, `gcamdata` includes modules (data processing chunks) and functions to convert raw data inputs into hundreds of XML input files used by GCAM. As an illustration, in the latest GCAM version, GCAM v7.0, about 280 XML files, with a combined size of 4.1 GB, are generated. Although AgLU-related XMLs represent only about 10% of the total number of files, they contribute over 50% in size (~2.1 GB). The majority of AgLU-related data, whether directly or indirectly, rely on raw data sourced from FAOSTAT. 

Nonetheless, the FAOSTAT data employed within `gcamdata` has traditionally involved manual downloads and may have undergone preprocessing. In light of the increasing data needs, maintaining the FAOSTAT data processing tasks in `gcamdata` has become increasingly challenging. In addition, the processing of FAOSTAT data in the AgLU modules of `gcamdata` is tailored specifically for GCAM. Consequently, the integration of FAOSTAT data updates has proven to be a non-trivial task, and the data processed by the AgLU module has limited applicability in other modeling contexts [@zhao_cmp360]. The **`gcamfaostat`** package aims to address these limitations (\autoref{fig:Fig1}). The targeted approach incorporates data preparation, processing, and synthesis capabilities within a dedicated package, **`gcamfaostat`**, while regional and sectoral aggregation functions in the model data system are implemented using standalone routines within the `gcamdata` package. This strategy not only ensures the streamlined operation of **`gcamfaostat`** but also contributes to keeping data systems lightweight and more straightforward to maintain.  

![New framework of utilizing FAOSTAT data in GCAM and similar large-scale models through gcamfaostat. Modules with identifier "_xfaostat_" only exist in gcamfaostat. The AgLU-related modules ("_aglu_") that rely on outputs from gcamfaostat can run in both packages. Other gcamdata modules that process data in such areas as energy, emissions, water, and socioeconomics only exist in gcamdata. \label{fig:Fig1}](./man/figures/Fig_gcamfaostat_and_gcamdata.jpg){width=70%} 



## Key functions 

Here we describe key functions included in **`gcamfaostat (v1.0.0)`** focusing on the data [preparing](https://jgcri.github.io/gcamfaostat/articles/vignette_preparing_data.html) and [processing](https://jgcri.github.io/gcamfaostat/articles/vignette_processing_flow.html). More details about functions and examples for
[data tracing](https://jgcri.github.io/gcamfaostat/articles/vignette_preparing_data.html#generate-the-metadata-for-the-gcamfaostat-input-data), [visualization](https://jgcri.github.io/gcamfaostat/articles/vignette_visualization.html) and [other cabilities](https://jgcri.github.io/gcamfaostat/articles/vignette_use_cases.html) are illustrated in the online [**User Guide**](https://jgcri.github.io/gcamfaostat/index.html). 

The architecture of **`gcamfaostat`** processing modules is depicted in \autoref{fig:Fig2}. This framework currently comprises eight preprocessing modules and nine processing and synthesizing modules, generating twelve output files tailored for
[GCAM v7.0](https://github.com/JGCRI/gcam-core/releases/tag/gcam-v7.0). Each module is essentially an `R` function with well-defined inputs and outputs. 


Note that by default, the preprocessed FAOSTAT data, i.e., outputs of the `xfaostat_L101_*` modules, have been stored in the [`Prebuilt Data`](https://github.com/JGCRI/gcamfaostat/blob/main/data/PREBUILT_DATA.rda) of the package. **`gcamfaostat`** includes a function to generate metadata ([`gcamfaostat_metadata`](https://jgcri.github.io/gcamfaostat/reference/gcamfaostat_metadata.html)). It accesses both the latest FAOSTAT metadata and local data information and returns a [summary table] (see [example online](https://jgcri.github.io/gcamfaostat/articles/vignette_preparing_data.html#generate-the-metadata-for-the-gcamfaostat-input-data)) including the dataset information needed for **`gcamfaostat`**. There are also functions to download FAOSTAT raw data from either a remote archive [(`FF_download_RemoteArchive`)](https://jgcri.github.io/gcamfaostat/reference/FF_download_RemoteArchive.html) or directly from FAOSTAT [(`FF_download_FAOSTAT`)](https://jgcri.github.io/gcamfaostat/reference/FF_download_FAOSTAT.html).  

To showcase the flexibility and expandability of our package, we also incorporated two AgLU modules (from `gcamdata`) that exemplify the data aggregation processes, e.g., across regions, sectors, and time. More importantly, the [`driver_drake()`](https://jgcri.github.io/gcamfaostat/reference/driver_drake.html) function plays a pivotal role by executing all available data processing modules, thereby generating both intermediate and final outputs, which are vital components of our comprehensive data processing pipeline. The function was inherited from `gcamdata` and it uses the drake [@Landau2018] pipeline framework, which simplifies module updates, data tracing, and results visualization process. 


![Data processing architecture in gcamfaostat. \label{fig:Fig2}](./man/figures/Fig_data_processing_flow.jpg){width=100%}

Finally, data development is never a once and for all task, and continued efforts are needed to sustain and improve the processing procedures. Future work and community contribution are also detailed in the online [User Guide](https://jgcri.github.io/gcamfaostat/index.html). 

  


# Acknowledgements

This research was supported by the US Department of Energy, Office of Science, as part of research in MultiSector Dynamics, Earth and Environmental System Modeling Program. The Pacific Northwest National Laboratory is operated for DOE by Battelle Memorial Institute under contract DE-AC05-76RL01830. Dr. Calvin is currently detailed to the National Aeronautics and Space Administration. Dr. Calvin’s contributions to this article occurred prior to her detail. The views expressed are her own and do not necessarily represent the views of the National Aeronautics and Space Administration or the United States Government. We extend our sincere appreciation to Matthew Binsted and Page Kyle for their invaluable contributions.  
 



# References
