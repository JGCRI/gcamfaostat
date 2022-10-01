[![codecov](https://codecov.io/gh/JGCRI/gcamdata/branch/main/graph/badge.svg)](https://codecov.io/gh/JGCRI/gcamdata)
![R-CMD](https://github.com/JGCRI/gcamdata/workflows/R-CMD/badge.svg)
![coverage-test](https://github.com/JGCRI/gcamdata/workflows/coverage-test/badge.svg)



# gcamdata-faostat
Functions in this R package (**gcamdata-faostat**) download, clean, processe, connect, and visualize data from FAOSTAT for global economic and integrated assessment modeling. The package is built based on the existing gcamdata package structure for consistency, transparency, and traceability. 

The goals of this version are:
(1) Check FAOSTAT data updates and download necessary datasets
(2) Develop a new method of primary equivalent aggregation to aggregate supply-utilization-accounting (SUA) data for items along the supply chain (e.g., wheat flour, bran, and germ to wheat-primary-equivalent). The method preserves balance across space (trade balance), time (storage carryover), supply-utilization, and the combination of these dimensions with minimal adjustments. 
(3) Apply the new method of primary equivalent aggregation to aggregating FAO ~500 SUA (SCL) items to ~100 primary equivalent items in FAO Food Balance Sheet (FBS).
(4) Compare the balanced data compiled using different methods and visualize the difference.      
    




Copyright 2019 Battelle Memorial Institute; see the LICENSE file.
