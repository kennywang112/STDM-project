## Integrating Spatial Topology and Exogenous Variables for Traffic Crash Forecasting: A Case Study of Greater London

Spatio-temporal modelling simultaneously accounts for spatial autocorrelation and temporal dependencies, capturing the non-linear propagation of risk across topological networks. This study evaluates a range of statistical and deep learning architectures to identify a robust framework for accident prediction. The findings demonstrate that incorporating spatial topology into temporal forecasting substantially improves model performance, providing data-driven insights for urban transport safety. The data is sourced from the following datasets:

-   [London shp](https://download.geofabrik.de/europe/united-kingdom/england/greater-london.html): *greater-london-latest-free.shp.zip*
    -   need description to better understand the data [*Format description PDF*](https://download.geofabrik.de/osm-data-in-gis-formats-free.pdf)
-   [Traffic accident](https://www.gov.uk/government/statistical-data-sets/road-safety-open-data#latest-data): *Road Safety Data - Collisions - 2024*
    -   need [code list](https://www.gov.uk/government/statistical-data-sets/road-safety-open-data#latest-data) for map the code for actual data: *dft-road-casualty-statistics-road-safety-open-dataset-data-guide-2024*
-   [Traffic flow](https://roadtraffic.dft.gov.uk/downloads)
-   [Lsoa Population](https://www.ons.gov.uk/peoplepopulationandcommunity/populationandmigration/populationestimates/datasets/lowersuperoutputareamidyearpopulationestimates): sapelsoasyoa20222024
-   [England LAD Boundary](https://geoportal.statistics.gov.uk/datasets/ons::local-authority-districts-may-2024-boundaries-uk-bfe-2/about): Local_Authority_Districts_May_2024_Boundaries_UK_BFE_5988953988717086591

## Files

1.  `ST_Full.R` is used to construct the data for models, it runs `read_data.R` in utils and run models from `STSVR.R`, `STARIMA.R`, `STGCN.R`, and `LSTMGNN.R`
2.  `TemporalEDA.R` and `SpatialEDA.R` are used for temporal and spatial exploratory data analysis respectively.
3. `STConstructor.R`: Spatio-Temporal EDA, using [emergin hotspot analysis](https://pro.arcgis.com/en/pro-app/latest/tool-reference/space-time-pattern-mining/emerginghotspots.htm) to capture sptaio-temporal patterns.
