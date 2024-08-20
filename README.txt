R Code for running the Spatio-temporal dynamics of malaria vector niche overlaps in Africa is written in R software.This code has been tested on R version 4.0.5.
The codes consists of the following:

1. "A importing data": code for importing the variables and and shapefile of africa.
2. "B converting data spatial dataframe and rasterising" : source code  converting the importedd data to spatial dataframe and then rasterizing the variables
3. "C model implementation": source code for implementing the model.
4. "D transition rules": The trasition rules.
5. "Africa Shapefile" : shapefile of the study area
6. "Africa_5KM_grid" : grided study area. the centroids of the resulting grids were used for spatial and temporal data extraction
7. "Africa Shapefile" : shapefile of the study area
8. "Data processing codes" : codes for processing the data including geospatial gridding, raster clipping, raster point data extraction and resampling
9. "Model output rasters" : model outputs for the years 1985-2021
9. "Input data" : we have provided the links to all the were the databases containing the rasters. Additionally the occurrence data can currently be obtained through through contacting the Vector atlas team. However, the data will be publickly available at their website in near future. other processed datasets are deposited in the icipe data repository https://dmmg.icipe.org/dataportal/dataset/malaria-vectors-plant-interaction