R Code for running the Spatio-temporal dynamics of malaria vector niche overlaps in Africa is written in R software.This code has been tested on R version 4.0.5.
The codes consists of the following:

1. "Data processing codes";  This code entails importing the african shapefile and gridding to obtaine the centoids, which are then used for extracting point data from rasters forr eaach environmental variable. Further the code entails importing the rasters, clipping the to african area, reprojection and sampling and finally point data extraction. this is repeated for each time period. Further the code further entail point data extraction based on the occurrence of the vectors.   


The data sources for the rasters are provided in the manuscript (AppendiX A)


2. "Exploratory data analysis": 



3: "Model implementation codes" : This file contains codes for model implementation in R. The code involves importing variables processed data and the vector occurence data (shared https://dmmg.icipe.org/dataportal/dataset/spatio-temporal-dynamics-of-malaria-vector-niche-overlaps-in-africa) , converting data into spatial dataframe, model implementation, and otputing the model outputs inform of rasters. The model outputs are shared in file named "model_output_rasters.zip"  here.  

4: Other file shared here include the "shapefile of africa", "gridded Africa shapefile", 



1. "A importing data": code for importing the variables and and shapefile of africa.
2. "B converting data spatial dataframe and rasterising" : source code  converting the importedd data to spatial dataframe and then rasterizing the variables
3. "C model implementation": source code for implementing the model.
4. "D transition rules": The trasition rules.
5. "Africa Shapefile" : shapefile of the study area
6. "Africa_5KM_grid" : grided study area. the centroids of the resulting grids were used for spatial and temporal data extraction
7. "Africa Shapefile" : shapefile of the study area
8. "Data processing codes" : codes for processing the data including geospatial gridding, raster clipping, raster point data extraction and resampling
9. "Model output rasters" : model outputs for the years 1985-2021
10. "Input data" : we have provided the links to all the were the databases containing the rasters. Additionally the occurrence data can currently be obtained through through contacting the Vector atlas team. However, the data will be publickly available at their website in near future. other processed datasets are deposited in the icipe data repository https://dmmg.icipe.org/dataportal/dataset/
