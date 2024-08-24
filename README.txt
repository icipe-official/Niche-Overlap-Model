R Code for running the Spatio-temporal dynamics of malaria vector niche overlaps in Africa is written in R software.This code has been tested on R version 4.0.5.
The codes consists of the following:

1. "Data processing codes";  This code entails importing the african shapefile and gridding to obtain the centoids, which are then used for extracting point data from rasters forr eaach environmental variable. Further the code entails importing the rasters, clipping the to african area, reprojection, resampling and finally point data extraction based on the centroids. this is repeated for each time period. Further, the code entail point data extraction from the such rasters based on vectors occurence data. We have provided the links to the data source in the manucsript (Appendix A). The resulting were appended serially for each variable, using the "data appending code",  and shared together with the vector occurence data use in this study at the icipe data repository (https://dmmg.icipe.org/dataportal/dataset/). These datasets are used as inputrs for model implementation.  
2. "Exploratory data analysis": This file contains the codes for data exploration. 
3: "Model implementation codes" : This file contains codes for model implementation in R. The code involves importing variables processed data and the vector occurence data (shared https://dmmg.icipe.org/dataportal/dataset/spatio-temporal-dynamics-of-malaria-vector-niche-overlaps-in-africa) , converting data into spatial dataframe, model implementation, and otputing the model outputs inform of rasters. The model outputs are shared in file named "model_output_rasters.zip"  here.  
4: Other file shared here include the "Africa Shapefile", "Africa_5KM_grid",  "Model output rasters".  "Africa Shapefile" is the shapefile of the study area; "Africa_5KM_grid" is the gridded study area; "Model output rasters" contains the raster outputs for the years 1985-2021.



