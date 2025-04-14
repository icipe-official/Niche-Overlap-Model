**Manuscript title: Spatio-temporal dynamics of malaria vector niche overlaps in Africa**

**Authors: Eric Ali Ibrahim; John Odindi; Mark Wamalwa; Henri E. Z. Tonnang**

**Submitted to: Ecological informatics**




The study aimed to establish the spatio-temporal dynamics and explore the environmental variables that influence malaria vectors’ niche overlaps across Africa. Using environmental data from 1985 to 2021, with a monthly temporal resolution as predictors, we employed a dynamic Cellular Automata (CA) model to map niche overlap dynamics among primary (Anopheles gambiae complex, An. funestus group) and secondary (An. pharoensis, An. coustani) malaria vectors across the African continent. The codes and data shared, relates to the data processing, exploration and model implementation.  
The data processing was done in python, while exploratory analysis and model implementation  was done in R version 4.0.5.

The shared files consist of the following; 
1. "Data processing codes";  The code entails importing the african shapefile and gridding to 5 by 5 kilometers to obtain the centoids, which are then used for extracting point data from environmental variables' rasters. Further, the code entails importing the rasters, clipping to african area, reprojection and resampling, and point data extraction based on the centroids, for each timestep within the time scope. Further, the code entail point data extraction from the rasters based on vectors occurence data. We have provided the links to the raster data sources in the manucsript (Appendix A). These rasters are freely available without any restrictions. The resulting data were appended serially for each variable, and shared together with the vector occurence data sets at the icipe data repository. These datasets are used as inputs for model implementation.  
2. "Exploratory data analysis": This file contains the codes for data exploration. The input data is "species occurence data EDA" shared at the icipe data repository.
3: "Model implementation codes" : This file contains codes for model implementation in R. The code involves importing processed variables' data and the vector occurence data, converting data into spatial dataframe, model implementation, and exporting the model outputs inform of rasters. The model outputs are shared in file named "model_output_rasters.zip".  
4: Other file shared here include the "Africa Shapefile & Africa_5KM_grid" and "Model output rasters".  "Africa Shapefile & Africa_5KM_grid" file contains the shapefile of Africa and gridded Africa while "Model output rasters" file contains the raster outputs for the years 1985-2021 in .tif format.

