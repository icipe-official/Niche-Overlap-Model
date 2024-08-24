# # Install packages (if you don't have them).
# install.packages("rgdal")
# install.packages("raster")
# install.packages("rgeos")
# install.packages("dismo")

# Load packages.
library(rgdal)
library(raster)
library(rgeos)
library(dismo)
library(ggplot2)
library(sf)
library(readxl)
library(caret)
library(terra)
library(raster)
library(dplyr)
library(lubridate)

########## PART A: Importing Data ###########################
### (A)  Tasks that are done at this stage;
## (i) importing the shapefile of interest (eg african shapefile) 
## (ii) importing spatio-temporal data on occurence of vectors and environmental variables phenomenon of interest   

### (A_i) Importing african shapefile  (We need to be generic)
africa <- readOGR("D:/variables/Africa Shapefile/Africa.shp")#,

##(A_i) import the csv data
species_data1 <- read_excel("D:/modelling niche overlaps/dataset/occurence data updated.xlsx",  sheet = "data ")  # importing data
species_data <- species_data1[complete.cases(species_data1$longitude_1),]   # ensuring the data has spatial attributes) 
species_data <- data.frame(species_data)  
species_data_eda <- species_data 

#(A_ii) subseting data to only 4 species namely; (a) GAMBIAE COMPLEX, (b) FUNESTUS COMPLEX (c) pharoensis (d) coustani, and assigning values 1-4 for the species respectively : we need to be generic here for any context
species_data$species.presence1<- ifelse(species_data$species %in% c('GAMBIAE COMPLEX','gambiae','gambiae (S)','coluzzii (gambiae M)', 
                                                                    'coluzzii (gambiae M)', 'gambiae (S/M)', 'Anopheles gambiae'), 1, 
                                        ifelse(species_data$species %in% c('Anopheles funestus', 'funestus', 'FUNESTUS COMPLEX', 'Funestus Subgroup'), 2,
                                               ifelse(species_data$species %in% c('pharoensis', 'Anopheles pharoensis'), 3,
                                                      ifelse(species_data$species %in% c('coustani', 'COUSTANI COMPLEX'), 4, NA))))  # NA means that the rest of cells is assigned NAs

species_data <- species_data[complete.cases(species_data$species.presence1),]  # here we drop the NA hence left with the records of 4 species only 
species_data <- data.frame(species_data) #just ensuring the dataset is a dataframe
species_data_r <- species_data  ## the subset data containing 4 species. 

species_data_1985_2030 <- species_data_r %>% mutate(datetime = ymd(paste(year_end, "12-01")))   # The code creates a new column called "datetime" that represents the first day of December in the year specified in the year_end column for each row of the data frame.
species_data_1985_2030_df_ftd1 <- subset(species_data_1985_2030, datetime > "1984-12-31")  # This is to subset data for all observation to be greator then 31st december 1984.

species_data_1985_2030_df_ftd2 <- species_data_1985_2030_df_ftd1[, c("datetime", setdiff(names(species_data_1985_2030_df_ftd1), "datetime"))] # just rearranging the order of to start with datetime then the order of other variables follows (not necessary) 
species_data_1985_2030_df_ftd <- species_data_1985_2030_df_ftd2  
species_data_1985_2030_df_ftd$datetime <- as.Date(species_data_1985_2030_df_ftd$datetime) # important! making datetime variable to a date variable
species_data_1985_2030_df_ftd <- species_data_1985_2030_df_ftd %>% rename(latitude = latitude_1, longitude = longitude_1) # this was for purposes of ensuring that the lati and long are named the same, as like for other variables. 

### (A_iii) importing the predictor variables predictor variables:
build_up_1985_2030 <- read.table("D:/variables/build_up_1985_2030_df.txt", header = TRUE, sep = "\t", stringsAsFactors = FALSE)
build_up_1985_2030_df <- data.frame(build_up_1985_2030)
build_up_1985_2030_df_ftd1 <- subset(build_up_1985_2030_df)
build_up_1985_2030_df_ftd2 <- build_up_1985_2030_df_ftd1[, c("datetime", setdiff(names(build_up_1985_2030_df_ftd1), "datetime"))]; build_up_1985_2030_df_ftd <- build_up_1985_2030_df_ftd2
build_up_1985_2030_df_ftd$datetime <- as.Date(build_up_1985_2030_df_ftd$datetime)

temp_range_1985_2030 <- read.table("D:/variables/max_temp_1985_2022_df.txt", header = TRUE, sep = "\t", stringsAsFactors = FALSE)
temp_range_1985_2030_df <- data.frame(temp_range_1985_2030)
temp_range_1985_2030_df_ftd1 <- subset(temp_range_1985_2030_df)
temp_range_1985_2030_df_ftd2 <- temp_range_1985_2030_df_ftd1[, c("datetime", setdiff(names(temp_range_1985_2030_df_ftd1), "datetime"))]; temp_range_1985_2030_df_ftd <- temp_range_1985_2030_df_ftd2
temp_range_1985_2030_df_ftd$datetime <- as.Date(temp_range_1985_2030_df_ftd$datetime)

elevation_1985_2030 <- read.table("D:/variables/elevation_1985_2030_df.txt", header = TRUE, sep = "\t", stringsAsFactors = FALSE)
elevation_1985_2030_df <- data.frame(elevation_1985_2030)
elevation_1985_2030_df_ftd1 <- subset(elevation_1985_2030_df)#, datetime > "2005-12-31")
elevation_1985_2030_df_ftd2 <- elevation_1985_2030_df_ftd1[, c("datetime", setdiff(names(elevation_1985_2030_df_ftd1), "datetime"))]; elevation_1985_2030_df_ftd <- elevation_1985_2030_df_ftd2
elevation_1985_2030_df_ftd$datetime <- as.Date(elevation_1985_2030_df_ftd$datetime)

irrigation_1985_2030 <- read.table("D:/variables/irrigation_1985_2030_df.txt", header = TRUE, sep = "\t", stringsAsFactors = FALSE)
irrigation_1985_2030_df <- data.frame(irrigation_1985_2030)
irrigation_1985_2030_df_ftd1 <- subset(irrigation_1985_2030_df)
irrigation_1985_2030_df_ftd2 <- irrigation_1985_2030_df_ftd1[, c("datetime", setdiff(names(irrigation_1985_2030_df_ftd1), "datetime"))]; irrigation_1985_2030_df_ftd <- irrigation_1985_2030_df_ftd2
irrigation_1985_2030_df_ftd$datetime <- as.Date(irrigation_1985_2030_df_ftd$datetime)

ndvi_1985_2030 <- read.table("D:/variables/ndvi_1985_2030_df.txt", header = TRUE, sep = "\t", stringsAsFactors = FALSE)
ndvi_1985_2030_df <- data.frame(ndvi_1985_2030)
ndvi_1985_2030_df_ftd1 <- subset(ndvi_1985_2030_df)
ndvi_1985_2030_df_ftd2 <- ndvi_1985_2030_df_ftd1[, c("datetime", setdiff(names(ndvi_1985_2030_df_ftd1), "datetime"))]; ndvi_1985_2030_df_ftd <- ndvi_1985_2030_df_ftd2
ndvi_1985_2030_df_ftd$datetime <- as.Date(ndvi_1985_2030_df_ftd$datetime)

relative_humidity_1985_2030 <- read.table("D:/variables/rel_hum_1985_2030_df.txt", header = TRUE, sep = "\t", stringsAsFactors = FALSE)
relative_humidity_1985_2030_df <- data.frame(relative_humidity_1985_2030)
relative_humidity_1985_2030_df_ftd1 <- subset(relative_humidity_1985_2030_df)
relative_humidity_1985_2030_df_ftd2 <- relative_humidity_1985_2030_df_ftd1[, c("datetime", setdiff(names(relative_humidity_1985_2030_df_ftd1), "datetime"))]; relative_humidity_1985_2030_df_ftd <- relative_humidity_1985_2030_df_ftd2
relative_humidity_1985_2030_df_ftd$datetime <- as.Date(relative_humidity_1985_2030_df_ftd2$datetime)

lulc_1985_2030 <- read.table("D:/variables/MODIS_lulc_1985_2021_df.txt", header = TRUE, sep = "\t", stringsAsFactors = FALSE)
lulc_1985_2030_df <- data.frame(lulc_1985_2030)
lulc_1985_2030_df_ftd1 <- subset(lulc_1985_2030_df)
lulc_1985_2030_df_ftd2 <- lulc_1985_2030_df_ftd1[, c("datetime", setdiff(names(lulc_1985_2030_df_ftd1), "datetime"))]; lulc_1985_2030_df_ftd <- lulc_1985_2030_df_ftd2 
lulc_1985_2030_df_ftd$datetime <- as.Date(lulc_1985_2030_df_ftd2$datetime)

precipitation_1985_2030 <- read.table("D:/variables/precipitation_1985_2021_df.txt", header = TRUE, sep = "\t", stringsAsFactors = FALSE)
precipitation_1985_2030_df <- data.frame(precipitation_1985_2030)
precipitation_1985_2030_df_ftd1 <- subset(precipitation_1985_2030_df)
precipitation_1985_2030_df_ftd2 <- precipitation_1985_2030_df_ftd1[, c("datetime", setdiff(names(precipitation_1985_2030_df_ftd1), "datetime"))]; precipitation_1985_2030_df_ftd <- precipitation_1985_2030_df_ftd2
precipitation_1985_2030_df_ftd2$datetime <- as.Date(precipitation_1985_2030_df_ftd2$datetime)




######################################## PART B: ##################################################################################################
# #@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@  Part be (B) Selecting the initial point, conversion of data to spatial point dataframe and projection @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# steps involved
# (i) Selecting the initial point from species data
# (ii) Rasterising shapefile of africa, assign projection, grid it into 5 by 5 km,  
# (iii) Converting the dataset to spatial point dataframe 
# (iv) creating a raster which contains the raster of africa and occurences overlayed in it 
# v) making the datasets that we imported in Step (A) spatial Points dataframe and assigning a projection

# (B_i) Selecting the initial point from species data
library(sf)
species_data_init <- species_data[species_data$year_end <= "1985", ]   ### filtering data to select the base year  (initial year) ie which contains initial point( eg 1985) based on the column called "end_year" and which has yearly entries 

# (B_ii) Rasterizing shapefile of africa, assign projection, grid it into 5 by 5 km, 
utm_zone <- "+proj=longlat +datum=WGS84"  # projection used 
africa_proj <- spTransform(africa, CRS(utm_zone)) # this is for assigning projection to the shapefile of africa
ext <- floor(extent(africa_proj)) # which is also the same as the code;
res11 <- 0.008983153 * 5  # defining the resolution of the grids. 5 implies 5 kilometers. Note: 1 kilometers is equivalent to  0.008983153 degrees
africa_raster_pro1 <- raster(ext, res= res11, crs= utm_zone) # creating a ratser for the africa gridded based and 5 km resolution (res1)  and projection is "+proj=longlat +datum=WGS84"

# (B_iii) Converting the dataset to spatial point dataframe
crs <- CRS("+proj=longlat +datum=WGS84")
species_data_proj <- SpatialPointsDataFrame(coords = species_data_init[, c("longitude_1", "latitude_1")], data = species_data_init, proj4string = crs)

species_data_projf <- spTransform(species_data_proj, utm_zone) # reprojecting just to ensure that it as the same projection as that of the rasterized african shapefile.

# (B_iv) creating a raster layer which contains the raster of Africa and occurrences overlayed in it 
#### Rasterizing ......
in_re <- rasterize(species_data_projf, africa_raster_pro1, field= species_data_projf$species.presence1)  # creating a raster layer (in_re) by assigning values from the species_data_projf$species.presence1 variable to the cells of the africa_raster_pro1 raster. The resulting raster will represent the spatial distribution of the species presence based on the values in the specified field.
plot(in_re) # ploting
plot(africa_proj, add=TRUE) # ploting

in_re[in_re == 1] <- 1 # represents gambiae
in_re[in_re == 2] <- 2 # represents funestus
in_re[in_re == 3] <- 3 # represents pharaoensis 
in_re[in_re == 4] <- 4 # represents Coustani
in_re[is.na(in_re)] <- 0 # all other cells which have no value are assigned to 0


# v) making the datasets that we imported in Step (A) spatial Points dataframe and assigning a projection
# ####........... for vector presence ..........
species_data_var <- SpatialPointsDataFrame(coords = species_data_1985_2030_df_ftd[, c("longitude", "latitude")], data = species_data_1985_2030_df_ftd, proj4string = crs)
species_data_var_1 <- spTransform(species_data_var, utm_zone) # just reprojecting incase need be.. this may not be necessary

# ####........... for build_up ..........
build_up_var <- SpatialPointsDataFrame(coords = build_up_1985_2030_df_ftd[, c("longitude", "latitude")], data = build_up_1985_2030_df_ftd, proj4string = crs)
build_up_var_1 <- spTransform(build_up_var, utm_zone)

####........... for temp_range   ..........
temp_range_var <- SpatialPointsDataFrame(coords = temp_range_1985_2030_df_ftd[, c("longitude", "latitude")], data = temp_range_1985_2030_df_ftd, proj4string = crs)
temp_range_var_1 <- spTransform(temp_range_var, utm_zone)

####........... elevation ..........
elevation_var <- SpatialPointsDataFrame(coords = elevation_1985_2030_df_ftd[, c("longitude", "latitude")], data = elevation_1985_2030_df_ftd, proj4string = crs)
elevation_var_1 <- spTransform(elevation_var, utm_zone)

####........... irrigation ..........
irrigation_var <- SpatialPointsDataFrame(coords = irrigation_1985_2030_df_ftd[, c("longitude", "latitude")], data = irrigation_1985_2030_df_ftd, proj4string = crs)
irrigation_var_1 <- spTransform(irrigation_var, utm_zone)
# 
# ####........... NDVI  ..........
ndvi_var <- SpatialPointsDataFrame(coords = ndvi_1985_2030_df_ftd[, c("longitude", "latitude")], data = ndvi_1985_2030_df_ftd, proj4string = crs)
ndvi_var_1 <- spTransform(ndvi_var, utm_zone)

####........... relative humidity ..........
relative_humidity_var <- SpatialPointsDataFrame(coords = relative_humidity_1985_2030_df_ftd[, c("longitude", "latitude")], data = relative_humidity_1985_2030_df_ftd, proj4string = crs)
relative_humidity_var_1 <- spTransform(relative_humidity_var, utm_zone)

# ####........... LULC ..........
lulc_var <- SpatialPointsDataFrame(coords = lulc_1985_2030_df_ftd[, c("longitude", "latitude")], data = lulc_1985_2030_df_ftd, proj4string = crs)
lulc_var_1 <- spTransform(lulc_var, utm_zone)

# ####........... precipitation ..........
precipitation_var <- SpatialPointsDataFrame(coords = precipitation_1985_2030_df_ftd[, c("longitude", "latitude")], data = precipitation_1985_2030_df_ftd, proj4string = crs)
precipitation_var_1 <- spTransform(precipitation_var, utm_zone)


######################################## PART C: ##################################################################################################
# #@@@@@@@@@@@@@@@@@@@  Part C) ordering the spatial data sequential for purposes of indentifying each timestep sequentially @@@@@@@@@@@@@@@@
# (C_i) involves creating an ordered data arranged in ascending order to ensure that we can select the data sequencially (ie at everry time step sequencially)

## the function for the same
library(data.table)
selectDataSequentially <- function(data) {
  dt <- as.data.table(data)   # Convert the data frame to a data.table
  setorder(dt, datetime)   # Sort the data.table by datetime in ascending order
  dt[, month_year := format(datetime, "%m-%Y")]   # Create a new column for month-year combination
  unique_combinations <- unique(dt$month_year)   # Get unique month-year combinations
  selected_data <- vector("list", length(unique_combinations))   # Create an empty list to store the selected data
  # Iterate over unique month-year combinations
  for (i in seq_along(unique_combinations)) {
    combination <- unique_combinations[i]     # Subset the data.table for the current combination
    selected_data[[i]] <- dt[month_year == combination, ]
  }
  dt[, month_year := NULL]   # Remove the month_year column
  return(selected_data)   # Return the selected data as a list
}

selected_data_species_t_i <- selectDataSequentially(species_data_var_1); rm(list= c("species_data_var_1")) # rm function was just to remove objects and free space)
selected_data_build_up_t_i <- selectDataSequentially(build_up_var_1); rm(list= c("build_up_var_1")) 
selected_data_temp_range_t_i <- selectDataSequentially(temp_range_var_1); rm(list= c("temp_range_var_1"))
selected_data_elevation_t_i <- selectDataSequentially(elevation_var_1); rm(list= c("elevation_var_1"))
selected_data_irrigation_t_i <- selectDataSequentially(irrigation_var_1); rm(list= c("irrigation_var_1"))
selected_data_ndvi_t_i <- selectDataSequentially(ndvi_var_1); rm(list= c("ndvi_var_1"))
selected_data_relative_humidity_t_i <- selectDataSequentially(relative_humidity_var_1); rm(list= c("relative_humidity_var_1"))
selected_data_lulc_t_i <- selectDataSequentially(lulc_var_1); rm(list= c("lulc_var_1"))
selected_data_precipitation_t_i <- selectDataSequentially(precipitation_var_1); rm(list= c("precipitation_var_1"))

############################### PLOTING THE DISTRIBUTION OF THE species #####
in_re # recalling the initial raster overlay 

# Identify cells with the target value
ir_cells <- which(in_re[] > 0, arr.ind = TRUE) # cells whose values are greater than 0 (recall 1 - gambiae, 2 - funestus, 3-pharaonsis, 4 - coustani) so that we will find neighbourhoods of such cells
plot(in_re) # not necessary but just a plot to ensure that the cells are overlayed to the raster.

######################################## PART D: Modeling ##################################################################################################
# NB the above was part of data processing; bellow is the the model implementation; as we adopt this we out to be as generic as possible @@@@@@@@@@@@@@@@

for (i in 1:60) {  # i represents the timesteps eg  eg i= 1 is the first timestep, and 60 was the maximum timestep
  species_data_t_i <- selected_data_species_t_i [[i]] # this calls the data for the timestep involved, then converts its to spatial point dataframe, assigns projection to it then creates a raster 
  crs <- CRS("+proj=longlat +datum=WGS84")
  utm_zone <- "+proj=longlat +datum=WGS84"
  species_data_t_i_spdf <- SpatialPointsDataFrame(coords = species_data_t_i[, c("longitude", "latitude")], data = species_data_t_i, proj4string = crs)
  species_data_t_i_spdf_spt <- spTransform(species_data_t_i_spdf, utm_zone) # just incase the reprojection was required
  species_data_raster_t_i <- rasterize(species_data_t_i_spdf_spt, africa_raster_pro1, field= species_data_t_i_spdf$species.presence1)
  
  build_up_t_i <- selected_data_build_up_t_i [[i]] # same as above for all variables involved 
  build_up_t_i_spdf <- SpatialPointsDataFrame(coords = build_up_t_i[, c("longitude", "latitude")], data = build_up_t_i, proj4string = crs)
  build_up_t_i_spdf_spt <- spTransform(build_up_t_i_spdf, utm_zone)
  build_up_raster_t_i <- rasterize(build_up_t_i_spdf_spt, africa_raster_pro1, field= build_up_t_i_spdf$build_up)
  
  temp_range_t_i <- selected_data_temp_range_t_i [[i]]
  temp_range_t_i_spdf <- SpatialPointsDataFrame(coords = temp_range_t_i[, c("longitude", "latitude")], data = temp_range_t_i, proj4string = crs)
  temp_range_t_i_spdf_spt <- spTransform(temp_range_t_i_spdf, utm_zone)
  temp_range_raster_t_i <- rasterize(temp_range_t_i_spdf_spt, africa_raster_pro1, field= temp_range_t_i_spdf$max_temp)
  
  elevation_t_i <- selected_data_elevation_t_i [[i]]
  elevation_t_i_spdf <- SpatialPointsDataFrame(coords = elevation_t_i[, c("longitude", "latitude")], data = elevation_t_i, proj4string = crs)
  elevation_t_i_spdf_spt <- spTransform(elevation_t_i_spdf, utm_zone)
  elevation_raster_t_i <- rasterize(elevation_t_i_spdf_spt, africa_raster_pro1, field= elevation_t_i_spdf$elevation)
  
  irrigation_t_i <- selected_data_irrigation_t_i [[i]]
  irrigation_t_i_spdf <- SpatialPointsDataFrame(coords = irrigation_t_i[, c("longitude", "latitude")], data = irrigation_t_i, proj4string = crs)
  irrigation_t_i_spdf_spt <- spTransform(irrigation_t_i_spdf, utm_zone)
  irrigation_raster_t_i <- rasterize(irrigation_t_i_spdf_spt, africa_raster_pro1, field= irrigation_t_i_spdf$irrigation)
  
  ndvi_t_i <- selected_data_ndvi_t_i [[i]]
  ndvi_t_i_spdf <- SpatialPointsDataFrame(coords = ndvi_t_i[, c("longitude", "latitude")], data = ndvi_t_i, proj4string = crs)
  ndvi_t_i_spdf_spt <- spTransform(ndvi_t_i_spdf, utm_zone)
  ndvi_raster_t_i <- rasterize(ndvi_t_i_spdf_spt, africa_raster_pro1, field= ndvi_t_i_spdf$ndvi)
  
  relative_humidity_t_i <- selected_data_relative_humidity_t_i [[i]]
  relative_humidity_t_i_spdf <- SpatialPointsDataFrame(coords = relative_humidity_t_i[, c("longitude", "latitude")], data = relative_humidity_t_i, proj4string = crs)
  relative_humidity_t_i_spdf_spt <- spTransform(relative_humidity_t_i_spdf, utm_zone)
  relative_humidity_raster_t_i <- rasterize(relative_humidity_t_i_spdf_spt, africa_raster_pro1, field= relative_humidity_t_i_spdf$rel_hum)
  
  lulc_t_i <- selected_data_lulc_t_i [[i]]
  lulc_t_i_spdf <- SpatialPointsDataFrame(coords = lulc_t_i[, c("longitude", "latitude")], data = lulc_t_i, proj4string = crs)
  lulc_t_i_spdf_spt <- spTransform(lulc_t_i_spdf, utm_zone)
  lulc_raster_t_i <- rasterize(lulc_t_i_spdf_spt, africa_raster_pro1, field= lulc_t_i_spdf$lulc)
  
  precipitation_t_i <- selected_data_precipitation_t_i [[i]]
  precipitation_t_i_spdf <- SpatialPointsDataFrame(coords = precipitation_t_i[, c("longitude", "latitude")], data = precipitation_t_i, proj4string = crs)
  precipitation_t_i_spdf_spt <- spTransform(precipitation_t_i_spdf, utm_zone)
  precipitation_raster_t_i <- rasterize(precipitation_t_i_spdf_spt, africa_raster_pro1, field= precipitation_t_i_spdf$prec)
  
  
  for (j in 1:12) { 
    for (k in 1:2) { # if within a month we expect the distance covered to be like extended moore neighbourhood eg 2 cells other than one for example. 
      # Applying transition rules
      
      in_re_new1 = in_re # this is the overlayed raster at initial timestep ( )
      plot(in_re_new1)
      
      # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ Establishing cells which has values 1 - 4 @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
      ir_reported_cells_1 <- which(values(in_re_new1) == 1)
      ir_reported_cells_2 <- which(values(in_re_new1) == 2)
      ir_reported_cells_3 <- which(values(in_re_new1) == 3)
      ir_reported_cells_4 <- which(values(in_re_new1) == 4)
      
      # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ determining the neibourhoods of the identified cells @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
      neibouring_cells_1 <- adjacent(in_re_new, cell=ir_reported_cells_1, direction=8,include=F,pairs=F)
      neibouring_cells_2 <- adjacent(in_re_new, cell=ir_reported_cells_2, direction=8,include=F,pairs=F)
      neibouring_cells_3 <- adjacent(in_re_new, cell=ir_reported_cells_3, direction=8,include=F,pairs=F)
      neibouring_cells_4 <- adjacent(in_re_new, cell=ir_reported_cells_4, direction=8,include=F,pairs=F)
      
      # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ establishing the intersections between the neighborhoods @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
      shared_neighborhood_1_2 <- intersect(neibouring_cells_1, neibouring_cells_2) # this implies intersection of the neighbouhood of cell 1 and cell 2 
      shared_neighborhood_1_3 <- intersect(neibouring_cells_1, neibouring_cells_3)
      shared_neighborhood_1_4 <- intersect(neibouring_cells_1, neibouring_cells_4)
      
      shared_neighborhood_2_3 <- intersect(neibouring_cells_2, neibouring_cells_3)
      shared_neighborhood_2_4 <- intersect(neibouring_cells_2, neibouring_cells_4)
      shared_neighborhood_3_4 <- intersect(neibouring_cells_3, neibouring_cells_4)
      
      shared_neighborhood_1_2_3 <- intersect(shared_neighborhood_1_2, neibouring_cells_3)
      shared_neighborhood_1_2_4 <- intersect(shared_neighborhood_1_2, neibouring_cells_4)
      shared_neighborhood_1_3_4 <- intersect(shared_neighborhood_1_3, neibouring_cells_4)
      shared_neighborhood_2_3_4 <- intersect(shared_neighborhood_2_3, neibouring_cells_4)
      
      shared_neighborhood_1_2_3_4 <- intersect(shared_neighborhood_1_2_3, neibouring_cells_4)
      
      # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ identifying values assigned values 5 - 14 were each stands for an overlap @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
      ir_reported_cells_5 <- which(values(in_re_new1) == 5)
      ir_reported_cells_6 <- which(values(in_re_new1) == 6)
      ir_reported_cells_7 <- which(values(in_re_new1) == 7)
      ir_reported_cells_8 <- which(values(in_re_new1) == 8)
      ir_reported_cells_9 <- which(values(in_re_new1) == 9)
      ir_reported_cells_10 <- which(values(in_re_new1) == 10)
      ir_reported_cells_11 <- which(values(in_re_new1) == 11)
      ir_reported_cells_12 <- which(values(in_re_new1) == 12)
      ir_reported_cells_13 <- which(values(in_re_new1) == 13)
      ir_reported_cells_14 <- which(values(in_re_new1) == 14)
      ir_reported_cells_15 <- which(values(in_re_new1) == 15)
      
      # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ determining the neibourhoods of the identified cells @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
      neibouring_cells_1_2 <- adjacent(in_re_new, cell=ir_reported_cells_5, direction=8,include=F,pairs=F)
      neibouring_cells_1_3 <- adjacent(in_re_new, cell=ir_reported_cells_6, direction=8,include=F,pairs=F)
      neibouring_cells_1_4 <- adjacent(in_re_new, cell=ir_reported_cells_7, direction=8,include=F,pairs=F)
      neibouring_cells_2_3 <- adjacent(in_re_new, cell=ir_reported_cells_8, direction=8,include=F,pairs=F)
      
      neibouring_cells_2_4 <- adjacent(in_re_new, cell=ir_reported_cells_9, direction=8,include=F,pairs=F)
      neibouring_cells_3_4 <- adjacent(in_re_new, cell=ir_reported_cells_10, direction=8,include=F,pairs=F)
      neibouring_cells_1_2_3 <- adjacent(in_re_new, cell=ir_reported_cells_11, direction=8,include=F,pairs=F)
      neibouring_cells_1_2_4 <- adjacent(in_re_new, cell=ir_reported_cells_12, direction=8,include=F,pairs=F)
      
      neibouring_cells_1_3_4 <- adjacent(in_re_new, cell=ir_reported_cells_13, direction=8,include=F,pairs=F)
      neibouring_cells_2_3_4 <- adjacent(in_re_new, cell=ir_reported_cells_14, direction=8,include=F,pairs=F)
      neibouring_cells_1_2_3_4 <- adjacent(in_re_new, cell=ir_reported_cells_15, direction=8,include=F,pairs=F)
      
      # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ establishing the intersections between the neighborhoods of the cells above @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
      neibouring_cells_1_2_c1  <- intersect(neibouring_cells_1_2, ir_reported_cells_1)
      neibouring_cells_1_2_c2  <- intersect(neibouring_cells_1_2, ir_reported_cells_2)
      neibouring_cells_1_2_c3  <- intersect(neibouring_cells_1_2, ir_reported_cells_3)
      neibouring_cells_1_2_c4  <- intersect(neibouring_cells_1_2, ir_reported_cells_4)
      
      neibouring_cells_1_3_c1  <- intersect(neibouring_cells_1_3, ir_reported_cells_1)
      neibouring_cells_1_3_c2  <- intersect(neibouring_cells_1_3, ir_reported_cells_2)
      neibouring_cells_1_3_c3  <- intersect(neibouring_cells_1_3, ir_reported_cells_3)
      neibouring_cells_1_3_c4  <- intersect(neibouring_cells_1_3, ir_reported_cells_4)
      
      neibouring_cells_1_4_c1  <- intersect(neibouring_cells_1_4, ir_reported_cells_1)
      neibouring_cells_1_4_c2  <- intersect(neibouring_cells_1_4, ir_reported_cells_2)
      neibouring_cells_1_4_c3  <- intersect(neibouring_cells_1_4, ir_reported_cells_3)
      neibouring_cells_1_4_c4  <- intersect(neibouring_cells_1_4, ir_reported_cells_4)
      
      neibouring_cells_2_3_c1  <- intersect(neibouring_cells_2_3, ir_reported_cells_1)
      neibouring_cells_2_3_c2  <- intersect(neibouring_cells_2_3, ir_reported_cells_2)
      neibouring_cells_2_3_c3  <- intersect(neibouring_cells_2_3, ir_reported_cells_3)
      neibouring_cells_2_3_c4  <- intersect(neibouring_cells_2_3, ir_reported_cells_4)
      
      neibouring_cells_2_4_c1  <- intersect(neibouring_cells_2_4, ir_reported_cells_1)
      neibouring_cells_2_4_c2  <- intersect(neibouring_cells_2_4, ir_reported_cells_2)
      neibouring_cells_2_4_c3  <- intersect(neibouring_cells_2_4, ir_reported_cells_3)
      neibouring_cells_2_4_c4  <- intersect(neibouring_cells_2_4, ir_reported_cells_4)
      
      neibouring_cells_3_4_c1  <- intersect(neibouring_cells_3_4, ir_reported_cells_1)
      neibouring_cells_3_4_c2  <- intersect(neibouring_cells_3_4, ir_reported_cells_2)
      neibouring_cells_3_4_c3  <- intersect(neibouring_cells_3_4, ir_reported_cells_3)
      neibouring_cells_3_4_c4  <- intersect(neibouring_cells_3_4, ir_reported_cells_4)
      
      neibouring_cells_1_2_3_c1  <- intersect(neibouring_cells_1_2_3, ir_reported_cells_1)
      neibouring_cells_1_2_3_c2  <- intersect(neibouring_cells_1_2_3, ir_reported_cells_2)
      neibouring_cells_1_2_3_c3  <- intersect(neibouring_cells_1_2_3, ir_reported_cells_3)
      neibouring_cells_1_2_3_c4  <- intersect(neibouring_cells_1_2_3, ir_reported_cells_4)
      
      neibouring_cells_1_2_4_c1  <- intersect(neibouring_cells_1_2_4, ir_reported_cells_1)
      neibouring_cells_1_2_4_c2  <- intersect(neibouring_cells_1_2_4, ir_reported_cells_2)
      neibouring_cells_1_2_4_c3  <- intersect(neibouring_cells_1_2_4, ir_reported_cells_3)
      neibouring_cells_1_2_4_c4  <- intersect(neibouring_cells_1_2_4, ir_reported_cells_4)
      
      neibouring_cells_1_3_4_c1  <- intersect(neibouring_cells_1_3_4, ir_reported_cells_1)
      neibouring_cells_1_3_4_c2  <- intersect(neibouring_cells_1_3_4, ir_reported_cells_2)
      neibouring_cells_1_3_4_c3  <- intersect(neibouring_cells_1_3_4, ir_reported_cells_3)
      neibouring_cells_1_3_4_c4  <- intersect(neibouring_cells_1_3_4, ir_reported_cells_4)
      
      neibouring_cells_2_3_4_c1  <- intersect(neibouring_cells_2_3_4, ir_reported_cells_1)
      neibouring_cells_2_3_4_c2  <- intersect(neibouring_cells_2_3_4, ir_reported_cells_2)
      neibouring_cells_2_3_4_c3  <- intersect(neibouring_cells_2_3_4, ir_reported_cells_3)
      neibouring_cells_2_3_4_c4  <- intersect(neibouring_cells_2_3_4, ir_reported_cells_4)
      
      neibouring_cells_1_2_3_4_c1  <- intersect(neibouring_cells_1_2_3_4, ir_reported_cells_1)
      neibouring_cells_1_2_3_4_c2  <- intersect(neibouring_cells_1_2_3_4, ir_reported_cells_2)
      neibouring_cells_1_2_3_4_c3  <- intersect(neibouring_cells_1_2_3_4, ir_reported_cells_3)
      neibouring_cells_1_2_3_4_c4  <- intersect(neibouring_cells_1_2_3_4, ir_reported_cells_4)
      
      # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ for all neighborhoods and intersections, we identify them uniquely @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
      
      # normal cells
      in_re_new1[neibouring_cells_1] <- 0.5
      in_re_new1[neibouring_cells_2] <- 1.5
      in_re_new1[neibouring_cells_3] <- 2.5
      in_re_new1[neibouring_cells_4] <- 3.5
      
      # intersects of neighbourhoods normal cells
      in_re_new1[neibouring_cells_1_2_c1] <- 4.5
      in_re_new1[neibouring_cells_1_2_c2] <- 4.5
      in_re_new1[neibouring_cells_1_2_c3] <- 10.5
      in_re_new1[neibouring_cells_1_2_c4] <- 11.5
      
      in_re_new1[neibouring_cells_1_3_c1] <- 5.5
      in_re_new1[neibouring_cells_1_3_c2] <- 10.5
      in_re_new1[neibouring_cells_1_3_c3] <- 5.5
      in_re_new1[neibouring_cells_1_3_c4] <- 12.5
      
      in_re_new1[neibouring_cells_1_4_c1] <- 6.5
      in_re_new1[neibouring_cells_1_4_c2] <- 11.5
      in_re_new1[neibouring_cells_1_4_c3] <- 12.5
      in_re_new1[neibouring_cells_1_4_c4] <- 6.5
      
      in_re_new1[neibouring_cells_2_3_c1] <- 10.5
      in_re_new1[neibouring_cells_2_3_c2] <- 7.5
      in_re_new1[neibouring_cells_2_3_c3] <- 7.5
      in_re_new1[neibouring_cells_2_3_c4] <- 13.5
      
      in_re_new1[neibouring_cells_2_4_c1] <- 11.5
      in_re_new1[neibouring_cells_2_4_c2] <- 8.5
      in_re_new1[neibouring_cells_2_4_c3] <- 13.5
      in_re_new1[neibouring_cells_2_4_c4] <- 8.5
      
      in_re_new1[neibouring_cells_3_4_c1] <- 12.5
      in_re_new1[neibouring_cells_3_4_c2] <- 13.5
      in_re_new1[neibouring_cells_3_4_c3] <- 9.5
      in_re_new1[neibouring_cells_3_4_c4] <- 9.5
      
      in_re_new1[neibouring_cells_1_2_3_c1] <- 10.5
      in_re_new1[neibouring_cells_1_2_3_c2] <- 10.5
      in_re_new1[neibouring_cells_1_2_3_c3] <- 10.5
      in_re_new1[neibouring_cells_1_2_3_c4] <- 14.5
      
      in_re_new1[neibouring_cells_1_2_4_c1] <- 11.5
      in_re_new1[neibouring_cells_1_2_4_c2] <- 11.5
      in_re_new1[neibouring_cells_1_2_4_c3] <- 14.5
      in_re_new1[neibouring_cells_1_2_4_c4] <- 11.5
      
      in_re_new1[neibouring_cells_1_3_4_c1] <- 12.5
      in_re_new1[neibouring_cells_1_3_4_c2] <- 14.5
      in_re_new1[neibouring_cells_1_3_4_c3] <- 12.5
      in_re_new1[neibouring_cells_1_3_4_c4] <- 12.5
      
      in_re_new1[neibouring_cells_2_3_4_c1] <- 14.5
      in_re_new1[neibouring_cells_2_3_4_c2] <- 13.5
      in_re_new1[neibouring_cells_2_3_4_c3] <- 13.5
      in_re_new1[neibouring_cells_2_3_4_c4] <- 13.5
      
      in_re_new1[neibouring_cells_1_2_3_4_c1] <- 14.5
      in_re_new1[neibouring_cells_1_2_3_4_c2] <- 14.5
      in_re_new1[neibouring_cells_1_2_3_4_c3] <- 14.5
      in_re_new1[neibouring_cells_1_2_3_4_c4] <- 14.5
      
      # intersects of normal cells  
      in_re_new1[neibouring_cells_1_2] <- 4.5
      in_re_new1[neibouring_cells_1_3] <- 5.5
      in_re_new1[neibouring_cells_1_4] <- 6.5
      in_re_new1[neibouring_cells_2_3] <- 7.5
      in_re_new1[neibouring_cells_2_4] <- 8.5
      in_re_new1[neibouring_cells_3_4] <- 9.5
      #
      in_re_new1[neibouring_cells_1_2_3] <- 10.5
      in_re_new1[neibouring_cells_1_2_4] <- 11.5
      in_re_new1[neibouring_cells_1_3_4] <- 12.5
      in_re_new1[neibouring_cells_2_3_4] <- 13.5
      
      in_re_new1[neibouring_cells_1_2_3_4] <- 14.5
      
      ############ applying transition rules for the model@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ 
      
      in_re_new1[(in_re_new1==0.5 &   ## in_re_new1==0.5 implies the neighborhood of the cell whose state is 1 (ie anopheles gambiae)
                    build_up_raster_t_i >= 0.000 & build_up_raster_t_i <= 206.940 &  # this captures the conditions of the buildups ..
                    temp_range_raster_t_i >= 18.233 & temp_range_raster_t_i <=  35.00 & 
                    elevation_raster_t_i >= 0 & elevation_raster_t_i <= 2804.0 & 
                    irrigation_raster_t_i >= 0 & irrigation_raster_t_i <= 1467.2 & 
                    ndvi_raster_t_i >= -0.08956 & ndvi_raster_t_i <= 0.851 & 
                    relative_humidity_raster_t_i >= 22.47 & relative_humidity_raster_t_i <= 89.730 &
                    lulc_raster_t_i != 1 & lulc_raster_t_i != 3 & 
                    precipitation_raster_t_i >= 0 & precipitation_raster_t_i <= 2138871)|   # | stands for or 
                   (in_re_new1==0.5 & # this was included for temperatures above 35 degrees to know how other variables boundaries change to avoid over-generalization
                      build_up_raster_t_i >= 0.000 & build_up_raster_t_i <= 206.940 & 
                      temp_range_raster_t_i > 35 & temp_range_raster_t_i <=  37.74 & 
                      elevation_raster_t_i >= 2 & elevation_raster_t_i <= 969.0 & 
                      irrigation_raster_t_i >= 0 & irrigation_raster_t_i <= 364.78 & 
                      ndvi_raster_t_i >= 0.07714 & ndvi_raster_t_i <= 0.5964 & 
                      relative_humidity_raster_t_i >= 22.47 & relative_humidity_raster_t_i <= 63.02 & 
                      lulc_raster_t_i != 1 & lulc_raster_t_i != 3 & 
                      precipitation_raster_t_i >= 0 & precipitation_raster_t_i <= 2138871)
                 |(species_data_raster_t_i==1)] = 1 # means assign neighboring cell (cell indentified as 0.5) which meets these conditions 1 for that specific timestep 
      
      in_re_new1[(in_re_new1==1.5 &  ## in_re_new1==0.5 implies the neighborhood of the cell whose state is 2 (ie anopheles funestus)
                    build_up_raster_t_i >= 0 & build_up_raster_t_i <= 202.800 & 
                    temp_range_raster_t_i >= 21.29 & temp_range_raster_t_i <= 35.00 & 
                    elevation_raster_t_i >= 0 & elevation_raster_t_i <= 2527.000 & 
                    irrigation_raster_t_i >= 0 & irrigation_raster_t_i <= 1467.21 & 
                    ndvi_raster_t_i >= -0.07879 & ndvi_raster_t_i <= 0.851 & 
                    relative_humidity_raster_t_i >= 40.50 & relative_humidity_raster_t_i <= 88.61 & 
                    lulc_raster_t_i != 1 & lulc_raster_t_i != 3 & 
                    precipitation_raster_t_i >= 0 & precipitation_raster_t_i <= 2138871)|
                   (in_re_new1==1.5 & # this was included for temperatures above 35 degrees to know how other variables boundaries change to avoid over-generalization
                      build_up_raster_t_i >= 0 & build_up_raster_t_i <= 130.740 & 
                      temp_range_raster_t_i >= 35 & temp_range_raster_t_i <= 37.12 & 
                      elevation_raster_t_i >= 9.0 & elevation_raster_t_i <= 747.000 & 
                      irrigation_raster_t_i >= 0 & irrigation_raster_t_i <= 225.02 & 
                      ndvi_raster_t_i >= -0.07879 & ndvi_raster_t_i <= 0.851 & 
                      relative_humidity_raster_t_i >= 40.50 & relative_humidity_raster_t_i <= 55.87 & 
                      lulc_raster_t_i != 1 & lulc_raster_t_i != 3 & 
                      precipitation_raster_t_i >= 0 & precipitation_raster_t_i <= 2138871)
                 |(species_data_raster_t_i==2)] = 2 
      
      in_re_new1[(in_re_new1==2.5 &  # for species 3 which is pharaoensis, its neighbourhood is assigned 2.5
                    build_up_raster_t_i >= 0 & build_up_raster_t_i <= 137.535 & 
                    temp_range_raster_t_i >= 24.03 & temp_range_raster_t_i <= 35 & 
                    elevation_raster_t_i >= 0 & elevation_raster_t_i <= 2031.0 & 
                    irrigation_raster_t_i >= 0 & irrigation_raster_t_i <= 485.37 & 
                    ndvi_raster_t_i >= -0.1335 & ndvi_raster_t_i <= 0.793 &
                    relative_humidity_raster_t_i >= 38.14 & relative_humidity_raster_t_i <= 85.34 & 
                    lulc_raster_t_i != 1 & lulc_raster_t_i != 3 & 
                    precipitation_raster_t_i >= 0 & precipitation_raster_t_i <= 876076
      )|
        (in_re_new1==2.5 & 
           build_up_raster_t_i >= 0 & build_up_raster_t_i <= 137.535 & 
           temp_range_raster_t_i > 35 & temp_range_raster_t_i <= 38.06 & 
           irrigation_raster_t_i >= 0 & irrigation_raster_t_i <= 485.37 & 
           ndvi_raster_t_i >= -0.1335 & ndvi_raster_t_i <= 0.793 & 
           relative_humidity_raster_t_i >= 38.14 & relative_humidity_raster_t_i <= 55.87 & 
           shrubs_raster_t_i >= 1 & shrubs_raster_t_i <= 328.90 &
           lulc_raster_t_i != 1 & lulc_raster_t_i != 3 & 
           precipitation_raster_t_i >= 0 & precipitation_raster_t_i <= 876076)
      |(species_data_raster_t_i==3)] = 3 
      
      in_re_new1[(in_re_new1==3.5 &  # for species 4 which is coustani, its neighbourhood is assigned 3.5
                    build_up_raster_t_i >= 0 & build_up_raster_t_i <= 84.345 & 
                    temp_range_raster_t_i >= 20.30 & temp_range_raster_t_i <= 35 & 
                    elevation_raster_t_i >= 0.0 & elevation_raster_t_i <= 2056.0 & 
                    irrigation_raster_t_i >= 0.0 & irrigation_raster_t_i <= 468.11 & 
                    ndvi_raster_t_i >= -0.07879 & ndvi_raster_t_i <= 0.784 & 
                    relative_humidity_raster_t_i >= 44.85 & relative_humidity_raster_t_i <= 84.61 & 
                    lulc_raster_t_i != 1 & lulc_raster_t_i != 3 & 
                    precipitation_raster_t_i >= 61 & precipitation_raster_t_i <= 2138871)
                 |(in_re_new1==3.5 & 
                     build_up_raster_t_i >= 0 & build_up_raster_t_i <= 84.345 & 
                     temp_range_raster_t_i > 35 & temp_range_raster_t_i <= 37.29 & 
                     elevation_raster_t_i >= 0.0 & elevation_raster_t_i <= 468.11 & 
                     irrigation_raster_t_i >= 0.0 & irrigation_raster_t_i <= 485.37 & 
                     ndvi_raster_t_i >= -0.07879 & ndvi_raster_t_i <= 0.784 & 
                     relative_humidity_raster_t_i >= 44.85 & relative_humidity_raster_t_i <= 53.16 & 
                     lulc_raster_t_i != 1 & lulc_raster_t_i != 3 & 
                     precipitation_raster_t_i >= 61 & precipitation_raster_t_i <= 2138871)
                 |(species_data_raster_t_i==4)] = 4 
      
      ### @@@@@@@@@@@@@@@@@@ continuation; this involves were we have intersections of neighbourhoods, or neighbourhood and cell occupied by a species but not a neighbourhood cell
      in_re_new1[(in_re_new1==4.5 & 
                    build_up_raster_t_i >= 0 & build_up_raster_t_i <= 202.800 & 
                    temp_range_raster_t_i >= 21.290  & temp_range_raster_t_i <= 35.000 & 
                    elevation_raster_t_i >= 0 & elevation_raster_t_i <= 2527.0 & 
                    irrigation_raster_t_i >= 0  & irrigation_raster_t_i <= 1467.205 & 
                    ndvi_raster_t_i >= -0.079 & ndvi_raster_t_i <= 0.851 & 
                    relative_humidity_raster_t_i >= 40.500 & relative_humidity_raster_t_i <= 88.610 & 
                    lulc_raster_t_i != 1 & lulc_raster_t_i != 3 & 
                    precipitation_raster_t_i >= 0 & precipitation_raster_t_i <= 2138871)
                 |(species_data_raster_t_i== 1| species_data_raster_t_i==2)] = 5
      
      in_re_new1[(in_re_new1==4.5 & 
                    build_up_raster_t_i >= 0.000 & build_up_raster_t_i <= 206.940 & 
                    temp_range_raster_t_i >= 18.233 & temp_range_raster_t_i <=  35.00 & 
                    elevation_raster_t_i >= 0 & elevation_raster_t_i <= 2804.0 & 
                    irrigation_raster_t_i >= 0 & irrigation_raster_t_i <= 1467.2 & 
                    ndvi_raster_t_i >= -0.08956 & ndvi_raster_t_i <= 0.851 & 
                    relative_humidity_raster_t_i >= 22.47 & relative_humidity_raster_t_i <= 89.730 &
                    lulc_raster_t_i != 1 & lulc_raster_t_i != 3 & 
                    precipitation_raster_t_i >= 0 & precipitation_raster_t_i <= 2138871)|
                   (in_re_new1==4.5 & 
                      build_up_raster_t_i >= 0.000 & build_up_raster_t_i <= 206.940 & 
                      temp_range_raster_t_i > 35 & temp_range_raster_t_i <=  37.74 & 
                      elevation_raster_t_i >= 2 & elevation_raster_t_i <= 969.0 & 
                      irrigation_raster_t_i >= 0 & irrigation_raster_t_i <= 364.78 & 
                      ndvi_raster_t_i >= 0.07714 & ndvi_raster_t_i <= 0.5964 & 
                      relative_humidity_raster_t_i >= 22.47 & relative_humidity_raster_t_i <= 63.02 & 
                      lulc_raster_t_i != 1 & lulc_raster_t_i != 3 & 
                      precipitation_raster_t_i >= 0 & precipitation_raster_t_i <= 2138871)
                 |(species_data_raster_t_i==1)] = 1 
      
      in_re_new1[(in_re_new1==4.5 &
                    build_up_raster_t_i >= 0 & build_up_raster_t_i <= 202.800 & 
                    temp_range_raster_t_i >= 21.29 & temp_range_raster_t_i <= 35.00 & 
                    elevation_raster_t_i >= 0 & elevation_raster_t_i <= 2527.000 & 
                    irrigation_raster_t_i >= 0 & irrigation_raster_t_i <= 1467.21 & 
                    ndvi_raster_t_i >= -0.07879 & ndvi_raster_t_i <= 0.851 & 
                    relative_humidity_raster_t_i >= 40.50 & relative_humidity_raster_t_i <= 88.61 & 
                    lulc_raster_t_i != 1 & lulc_raster_t_i != 3 & 
                    precipitation_raster_t_i >= 0 & precipitation_raster_t_i <= 2138871)|
                   (in_re_new1==4.5 &
                      build_up_raster_t_i >= 0 & build_up_raster_t_i <= 130.740 & 
                      temp_range_raster_t_i >= 35 & temp_range_raster_t_i <= 37.12 & 
                      elevation_raster_t_i >= 9.0 & elevation_raster_t_i <= 747.000 & 
                      irrigation_raster_t_i >= 0 & irrigation_raster_t_i <= 225.02 & 
                      ndvi_raster_t_i >= -0.07879 & ndvi_raster_t_i <= 0.851 & 
                      relative_humidity_raster_t_i >= 40.50 & relative_humidity_raster_t_i <= 55.87 & 
                      lulc_raster_t_i != 1 & lulc_raster_t_i != 3 & 
                      precipitation_raster_t_i >= 0 & precipitation_raster_t_i <= 2138871)
                 |(species_data_raster_t_i==2)] = 2 
      
      ### @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
      in_re_new1[(in_re_new1==5.5 & 
                    build_up_raster_t_i >= 0 & build_up_raster_t_i <= 137.535 & 
                    temp_range_raster_t_i >= 24.030 & temp_range_raster_t_i <= 35 & 
                    elevation_raster_t_i >= 0 & elevation_raster_t_i <= 2031.000 & 
                    irrigation_raster_t_i >= 0 & irrigation_raster_t_i <= 485.37 & 
                    ndvi_raster_t_i >= -0.090 & ndvi_raster_t_i <= 0.793 & 
                    relative_humidity_raster_t_i >= 30.305 & relative_humidity_raster_t_i <= 85.340 & 
                    lulc_raster_t_i != 1 & lulc_raster_t_i != 3 & 
                    precipitation_raster_t_i >= 0 & precipitation_raster_t_i <= 876076)
                 |(species_data_raster_t_i==1 & species_data_raster_t_i==3)] = 6
      
      in_re_new1[(in_re_new1==5.5 & 
                    build_up_raster_t_i >= 0.000 & build_up_raster_t_i <= 206.940 & 
                    temp_range_raster_t_i >= 18.233 & temp_range_raster_t_i <=  35.00 & 
                    elevation_raster_t_i >= 0 & elevation_raster_t_i <= 2804.0 & 
                    irrigation_raster_t_i >= 0 & irrigation_raster_t_i <= 1467.2 & 
                    ndvi_raster_t_i >= -0.08956 & ndvi_raster_t_i <= 0.851 & 
                    relative_humidity_raster_t_i >= 22.47 & relative_humidity_raster_t_i <= 89.730 &
                    lulc_raster_t_i != 1 & lulc_raster_t_i != 3 & 
                    precipitation_raster_t_i >= 0 & precipitation_raster_t_i <= 2138871)|
                   (in_re_new1==5.5 & 
                      build_up_raster_t_i >= 0.000 & build_up_raster_t_i <= 206.940 & 
                      temp_range_raster_t_i > 35 & temp_range_raster_t_i <=  37.74 & 
                      elevation_raster_t_i >= 2 & elevation_raster_t_i <= 969.0 & 
                      irrigation_raster_t_i >= 0 & irrigation_raster_t_i <= 364.78 & 
                      ndvi_raster_t_i >= 0.07714 & ndvi_raster_t_i <= 0.5964 & 
                      relative_humidity_raster_t_i >= 22.47 & relative_humidity_raster_t_i <= 63.02 & 
                      lulc_raster_t_i != 1 & lulc_raster_t_i != 3 & 
                      precipitation_raster_t_i >= 0 & precipitation_raster_t_i <= 2138871)
                 |(species_data_raster_t_i==1)] = 1 
      
      in_re_new1[(in_re_new1==5.5 & 
                    build_up_raster_t_i >= 0 & build_up_raster_t_i <= 137.535 & 
                    temp_range_raster_t_i >= 24.03 & temp_range_raster_t_i <= 35 & 
                    elevation_raster_t_i >= 0 & elevation_raster_t_i <= 2031.0 & 
                    irrigation_raster_t_i >= 0 & irrigation_raster_t_i <= 485.37 & 
                    ndvi_raster_t_i >= -0.1335 & ndvi_raster_t_i <= 0.793 &
                    relative_humidity_raster_t_i >= 38.14 & relative_humidity_raster_t_i <= 85.34 & 
                    lulc_raster_t_i != 1 & lulc_raster_t_i != 3 & 
                    precipitation_raster_t_i >= 0 & precipitation_raster_t_i <= 876076)|
                   (in_re_new1==5.5 & 
                      build_up_raster_t_i >= 0 & build_up_raster_t_i <= 137.535 & 
                      temp_range_raster_t_i > 35 & temp_range_raster_t_i <= 38.06 & 
                      elevation_raster_t_i >= 0 & elevation_raster_t_i <= 747.0 & 
                      irrigation_raster_t_i >= 0 & irrigation_raster_t_i <= 485.37 & 
                      ndvi_raster_t_i >= -0.1335 & ndvi_raster_t_i <= 0.793 & 
                      relative_humidity_raster_t_i >= 38.14 & relative_humidity_raster_t_i <= 55.87 & 
                      shrubs_raster_t_i >= 1 & shrubs_raster_t_i <= 328.90 &
                      lulc_raster_t_i != 1 & lulc_raster_t_i != 3 & 
                      precipitation_raster_t_i >= 0 & precipitation_raster_t_i <= 876076)
                 |(species_data_raster_t_i==3)] = 3 
      
      ### @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
      in_re_new1[(in_re_new1==6.5 & 
                    build_up_raster_t_i >= 0 & build_up_raster_t_i <= 84.345 & 
                    temp_range_raster_t_i >= 20.300 & temp_range_raster_t_i <= 35.00 & 
                    elevation_raster_t_i >= 0 & elevation_raster_t_i <= 2056.000 & 
                    irrigation_raster_t_i >= 0 & irrigation_raster_t_i <= 468.11 & 
                    ndvi_raster_t_i >= -0.0845 & ndvi_raster_t_i <= 0.784 & 
                    relative_humidity_raster_t_i >= 44.850 & relative_humidity_raster_t_i <= 84.610 & 
                    lulc_raster_t_i != 1 & lulc_raster_t_i != 3 & 
                    precipitation_raster_t_i >= 61 & precipitation_raster_t_i <= 2138871)|
                   (species_data_raster_t_i==1 & species_data_raster_t_i==4)] = 7
      
      in_re_new1[(in_re_new1==6.5 & 
                    build_up_raster_t_i >= 0.000 & build_up_raster_t_i <= 206.940 & 
                    temp_range_raster_t_i >= 18.233 & temp_range_raster_t_i <=  35.00 & 
                    elevation_raster_t_i >= 0 & elevation_raster_t_i <= 2804.0 & 
                    irrigation_raster_t_i >= 0 & irrigation_raster_t_i <= 1467.2 & 
                    ndvi_raster_t_i >= -0.08956 & ndvi_raster_t_i <= 0.851 & 
                    relative_humidity_raster_t_i >= 22.47 & relative_humidity_raster_t_i <= 89.730 &
                    lulc_raster_t_i != 1 & lulc_raster_t_i != 3 & 
                    precipitation_raster_t_i >= 0 & precipitation_raster_t_i <= 2138871)|
                   (in_re_new1==6.5 & 
                      build_up_raster_t_i >= 0.000 & build_up_raster_t_i <= 206.940 & 
                      temp_range_raster_t_i > 35 & temp_range_raster_t_i <=  37.74 & 
                      elevation_raster_t_i >= 2 & elevation_raster_t_i <= 969.0 & 
                      irrigation_raster_t_i >= 0 & irrigation_raster_t_i <= 364.78 & 
                      ndvi_raster_t_i >= 0.07714 & ndvi_raster_t_i <= 0.5964 & 
                      relative_humidity_raster_t_i >= 22.47 & relative_humidity_raster_t_i <= 63.02 & 
                      lulc_raster_t_i != 1 & lulc_raster_t_i != 3 & 
                      precipitation_raster_t_i >= 0 & precipitation_raster_t_i <= 2138871)
                 |(species_data_raster_t_i==1)] = 1 
      
      in_re_new1[(in_re_new1==6.5 & 
                    build_up_raster_t_i >= 0 & build_up_raster_t_i <= 84.345 & 
                    temp_range_raster_t_i >= 20.30 & temp_range_raster_t_i <= 35 & 
                    elevation_raster_t_i >= 0.0 & elevation_raster_t_i <= 2056.0 & 
                    irrigation_raster_t_i >= 0.0 & irrigation_raster_t_i <= 468.11 & 
                    ndvi_raster_t_i >= -0.07879 & ndvi_raster_t_i <= 0.784 & 
                    relative_humidity_raster_t_i >= 44.85 & relative_humidity_raster_t_i <= 84.61 & 
                    lulc_raster_t_i != 1 & lulc_raster_t_i != 3 & 
                    precipitation_raster_t_i >= 61 & precipitation_raster_t_i <= 2138871)
                 |(in_re_new1==6.5 & 
                     build_up_raster_t_i >= 0 & build_up_raster_t_i <= 84.345 & 
                     temp_range_raster_t_i > 35 & temp_range_raster_t_i <= 37.29 & 
                     elevation_raster_t_i >= 0.0 & elevation_raster_t_i <= 468.11 & 
                     irrigation_raster_t_i >= 0.0 & irrigation_raster_t_i <= 485.37 & 
                     ndvi_raster_t_i >= -0.07879 & ndvi_raster_t_i <= 0.784 & 
                     relative_humidity_raster_t_i >= 44.85 & relative_humidity_raster_t_i <= 53.16 & 
                     lulc_raster_t_i != 1 & lulc_raster_t_i != 3 & 
                     precipitation_raster_t_i >= 61 & precipitation_raster_t_i <= 2138871)
                 |(species_data_raster_t_i==4)] = 4 
      
      ### @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
      in_re_new1[(in_re_new1==7.5 & 
                    build_up_raster_t_i >= 0 & build_up_raster_t_i <=  137.535 & 
                    temp_range_raster_t_i >= 24.03 & temp_range_raster_t_i <= 35.00 & 
                    elevation_raster_t_i >= 0 & elevation_raster_t_i <= 2031.000 & 
                    irrigation_raster_t_i >= 0 & irrigation_raster_t_i <= 485.320 & 
                    ndvi_raster_t_i >= -0.079 & ndvi_raster_t_i <= 0.793 & 
                    relative_humidity_raster_t_i >= 40.500 & relative_humidity_raster_t_i <= 85.340 & 
                    lulc_raster_t_i != 1 & lulc_raster_t_i != 3 & 
                    precipitation_raster_t_i >= 0 & precipitation_raster_t_i <= 876076)
                 |(species_data_raster_t_i==2 & species_data_raster_t_i==3)] = 8
      
      in_re_new1[(in_re_new1==7.5 &
                    build_up_raster_t_i >= 0 & build_up_raster_t_i <= 202.800 & 
                    temp_range_raster_t_i >= 21.29 & temp_range_raster_t_i <= 35.00 & 
                    elevation_raster_t_i >= 0 & elevation_raster_t_i <= 2527.000 & 
                    irrigation_raster_t_i >= 0 & irrigation_raster_t_i <= 1467.21 & 
                    ndvi_raster_t_i >= -0.07879 & ndvi_raster_t_i <= 0.851 & 
                    relative_humidity_raster_t_i >= 40.50 & relative_humidity_raster_t_i <= 88.61 & 
                    lulc_raster_t_i != 1 & lulc_raster_t_i != 3 & 
                    precipitation_raster_t_i >= 0 & precipitation_raster_t_i <= 2138871)|
                   (in_re_new1==7.5 &
                      build_up_raster_t_i >= 0 & build_up_raster_t_i <= 130.740 & 
                      temp_range_raster_t_i >= 35 & temp_range_raster_t_i <= 37.12 & 
                      elevation_raster_t_i >= 9.0 & elevation_raster_t_i <= 747.000 & 
                      irrigation_raster_t_i >= 0 & irrigation_raster_t_i <= 225.02 & 
                      ndvi_raster_t_i >= -0.07879 & ndvi_raster_t_i <= 0.851 & 
                      relative_humidity_raster_t_i >= 40.50 & relative_humidity_raster_t_i <= 55.87 & 
                      lulc_raster_t_i != 1 & lulc_raster_t_i != 3 & 
                      precipitation_raster_t_i >= 0 & precipitation_raster_t_i <= 2138871)
                 |(species_data_raster_t_i==2)] = 2 
      
      in_re_new1[(in_re_new1==7.5 & 
                    build_up_raster_t_i >= 0 & build_up_raster_t_i <= 137.535 & 
                    temp_range_raster_t_i >= 24.03 & temp_range_raster_t_i <= 35 & 
                    elevation_raster_t_i >= 0 & elevation_raster_t_i <= 2031.0 & 
                    irrigation_raster_t_i >= 0 & irrigation_raster_t_i <= 485.37 & 
                    ndvi_raster_t_i >= -0.1335 & ndvi_raster_t_i <= 0.793 &
                    relative_humidity_raster_t_i >= 38.14 & relative_humidity_raster_t_i <= 85.34 & 
                    lulc_raster_t_i != 1 & lulc_raster_t_i != 3 & 
                    precipitation_raster_t_i >= 0 & precipitation_raster_t_i <= 876076)|
                   (in_re_new1==7.5 & 
                      build_up_raster_t_i >= 0 & build_up_raster_t_i <= 137.535 & 
                      temp_range_raster_t_i > 35 & temp_range_raster_t_i <= 38.06 & 
                      elevation_raster_t_i >= 0 & elevation_raster_t_i <= 747.0 & 
                      irrigation_raster_t_i >= 0 & irrigation_raster_t_i <= 485.37 & 
                      ndvi_raster_t_i >= -0.1335 & ndvi_raster_t_i <= 0.793 & 
                      relative_humidity_raster_t_i >= 38.14 & relative_humidity_raster_t_i <= 55.87 & 
                      shrubs_raster_t_i >= 1 & shrubs_raster_t_i <= 328.90 &
                      lulc_raster_t_i != 1 & lulc_raster_t_i != 3 & 
                      precipitation_raster_t_i >= 0 & precipitation_raster_t_i <= 876076)
                 |(species_data_raster_t_i==3)] = 3 
      
      ### @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
      in_re_new1[(in_re_new1==8.5 & 
                    build_up_raster_t_i >= 0 & build_up_raster_t_i <= 84.345 & 
                    temp_range_raster_t_i >= 21.290 & temp_range_raster_t_i <= 35.00 & 
                    elevation_raster_t_i >= 0 & elevation_raster_t_i <= 2056.000 & 
                    irrigation_raster_t_i >= 0 & irrigation_raster_t_i <= 468.11 & 
                    ndvi_raster_t_i >= -0.079 & ndvi_raster_t_i <= 0.784 & 
                    relative_humidity_raster_t_i >= 44.850 & relative_humidity_raster_t_i <= 84.610 & 
                    lulc_raster_t_i != 1 & lulc_raster_t_i != 3 & 
                    precipitation_raster_t_i >= 61 & precipitation_raster_t_i <= 2138871)
                 |(species_data_raster_t_i==2 & species_data_raster_t_i==4)] = 9
      
      in_re_new1[(in_re_new1==8.5 &
                    build_up_raster_t_i >= 0 & build_up_raster_t_i <= 202.800 & 
                    temp_range_raster_t_i >= 21.29 & temp_range_raster_t_i <= 35.00 & 
                    elevation_raster_t_i >= 0 & elevation_raster_t_i <= 2527.000 & 
                    irrigation_raster_t_i >= 0 & irrigation_raster_t_i <= 1467.21 & 
                    ndvi_raster_t_i >= -0.07879 & ndvi_raster_t_i <= 0.851 & 
                    relative_humidity_raster_t_i >= 40.50 & relative_humidity_raster_t_i <= 88.61 & 
                    lulc_raster_t_i != 1 & lulc_raster_t_i != 3 & 
                    precipitation_raster_t_i >= 0 & precipitation_raster_t_i <= 2138871)|
                   (in_re_new1==8.5 &
                      build_up_raster_t_i >= 0 & build_up_raster_t_i <= 130.740 & 
                      temp_range_raster_t_i >= 35 & temp_range_raster_t_i <= 37.12 & 
                      elevation_raster_t_i >= 9.0 & elevation_raster_t_i <= 747.000 & 
                      irrigation_raster_t_i >= 0 & irrigation_raster_t_i <= 225.02 & 
                      ndvi_raster_t_i >= -0.07879 & ndvi_raster_t_i <= 0.851 & 
                      relative_humidity_raster_t_i >= 40.50 & relative_humidity_raster_t_i <= 55.87 & 
                      lulc_raster_t_i != 1 & lulc_raster_t_i != 3 & 
                      precipitation_raster_t_i >= 0 & precipitation_raster_t_i <= 2138871)
                 |(species_data_raster_t_i==2)] = 2 
      
      in_re_new1[(in_re_new1==8.5 & 
                    build_up_raster_t_i >= 0 & build_up_raster_t_i <= 84.345 & 
                    temp_range_raster_t_i >= 20.30 & temp_range_raster_t_i <= 35 & 
                    elevation_raster_t_i >= 0.0 & elevation_raster_t_i <= 2056.0 & 
                    irrigation_raster_t_i >= 0.0 & irrigation_raster_t_i <= 468.11 & 
                    ndvi_raster_t_i >= -0.07879 & ndvi_raster_t_i <= 0.784 & 
                    relative_humidity_raster_t_i >= 44.85 & relative_humidity_raster_t_i <= 84.61 & 
                    lulc_raster_t_i != 1 & lulc_raster_t_i != 3 & 
                    precipitation_raster_t_i >= 61 & precipitation_raster_t_i <= 2138871)
                 |(in_re_new1==8.5 & 
                     build_up_raster_t_i >= 0 & build_up_raster_t_i <= 84.345 & 
                     temp_range_raster_t_i > 35 & temp_range_raster_t_i <= 37.29 & 
                     elevation_raster_t_i >= 0.0 & elevation_raster_t_i <= 468.11 & 
                     irrigation_raster_t_i >= 0.0 & irrigation_raster_t_i <= 485.37 & 
                     ndvi_raster_t_i >= -0.07879 & ndvi_raster_t_i <= 0.784 & 
                     relative_humidity_raster_t_i >= 44.85 & relative_humidity_raster_t_i <= 53.16 & 
                     lulc_raster_t_i != 1 & lulc_raster_t_i != 3 & 
                     precipitation_raster_t_i >= 61 & precipitation_raster_t_i <= 2138871)
                 |(species_data_raster_t_i==4)] = 4 
      
      ### @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
      in_re_new1[(in_re_new1==9.5 & 
                    build_up_raster_t_i >= 0 & build_up_raster_t_i <= 84.345 & 
                    temp_range_raster_t_i >= 24.03 & temp_range_raster_t_i <= 35.00 & 
                    elevation_raster_t_i >= 0 & elevation_raster_t_i <= 2031.000 & 
                    irrigation_raster_t_i >= 0 & irrigation_raster_t_i <= 468.11 & 
                    ndvi_raster_t_i >= -0.079 & ndvi_raster_t_i <= 0.784 & 
                    relative_humidity_raster_t_i >= 44.850 & relative_humidity_raster_t_i <= 84.610 & 
                    lulc_raster_t_i != 1 & lulc_raster_t_i != 3 & 
                    precipitation_raster_t_i >= 61 & precipitation_raster_t_i <= 876076)
                 |(species_data_raster_t_i==3 & species_data_raster_t_i==4)] = 10
      
      in_re_new1[(in_re_new1==9.5 & 
                    build_up_raster_t_i >= 0 & build_up_raster_t_i <= 137.535 & 
                    temp_range_raster_t_i >= 24.03 & temp_range_raster_t_i <= 35 & 
                    elevation_raster_t_i >= 0 & elevation_raster_t_i <= 2031.0 & 
                    irrigation_raster_t_i >= 0 & irrigation_raster_t_i <= 485.37 & 
                    ndvi_raster_t_i >= -0.1335 & ndvi_raster_t_i <= 0.793 &
                    relative_humidity_raster_t_i >= 38.14 & relative_humidity_raster_t_i <= 85.34 & 
                    lulc_raster_t_i != 1 & lulc_raster_t_i != 3 & 
                    precipitation_raster_t_i >= 0 & precipitation_raster_t_i <= 876076)|
                   (in_re_new1==9.5 & 
                      build_up_raster_t_i >= 0 & build_up_raster_t_i <= 137.535 & 
                      temp_range_raster_t_i > 35 & temp_range_raster_t_i <= 38.06 & 
                      elevation_raster_t_i >= 0 & elevation_raster_t_i <= 747.0 & 
                      irrigation_raster_t_i >= 0 & irrigation_raster_t_i <= 485.37 & 
                      ndvi_raster_t_i >= -0.1335 & ndvi_raster_t_i <= 0.793 & 
                      relative_humidity_raster_t_i >= 38.14 & relative_humidity_raster_t_i <= 55.87 & 
                      shrubs_raster_t_i >= 1 & shrubs_raster_t_i <= 328.90 &
                      lulc_raster_t_i != 1 & lulc_raster_t_i != 3 & 
                      precipitation_raster_t_i >= 0 & precipitation_raster_t_i <= 876076)
                 |(species_data_raster_t_i==3)] = 3 
      
      in_re_new1[(in_re_new1==9.5 & 
                    build_up_raster_t_i >= 0 & build_up_raster_t_i <= 84.345 & 
                    temp_range_raster_t_i >= 20.30 & temp_range_raster_t_i <= 35 & 
                    elevation_raster_t_i >= 0.0 & elevation_raster_t_i <= 2056.0 & 
                    irrigation_raster_t_i >= 0.0 & irrigation_raster_t_i <= 468.11 & 
                    ndvi_raster_t_i >= -0.07879 & ndvi_raster_t_i <= 0.784 & 
                    relative_humidity_raster_t_i >= 44.85 & relative_humidity_raster_t_i <= 84.61 & 
                    lulc_raster_t_i != 1 & lulc_raster_t_i != 3 & 
                    precipitation_raster_t_i >= 61 & precipitation_raster_t_i <= 2138871)
                 |(in_re_new1==9.5 & 
                     build_up_raster_t_i >= 0 & build_up_raster_t_i <= 84.345 & 
                     temp_range_raster_t_i > 35 & temp_range_raster_t_i <= 37.29 & 
                     elevation_raster_t_i >= 0.0 & elevation_raster_t_i <= 468.11 & 
                     irrigation_raster_t_i >= 0.0 & irrigation_raster_t_i <= 485.37 & 
                     ndvi_raster_t_i >= -0.07879 & ndvi_raster_t_i <= 0.784 & 
                     relative_humidity_raster_t_i >= 44.85 & relative_humidity_raster_t_i <= 53.16 & 
                     lulc_raster_t_i != 1 & lulc_raster_t_i != 3 & 
                     precipitation_raster_t_i >= 61 & precipitation_raster_t_i <= 2138871)
                 |(species_data_raster_t_i==4)] = 4 
      
      ### @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
      in_re_new1[(in_re_new1==10.5 & 
                    build_up_raster_t_i >= 0 & build_up_raster_t_i <= 137.535 & 
                    temp_range_raster_t_i >= 24.03 & temp_range_raster_t_i <= 35.00 & 
                    elevation_raster_t_i >= 0 & elevation_raster_t_i <= 2031.000 & 
                    irrigation_raster_t_i >= 0 & irrigation_raster_t_i <= 485.37 & 
                    ndvi_raster_t_i >= -0.079 & ndvi_raster_t_i <= 0.793 & 
                    relative_humidity_raster_t_i >= 40.500 & relative_humidity_raster_t_i <= 85.340 & 
                    lulc_raster_t_i != 1 & lulc_raster_t_i != 3 & 
                    precipitation_raster_t_i >= 0 & precipitation_raster_t_i <= 876076)
                 |(species_data_raster_t_i==1 & species_data_raster_t_i==2 & species_data_raster_t_i==3)] = 11
      
      ### @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
      in_re_new1[(in_re_new1==11.5 & 
                    build_up_raster_t_i >= 0 & build_up_raster_t_i <= 84.345 & 
                    temp_range_raster_t_i >= 19.941 & temp_range_raster_t_i <= 35.00 & 
                    elevation_raster_t_i >= 0 & elevation_raster_t_i <= 2056.000 & 
                    irrigation_raster_t_i >= 0 & irrigation_raster_t_i <= 468.11 & 
                    ndvi_raster_t_i >= -0.079 & ndvi_raster_t_i <= 0.784 & 
                    relative_humidity_raster_t_i >= 44.850 & relative_humidity_raster_t_i <= 84.610 & 
                    lulc_raster_t_i != 1 & lulc_raster_t_i != 3 & 
                    precipitation_raster_t_i >= 61 & precipitation_raster_t_i <= 2138871)
                 |(species_data_raster_t_i==1 & species_data_raster_t_i==2 & species_data_raster_t_i==4)] = 12
      
      ### @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
      in_re_new1[(in_re_new1==12.5 & 
                    build_up_raster_t_i >= 0 & build_up_raster_t_i <= 84.345 & 
                    temp_range_raster_t_i >= 24.03 & temp_range_raster_t_i <= 35.00 & 
                    elevation_raster_t_i >= 0 & elevation_raster_t_i <= 2031.000 & 
                    irrigation_raster_t_i >= 0 & irrigation_raster_t_i <= 468.110 & 
                    ndvi_raster_t_i >= -0.079 & ndvi_raster_t_i <= 0.784 & 
                    relative_humidity_raster_t_i >= 44.850 & relative_humidity_raster_t_i <= 84.610 & 
                    lulc_raster_t_i != 1 & lulc_raster_t_i != 3 & 
                    precipitation_raster_t_i >= 61 & precipitation_raster_t_i <= 2138871)
                 |(species_data_raster_t_i==1 & species_data_raster_t_i==3 & species_data_raster_t_i==4)] = 13
      
      ### @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
      in_re_new1[(in_re_new1==13.5 & 
                    build_up_raster_t_i >= 0 & build_up_raster_t_i <= 84.345 & 
                    temp_range_raster_t_i >= 24.03 & temp_range_raster_t_i <= 35.00 & 
                    elevation_raster_t_i >= 0 & elevation_raster_t_i <= 2031.000 & 
                    irrigation_raster_t_i >= 0 & irrigation_raster_t_i <= 468.110 & 
                    ndvi_raster_t_i >= -0.079 & ndvi_raster_t_i <= 0.784 & 
                    relative_humidity_raster_t_i >= 44.850 & relative_humidity_raster_t_i <= 84.610 & 
                    lulc_raster_t_i != 1 & lulc_raster_t_i != 3 & 
                    precipitation_raster_t_i >= 61 & precipitation_raster_t_i <= 876076)
                 |(species_data_raster_t_i==2 & species_data_raster_t_i==3 & species_data_raster_t_i==4)] = 14
      
      ### @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
      in_re_new1[(in_re_new1==14.5 & 
                    build_up_raster_t_i >= 0 & build_up_raster_t_i <= 84.345 & 
                    temp_range_raster_t_i >= 24.03 & temp_range_raster_t_i <= 35.00 & 
                    elevation_raster_t_i >= 0 & elevation_raster_t_i <= 2031.000 & 
                    irrigation_raster_t_i >= 0 & irrigation_raster_t_i <= 468.110 & 
                    ndvi_raster_t_i >= -0.079 & ndvi_raster_t_i <= 0.784 & 
                    relative_humidity_raster_t_i >= 44.850 & relative_humidity_raster_t_i <= 84.610 & 
                    lulc_raster_t_i != 1 & lulc_raster_t_i != 3 & 
                    precipitation_raster_t_i >= 61 & precipitation_raster_t_i <= 2138871)
                 |(species_data_raster_t_i==1 & species_data_raster_t_i==2 & species_data_raster_t_i==3 & species_data_raster_t_i==4)] = 15
      
      ############@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ 
      # intersecting Neighbourhood if there is any cell that was identified as neighbouring cell, but not has not changed its state (value) is assigned back to zero below
      in_re_new1[in_re_new1==0.5] = 0
      in_re_new1[in_re_new1==1.5] = 0
      in_re_new1[in_re_new1==2.5] = 0
      in_re_new1[in_re_new1==3.5] = 0
      in_re_new1[in_re_new1==4.5] = 0
      in_re_new1[in_re_new1==5.5] = 0
      in_re_new1[in_re_new1==6.5] = 0
      in_re_new1[in_re_new1==7.5] = 0
      in_re_new1[in_re_new1==8.5] = 0
      in_re_new1[in_re_new1==9.5] = 0
      in_re_new1[in_re_new1==10.5] = 0
      in_re_new1[in_re_new1==11.5] = 0
      in_re_new1[in_re_new1==12.5] = 0
      in_re_new1[in_re_new1==13.5] = 0
      in_re_new1[in_re_new1==14.5] = 0
      in_re_new1[in_re_new1==15.5] = 0
      
      #updating the raster
      in_re = in_re_new1 # here we create a new object assigning the updated raster following application of transition rules. This object is used for updating (based on transition rules) in the next time-step (iteration i+1) ( see line 410 this object will be recalled at that time every time new iteration commences)
      in_re_new = in_re_new1 # here the updated raster is the assigned another object name (differentiate from object name in line 1082) then used in the following lines, for visualized and output at iteration i (timestep i)
      
      # plotting the raster
      africa_raster_trans_cropped_new <- crop(in_re_new, extent(africa_proj)) # just for cropping the extents to fit in african projection
      africa_raster_trans_masked_new <- mask(africa_raster_trans_cropped_new, africa_proj) 
      
      my_colors <- c("whitesmoke", "cyan", "deeppink", "deepskyblue", "brown", "green", 
                     "darkolivegreen1", "darksalmon", "blue", "purple", "red", 
                     "seagreen", "tan4", "darkorange", "violet", "olivedrab") # We define these colours to distinctly identify each cell based on values assigned
      
      cuts=c(0, 0.5, 1:15) # the number of colours and defined in the legend
      plot(africa_raster_trans_masked_new, main= paste(" ", format(temp_range_t_i$datetime[i], "%B %Y")), breaks=cuts, col = my_colors) #plot which should be visualized as output in the platform . "%B %Y" is the month and the year of a particular iteratation
      
      map_name <- format(temp_range_t_i$datetime[i], "%B %Y")
      output_file <- paste("D:/modelling niche overlaps/rasters plots species/plots/",map_name,"_Month", j,".tiff") # this involve writing the output in a folder where the outputs are stored as images if moore extended neigbourhood
      tiff(filename = output_file)
      plot(africa_raster_trans_masked_new, main= paste(" ", format(temp_range_t_i$datetime[i], "%B %Y")),
           breaks=cuts, col = my_colors)
      dev.off()
      # # Read the TIFF file as a raster
      map_name <- format(temp_range_t_i$datetime[i], "%B %Y")
      output_raster <- paste("D:/modelling niche overlaps/rasters plots species/rasters/",map_name,"_Month", j,".tif") # this involve naming the writing the output in a folder whose path is specified. the outputs are stored as rasters
      writeRaster(africa_raster_trans_masked_new, filename = output_raster, format = "Gtiff", overwrite = TRUE) 
    }
  }
}








