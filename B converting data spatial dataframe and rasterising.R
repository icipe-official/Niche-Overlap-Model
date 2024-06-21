

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


