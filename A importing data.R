
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
africa <- readOGR("D:/fuzzy CA/Africa Shapefile/Africa.shp")#,

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
build_up_1985_2030 <- read.table("D:/stephensi modeling/data/variables processed yearly/build_up_1985_2030_df.txt", header = TRUE, sep = "\t", stringsAsFactors = FALSE)
build_up_1985_2030_df <- data.frame(build_up_1985_2030)
build_up_1985_2030_df_ftd1 <- subset(build_up_1985_2030_df)
build_up_1985_2030_df_ftd2 <- build_up_1985_2030_df_ftd1[, c("datetime", setdiff(names(build_up_1985_2030_df_ftd1), "datetime"))]; build_up_1985_2030_df_ftd <- build_up_1985_2030_df_ftd2
build_up_1985_2030_df_ftd$datetime <- as.Date(build_up_1985_2030_df_ftd$datetime)

temp_range_1985_2030 <- read.table("D:/stephensi modeling/data/variables processed yearly/max_temp_1985_2022_df.txt", header = TRUE, sep = "\t", stringsAsFactors = FALSE)
temp_range_1985_2030_df <- data.frame(temp_range_1985_2030)
temp_range_1985_2030_df_ftd1 <- subset(temp_range_1985_2030_df)
temp_range_1985_2030_df_ftd2 <- temp_range_1985_2030_df_ftd1[, c("datetime", setdiff(names(temp_range_1985_2030_df_ftd1), "datetime"))]; temp_range_1985_2030_df_ftd <- temp_range_1985_2030_df_ftd2
temp_range_1985_2030_df_ftd$datetime <- as.Date(temp_range_1985_2030_df_ftd$datetime)

elevation_1985_2030 <- read.table("D:/stephensi modeling/data/variables processed yearly/elevation_1985_2030_df.txt", header = TRUE, sep = "\t", stringsAsFactors = FALSE)
elevation_1985_2030_df <- data.frame(elevation_1985_2030)
elevation_1985_2030_df_ftd1 <- subset(elevation_1985_2030_df)#, datetime > "2005-12-31")
elevation_1985_2030_df_ftd2 <- elevation_1985_2030_df_ftd1[, c("datetime", setdiff(names(elevation_1985_2030_df_ftd1), "datetime"))]; elevation_1985_2030_df_ftd <- elevation_1985_2030_df_ftd2
elevation_1985_2030_df_ftd$datetime <- as.Date(elevation_1985_2030_df_ftd$datetime)

irrigation_1985_2030 <- read.table("D:/stephensi modeling/data/variables processed yearly/irrigation_1985_2030_df.txt", header = TRUE, sep = "\t", stringsAsFactors = FALSE)
irrigation_1985_2030_df <- data.frame(irrigation_1985_2030)
irrigation_1985_2030_df_ftd1 <- subset(irrigation_1985_2030_df)
irrigation_1985_2030_df_ftd2 <- irrigation_1985_2030_df_ftd1[, c("datetime", setdiff(names(irrigation_1985_2030_df_ftd1), "datetime"))]; irrigation_1985_2030_df_ftd <- irrigation_1985_2030_df_ftd2
irrigation_1985_2030_df_ftd$datetime <- as.Date(irrigation_1985_2030_df_ftd$datetime)

ndvi_1985_2030 <- read.table("D:/stephensi modeling/data/variables processed yearly/ndvi_1985_2030_df.txt", header = TRUE, sep = "\t", stringsAsFactors = FALSE)
ndvi_1985_2030_df <- data.frame(ndvi_1985_2030)
ndvi_1985_2030_df_ftd1 <- subset(ndvi_1985_2030_df)
ndvi_1985_2030_df_ftd2 <- ndvi_1985_2030_df_ftd1[, c("datetime", setdiff(names(ndvi_1985_2030_df_ftd1), "datetime"))]; ndvi_1985_2030_df_ftd <- ndvi_1985_2030_df_ftd2
ndvi_1985_2030_df_ftd$datetime <- as.Date(ndvi_1985_2030_df_ftd$datetime)

relative_humidity_1985_2030 <- read.table("D:/stephensi modeling/data/variables processed yearly/rel_hum_1985_2030_df.txt", header = TRUE, sep = "\t", stringsAsFactors = FALSE)
relative_humidity_1985_2030_df <- data.frame(relative_humidity_1985_2030)
relative_humidity_1985_2030_df_ftd1 <- subset(relative_humidity_1985_2030_df)
relative_humidity_1985_2030_df_ftd2 <- relative_humidity_1985_2030_df_ftd1[, c("datetime", setdiff(names(relative_humidity_1985_2030_df_ftd1), "datetime"))]; relative_humidity_1985_2030_df_ftd <- relative_humidity_1985_2030_df_ftd2
relative_humidity_1985_2030_df_ftd$datetime <- as.Date(relative_humidity_1985_2030_df_ftd2$datetime)

lulc_1985_2030 <- read.table("D:/stephensi modeling/data/variables processed yearly/MODIS_lulc_1985_2021_df.txt", header = TRUE, sep = "\t", stringsAsFactors = FALSE)
lulc_1985_2030_df <- data.frame(lulc_1985_2030)
lulc_1985_2030_df_ftd1 <- subset(lulc_1985_2030_df)
lulc_1985_2030_df_ftd2 <- lulc_1985_2030_df_ftd1[, c("datetime", setdiff(names(lulc_1985_2030_df_ftd1), "datetime"))]; lulc_1985_2030_df_ftd <- lulc_1985_2030_df_ftd2 
lulc_1985_2030_df_ftd$datetime <- as.Date(lulc_1985_2030_df_ftd2$datetime)

precipitation_1985_2030 <- read.table("D:/stephensi modeling/data/variables processed yearly/precipitation_1985_2021_df.txt", header = TRUE, sep = "\t", stringsAsFactors = FALSE)
precipitation_1985_2030_df <- data.frame(precipitation_1985_2030)
precipitation_1985_2030_df_ftd1 <- subset(precipitation_1985_2030_df)
precipitation_1985_2030_df_ftd2 <- precipitation_1985_2030_df_ftd1[, c("datetime", setdiff(names(precipitation_1985_2030_df_ftd1), "datetime"))]; precipitation_1985_2030_df_ftd <- precipitation_1985_2030_df_ftd2
precipitation_1985_2030_df_ftd2$datetime <- as.Date(precipitation_1985_2030_df_ftd2$datetime)

