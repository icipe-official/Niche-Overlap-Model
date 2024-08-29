
library(readxl)

species_data <- read_excel("C:\\Users\\eibrahim\\OneDrive - International Centre of Insect Physiology and Ecology (ICIPE)\\PHD thesis\\Vector_Atlas_project\\manuscipts_and_review_papers\\Manuscripts\\Obj_5_Niche_overlap_modeling\\modelling niche overlaps\\dataset\\species occurence data EDA.xlsx")

# filtering the data
species_data$species.presence1<- ifelse(species_data$species %in% c('GAMBIAE COMPLEX','gambiae', 'gambiae (S)','coluzzii (gambiae M)', 
                                                                    'coluzzii (gambiae M)', 'gambiae (S/M)', 'Anopheles gambiae'), 1,
                                        ifelse(species_data$species %in% c('Anopheles funestus', 'funestus', 'FUNESTUS COMPLEX', 'Funestus Subgroup'), 2,
                                               ifelse(species_data$species %in% c('pharoensis', 'Anopheles pharoensis'), 3,
                                                      ifelse(species_data$species %in% c('coustani', 'COUSTANI COMPLEX'), 4, NA))))

species_data <- species_data[complete.cases(species_data$species.presence1),]
species_data_df <- data.frame(species_data)

#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@subseting data @@@@@@@@@@@@@@@@@@
selected_variables <- c('shrubs', 'build_up', 'temp_range',  'elevation', 'irrigation', 'ndvi',                              
                        'relative_humidity', 'Climate_Moisture_Index', 'Cloud_Area_Fraction', 'Near_Surface_Wind_Speed',               
                        'Potential_Evapotranspiration', 'Precipitation',  'Surface_Downwelling_Shortwave_Radiation',
                        'species.presence1')

new_names <- c(
  'Shrubs',  'Built up', 'Temperature range', 'Elevation','Irrigation',  'NDVI',  'Relative_humidity',  
  'Climate moisture index', 'Cloud area fraction',  'Near surface wind Speed', 'Potential evapotranspiration',  
  'Precipitation',  'solar radiation','species presence')

# Subset data and rename columns
subset_df <- species_data_df[selected_variables]; names(subset_df) <- new_names


#####  Correlation analysis
library(metan)
correlation_analysis <- corr_coef(subset_df, method = "spearman")

plot(correlation_analysis, cex = 10.5,
     cex = 1.5, cex.axis = 1.5, 
     cex.lab = 1.3, cex.sub = 1.3)
par(cex.axis = 10.5)

############
# create subset of data;  
species_data_eda_gamb <- species_data_df[species_data_df$species.presence1 == "1", ]
species_data_eda_funes <- species_data_df[species_data_df$species.presence1 == "2", ]
species_data_eda_phar <- species_data_df[species_data_df$species.presence1 == "3", ]
species_data_eda_coust <- species_data_df[species_data_df$species.presence1 == "4", ]

#####  Cluster analysis

#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@subseting data @@@@@@@@@@@@@@@@@@
selected_variables1 <- c(                            
  'shrubs',  'build_up',  'temp_range',  'elevation', 'irrigation',                             
  'ndvi', 'relative_humidity', 'Climate_Moisture_Index', 'Cloud_Area_Fraction',               
  'Near_Surface_Wind_Speed', 'Potential_Evapotranspiration',  'Precipitation',                         
  'Surface_Downwelling_Shortwave_Radiation')

new_names1 <- c( 
  'Shrubs',  'Built up', 'Temperature range',   'Elevation',    'Irrigation',  'NDVI',   'Relative_humidity',  
  'Climate moisture index', 'Cloud area fraction',  'Near surface wind Speed', 'Potential evapotranspiration',  
  'Precipitation',  'solar radiation')

# Subset data and rename columns
# Euclidean distance
dist <- dist(species_data_df, diag=TRUE)

subset_df1 <- species_data_df[selected_variables1]
names(subset_df1) <- new_names1

# Calculate the correlation matrix
cor_matrix <- cor(subset_df1,  method = "spearman")
rownames(cor_matrix) <- colnames(subset_df1)
hc <- hclust(dist(cor_matrix))

# Create a dendrogram with variable names
plot(hc, main = "Dendrogram of Variables", xlab = "Variables", ylab = "Height", labels = colnames(subset_df1), 
     cex = 1.5,
     cex.axis = 1.5,
     cex.lab = 1.3,
     cex.sub = 1.3)


#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ descriptive statistics @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# create subset of data;  

summary(species_data_eda_gamb$ndvi); summary(species_data_eda_funes$ndvi); summary(species_data_eda_phar$ndvi); summary(species_data_eda_coust$ndvi)
summary(species_data_eda_gamb$Cloud_Area_Fraction); summary(species_data_eda_funes$Cloud_Area_Fraction); summary(species_data_eda_phar$Cloud_Area_Fraction); summary(species_data_eda_coust$Cloud_Area_Fraction)
summary(species_data_eda_gamb$Precipitation); summary(species_data_eda_funes$Precipitation); summary(species_data_eda_phar$Precipitation); summary(species_data_eda_coust$Precipitation)
summary(species_data_eda_gamb$irrigation); summary(species_data_eda_funes$irrigation); summary(species_data_eda_phar$irrigation); summary(species_data_eda_coust$irrigation)
summary(species_data_eda_gamb$elevation); summary(species_data_eda_funes$elevation); summary(species_data_eda_phar$elevation); summary(species_data_eda_coust$elevation)
summary(species_data_eda_gamb$shrubs); summary(species_data_eda_funes$shrubs); summary(species_data_eda_phar$shrubs); summary(species_data_eda_coust$shrubs)
summary(species_data_eda_gamb$temp_range); summary(species_data_eda_funes$temp_range); summary(species_data_eda_phar$temp_range); summary(species_data_eda_coust$temp_range)
summary(species_data_eda_gamb$Near_Surface_Relative_Humidity); summary(species_data_eda_funes$Near_Surface_Relative_Humidity); summary(species_data_eda_phar$Near_Surface_Relative_Humidity); summary(species_data_eda_coust$Near_Surface_Relative_Humidity)
summary(species_data_eda_gamb$build_up); summary(species_data_eda_funes$build_up); summary(species_data_eda_phar$build_up); summary(species_data_eda_coust$build_up)

