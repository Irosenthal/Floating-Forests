library(readr)
library(dplyr)
#######################
#data downloaded from https://landsat.usgs.gov/download-entire-collection-metadata on 1/18/17
#Use precollection
#########################

#clean up old metadata list
metalist <- read_csv("./Floating-Forests/data/landsat_scenes_filtered_metadata.csv")
metalist <- select(metalist, c(sceneID, sensor, acquisitionDate))
write_csv(metalist, "./Floating-Forests/data/FF_scene_list.csv")
#######################

metalist <- read_csv("./Floating-Forests/data/FF_scene_list.csv")



#start with this one, it is a reasonable size
#read in data
LSETM_raw <- read_csv("./scenes/metadata/LANDSAT_ETM.csv/LANDSAT_ETM.csv")
#remove unnecessary columns
LSETM_clean <-select(LSETM_raw,
                     sceneID, 
                     sensor, 
                     acquisitionDate, 
                     upperLeftCornerLatitude, 
                     upperLeftCornerLongitude, 
                     upperRightCornerLatitude,
                     upperRightCornerLongitude,
                     lowerLeftCornerLatitude,
                     lowerLeftCornerLongitude,
                     lowerRightCornerLatitude,
                     lowerRightCornerLongitude,
                     sceneCenterLatitude,
                     sceneCenterLongitude)

#join to old scene list
meta_ETM <- left_join(metalist, LSETM_clean, by = "sceneID")

#############
#LS 4+5
#These will be done by decade
#############

#start with 1980-1989
#read in data
LS45_1980_1989_raw <- read_csv("./scenes/metadata/LANDSAT_TM-1980-1989/LANDSAT_TM-1980-1989.csv")

#clean up
LS45_clean_1980_1989 <-select(LS45_1980_1989_raw,
                     sceneID, 
                     sensor, 
                     acquisitionDate, 
                     upperLeftCornerLatitude, 
                     upperLeftCornerLongitude, 
                     upperRightCornerLatitude,
                     upperRightCornerLongitude,
                     lowerLeftCornerLatitude,
                     lowerLeftCornerLongitude,
                     lowerRightCornerLatitude,
                     lowerRightCornerLongitude,
                     sceneCenterLatitude,
                     sceneCenterLongitude)

#join to scene list
meta_45_1980_1989 <- left_join(metalist, LS45_clean_1980_1989, by = "sceneID")

#############
#now 1990-1999
LS45_1990_1999_raw <- read_csv("./scenes/metadata/LANDSAT_TM-1990-1999/LANDSAT_TM-1990-1999.csv")

#clean up
LS45_clean_1990_1999 <-select(LS45_1990_1999_raw,
                    sceneID, 
                    sensor, 
                    acquisitionDate, 
                    upperLeftCornerLatitude, 
                    upperLeftCornerLongitude, 
                    upperRightCornerLatitude,
                    upperRightCornerLongitude,
                    lowerLeftCornerLatitude,
                    lowerLeftCornerLongitude,
                    lowerRightCornerLatitude,
                    lowerRightCornerLongitude,
                    sceneCenterLatitude,
                    sceneCenterLongitude)

#join to scene list
meta_45_1990_1999 <- left_join(metalist, LS45_clean_1990_1999, by = "sceneID")
################
#now 2000-2009
LS45_2000_2009_raw <- read_csv("./scenes/metadata/LANDSAT_TM-2000-2009/LANDSAT_TM-2000-2009.csv")

#clean up
LS45_clean_2000_2009 <-select(LS45_2000_2009_raw,
                              sceneID, 
                              sensor, 
                              acquisitionDate, 
                              upperLeftCornerLatitude, 
                              upperLeftCornerLongitude, 
                              upperRightCornerLatitude,
                              upperRightCornerLongitude,
                              lowerLeftCornerLatitude,
                              lowerLeftCornerLongitude,
                              lowerRightCornerLatitude,
                              lowerRightCornerLongitude,
                              sceneCenterLatitude,
                              sceneCenterLongitude)

#join to scene list
meta_45_2000_2009 <- left_join(metalist, LS45_clean_2000_2009, by = "sceneID")

#######
#now 2010-2012
LS45_2010_2012_raw <- read_csv("./scenes/metadata/LANDSAT_TM-2010-2012/LANDSAT_TM-2010-2012.csv")

#clean up
LS45_clean_2010_2012 <-select(LS45_2010_2012_raw,
                              sceneID, 
                              sensor, 
                              acquisitionDate, 
                              upperLeftCornerLatitude, 
                              upperLeftCornerLongitude, 
                              upperRightCornerLatitude,
                              upperRightCornerLongitude,
                              lowerLeftCornerLatitude,
                              lowerLeftCornerLongitude,
                              lowerRightCornerLatitude,
                              lowerRightCornerLongitude,
                              sceneCenterLatitude,
                              sceneCenterLongitude)

#join to scene list
meta_45_2010_2012 <- left_join(metalist, LS45_clean_2010_2012, by = "sceneID")

#########
#now LS8
LS8_raw <- read_csv("./scenes/metadata/LANDSAT_8/LANDSAT_8.csv")

#clean up
LS8_clean <-select(LS8_raw,
                   sceneID,
                   acquisitionDate,
                   upperLeftCornerLatitude,
                   upperLeftCornerLongitude,
                   upperRightCornerLatitude,
                   upperRightCornerLongitude,
                   lowerLeftCornerLatitude,
                   lowerLeftCornerLongitude,
                   lowerRightCornerLatitude,
                   lowerRightCornerLongitude,
                   sceneCenterLatitude,
                   sceneCenterLongitude)

#join to scene list
meta_LS8 <- left_join(metalist, LS8_clean, by = "sceneID")
#######################




LSETM_clean,
LS45_clean_1980_1989,
LS45_clean_1990_1999,
LS45_clean_2000_2009,
LS45_clean_2010_2012,
LS8_clean


meta_complete <- left_join(metalist, LSETM_clean, by = "sceneID")
meta_complete <- left_join(meta_complete, LS8_clean, by = "sceneID")

meta_ETM,
meta_45_1980_1989,
meta_45_1990_1999,
meta_45_2000_2009,
meta_45_2010_2012,
meta_LS8, 

meta_complete <- bind_rows(meta_ETM, meta_45_1980_1989)




