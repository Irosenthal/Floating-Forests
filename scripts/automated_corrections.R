setwd("C:/Users/irose/OneDrive/FF/Floating-Forests")
source("./FFmethods_fix.R")
library(rgdal)
library(dplyr)
library(tidyr)
library(readr)
library(sp)
library(R.matlab)
#########################################
#correct tiles
############

#done in tile_lookup_df.R
#this gets correct tiles for all bad tiles, and the joins them to a df of subjects we need to find corner coords for
#output saved as remaining_kelp_subjects_correct_row_col.csv
#join this with the actual subjects to get all the other data on there
#this could be any subjects table, as long as it has row and cols to match up. 

################
#load up subjects table and metadata table
#Clean up both, join old, complete subject table to new table with correct columns
subjects <- read_csv("./data/mongosubjectsfix.csv")

#make a new set of columns that are complete row/cols
#these will be joined with my lookup table to stick on the correct row/col, and then get the right metadata corners stuck on

subjects <- subjects %>%
  mutate("bad_row"= paste0(metadata.row_no, metadata.row))

subjects <- subjects %>%
  mutate("bad_col"= paste0(metadata.col_no, metadata.col))


subjects <- subjects %>%
  mutate(bad_rows = gsub("NA", "", bad_row))


subjects <- subjects %>%
  mutate(bad_cols = gsub("NA", "", bad_col))

subjects$bad_rows <- as.numeric(subjects$bad_rows) +1
subjects$bad_cols <- as.numeric(subjects$bad_cols) +1



subjects <- subjects %>%
  select(-bad_row, -bad_col)


#get ready for join
subjects <- subjects %>%
  unite("row_col",c(bad_rows, bad_cols), remove = FALSE)

#clean up subjects to free up RAM

subjects <- subjects %>%
  dplyr::select(-state, -metadata.source_rows, -metadata.source_cols, -random, -project_id, -workflow_ids)


#bring in data for join, use grid_compare.csv

grid_compare <- read_csv("./grid_compare.csv")
grid_compare <- grid_compare %>%
  dplyr::select(-X1, -X1_1)

correct_subjects_full <- left_join(subjects, grid_compare, by = "row_col")

correct_subjects_full[1,28]

write.csv(correct_subjects_full, "corrected_subjects_full")
# test
correct_subjects_full <- read_csv("./corrected_subjects_full.csv")
imagelookup <- filter(correct_subjects_full, as.character(zooniverse_id) == "AKP00016e6")


#have to use the adjusted up row/cols because the matrix doesn't start at 0. 
#IE in the actual metadata image AKP00016e6 shows up as 14,12, but in this analysis it is 15,13
######
corrected_tiles <- read_csv("./corrected_subjects_full.csv")
imagelookup <- filter(corrected_tiles, as.character(zooniverse_id) == "AKP00016e6")
#######

###########if the above is done$$$$$$$$$$$

corrected_tiles <- read_csv("./corrected_subjects_full.csv")


#first, convert lon/lat corners from metadata to UTM

#This function may be outdated, see below using sptransform
#newLongLatToUTM<-function(x,y,zone){
  #xy <- data.frame(ID = 1:length(x), X = x, Y = y)
  #coordinates(xy) <- c("X", "Y")
  #proj4string(xy) <- CRS("+proj=longlat +datum=WGS84")  
  #res <- spTransform(xy, CRS(paste("+proj=utm +zone=",zone,",+south, ellps=WGS84, +datum=WGS84 +units=m +no_defs",sep='')))
  #return(as.data.frame(res))
#}

# test example, scene: LE70440351999204EDC01

y <- c(37.00333, #from tom, where did they come from?
       36.99319,
       35.08046,
       35.07101)

x <- c(-123.87555,
       -121.21886,
       -123.85451,
       -121.26163)

#tassie test, scene LT50900881987245ASA00
#from metadata csv, wrong
y <- c(-39.3757,
       -39.6838,
       -40.9468,
       -41.2619)

x<- c(147.287,
      149.402,
      146.762,
      148.924)
#from L1
y <- c(-39.32696,#UR
       -39.35302,#UL
       -41.27919,#LR
       -41.30710)#LL

x<- c(149.48268,#UR
      146.68431,#UL
      149.55525,#LR
      146.67507)#LL

#The ll2utm function is outdated, see below using sptransform
#sceneUTM <- newLongLatToUTM(x,y,10)


scenedf <- data.frame(x,y) #put points in df
scenesp<- SpatialPoints(scenedf, proj4string=CRS("+proj=longlat +datum=WGS84")) #convert to sp object
scenesp_utm <- spTransform(scenesp, ("+proj=utm +zone=10 +south +ellps=WGS84 +datum=WGS84 +units=m +no_defs")) #transform to utm
sceneUTM <- as.data.frame(scenesp_utm)

######################
#corner corrections for FF images
######################
#start here using full_subjects.csv
#####################

full_subjects <- read_csv("./full_subjects.csv")

no_rows  <-  20
no_colls <-  20
image_width  <- 7981
image_height <-  7271
image_chunk_width <- image_width / no_rows #364
image_chunk_height <- image_height / no_colls #399
######################

subject_corners <- function(scene){
  
  #make a grid of latlongs by 30 meters using corners of scene as start/end points
  lonlatpix <-expand.grid(x = seq(full_subjects$CORNER_LL_PROJECTION_X_PRODUCT, full_subjects$CORNER_LR_PROJECTION_X_PRODUCT, by = 30),  
                          y = seq(full_subjects$CORNER_LL_PROJECTION_Y_PRODUCT, full_subjects$CORNER_UL_PROJECTION_Y_PRODUCT, by = -30))
  
  #this assigns each to a unique  pixel to each set of utm coords within the scene
  lonlatpixIndex <- mutate(lonlatpix, PID = 1:nrow(lonlatpix))
  sizey <- n_distinct(lonlatpix$y)
  sizex <- n_distinct(lonlatpix$x) 
  
  #generate pixel numbers for corners of of each ff image
  height <- seq(1, sizey, by = 363)
  width <- seq(1, sizex, by = 399)
  
  #pull out individual pixels
  #these are the same as lat and lon in matlab script and make same numbers
  latpix <- lonlatpixIndex %>%
    distinct(y) %>%
    dplyr::select(y) %>%
    mutate(PID = c(1:n_distinct(y)))
  
  lonpix <- lonlatpixIndex %>%
    distinct(x) %>%
    dplyr::select(x) %>%
    mutate(PID = c(1:n_distinct(x)))
  
  #these are at least the first part to the lat and lonslices in matlab. they are at least labeling correctly and calling out the correct positions.
  #select the pixels that occur on tile corners and assign them a pixel ID
  latgridindex <- latpix[seq(1,length(latpix$y), by = 363),] %>%
    mutate(Row = c(1:n_distinct(y)))
  
  longridindex <- lonpix[seq(1,length(lonpix$x), by = 399),] %>%
    mutate(Col = c(1:n_distinct(x)))
  
  #need to account for 20th row due to incorrect data processing - rows and cols started at 0.
  
  lastlon <- lonpix[nrow(lonpix),]
  Col <- nrow(longridindex) +1 
  lonbind <- cbind(lastlon, Col)
  longridindex <- rbind(longridindex,lonbind)
  
  
  lastlat <- latpix[nrow(latpix),]
  Row <- nrow(latgridindex) +1 
  latbind <- cbind(lastlat, Row)
  latgridindex <- rbind(latgridindex,latbind)
  
  #indexing reminder: UTM coords are in first column of dataframe
  #pull out important points 
  upper_left_x_utm <- longridindex[col, 1]
  upper_left_y_utm <- latgridindex[row, 1]
  lower_left_x_utm <- longridindex[col, 1]
  lower_left_y_utm <- latgridindex[row + 1, 1]
  upper_right_x_utm <- longridindex[col + 1, 1]
  upper_right_y_utm <- latgridindex[row, 1]
  lower_right_x_utm <-longridindex[col + 1, 1]
  lower_right_y_utm <- latgridindex[row + 1, 1]
  center_x_utm <- (upper_left_x_utm + ((upper_right_x_utm - upper_left_x_utm)/2))
  center_y_utm <- (lower_left_y_utm + ((upper_left_y_utm - lower_left_y_utm)/2))
  
  FFcorners_utm_function <- data.frame(upper_left_x_utm, upper_left_y_utm, 
                                       lower_left_x_utm, lower_left_y_utm, 
                                       upper_right_x_utm, upper_right_y_utm, 
                                       lower_right_x_utm, lower_right_y_utm,
                                       center_x_utm, center_y_utm)
  
}

#test out
#grab just a chunk of subjects


subjects_test <- full_subjects[1:100,]

corner_test <- sapply(subjects_test$zooniverse_id, subject_corners)












#pull in subject table with corrected row/cols, from tilerotation.r
data <- read_csv("scene_with_FF_UTM_correct_row_cols.csv")

#just want one scene
#this dataframe, by definition, only has one scene. shouldn't need this
#scenelookup <- filter(data, sceneID == "LE70440351999204EDC01")
##################
#function to recalculate corners based on NONRESIZED FF images
cornercalc <- function(arow){
  row <- unlist(scenelookup[arow,'true_row'])
  col <- unlist(scenelookup[arow,'true_col'])
  
  #indexing reminder: UTM coords are in first column of dataframe
  
  #pull out important points 
  upper_left_x_utm <- longridindex[col, 1]
  upper_left_y_utm <- latgridindex[row, 1]
  lower_left_x_utm <- longridindex[col, 1]
  lower_left_y_utm <- latgridindex[row + 1, 1]
  upper_right_x_utm <- longridindex[col + 1, 1]
  upper_right_y_utm <- latgridindex[row, 1]
  lower_right_x_utm <-longridindex[col + 1, 1]
  lower_right_y_utm <- latgridindex[row + 1, 1]
  center_x_utm <- (upper_left_x_utm + ((upper_right_x_utm - upper_left_x_utm)/2))
  center_y_utm <- (lower_left_y_utm + ((upper_left_y_utm - lower_left_y_utm)/2))
  
  FFcorners_utm_function <- data.frame(upper_left_x_utm, upper_left_y_utm, 
                                       lower_left_x_utm, lower_left_y_utm, 
                                       upper_right_x_utm, upper_right_y_utm, 
                                       lower_right_x_utm, lower_right_y_utm,
                                       center_x_utm, center_y_utm)
}
######################
#set up a dataframe to populate
scene_with_FF_UTM <- scenelookup

##apply cornercalc to data
rows <- nrow(scenelookup)
sceneslice_coords <- sapply(1:nrow(scenelookup), cornercalc)

#now transpose
sceneslice_coords_clean <- t(sceneslice_coords)

#bind back to scene table
scene_with_FF_UTM <- cbind(scene_with_FF_UTM, sceneslice_coords_clean)

#structure data for conversion from utm to lon lat
#This might not be necessary
center_lon <- scene_with_FF_UTM$center_x_utm %>%
  unlist()
center_lat <- scene_with_FF_UTM$center_y_utm %>%
  unlist()
lower_left_lat <- scene_with_FF_UTM$lower_left_x_utm %>%
  unlist()
lower_left_lon <- scene_with_FF_UTM$lower_left_y_utm %>%
  unlist()
upper_right_lat <- scene_with_FF_UTM$upper_right_x_utm %>%
  unlist()
upper_right_lon<- scene_with_FF_UTM$upper_right_y_utm %>% 
  unlist()

URutm <- data.frame(upper_right_lat = upper_right_lat, upper_right_lon =upper_right_lon)
LLutm <- data.frame(lower_left_lat = lower_left_lat, lower_left_lon =lower_left_lon)
#one of these is NA, track down why later. for now (as we check processing time) remove it
LLutm <- rm_na(LLutm, "lower_left_lon")

# prepare UTM coordinates matrix
utmcoor_LL<-SpatialPoints(cbind(LLutm$lower_left_lat,LLutm$lower_left_lon),
                          proj4string=CRS("+proj=utm +zone=10"))


utmcoor_UR<-SpatialPoints(cbind(URutm$upper_right_lat, URutm$upper_right_lon),
                          proj4string=CRS("+proj=utm +zone=10"))
#utmdata$X and utmdata$Y are corresponding to UTM Easting and Northing,
#utmdata$X and utmdata$Y are corresponding to UTM Easting and Northing, respectively.
#zone= UTM zone

#now convert to lonlat. again, maybe not necessary
UTMcoor_LLSP<-spTransform(utmcoor_LL,CRS("+proj=longlat"))
#now convert to lonlat
UTMcoor_URSP<-spTransform(utmcoor_UR,CRS("+proj=longlat"))
#convert to to a data.frame
UTMcoor_LLSP_df <- data.frame(LL_longitude = coordinates(UTMcoor_LLSP)[,1], LL_latitude = coordinates(UTMcoor_LLSP)[,2])
UTMcoor_URSP_df <- data.frame(UR_longitude = coordinates(UTMcoor_URSP)[,1], UR_latitude = coordinates(UTMcoor_URSP)[,2])

#add lon/lats
scene_with_FF_UTM <- cbind(scene_with_FF_UTM, UTMcoor_URSP_df, UTMcoor_LLSP_df)

#cleanup and save
scene_with_FF_UTM<-sapply(scene_with_FF_UTM,unlist)
scene_with_FF_UTM <- as.data.frame(scene_with_FF_UTM)
scene_with_FF_UTM <- rm_na(scene_with_FF_UTM, "lower_left_y_utm")
write_csv(scene_with_FF_UTM, path = "scene_with_FF_UTM_fix.csv")


###############
#Fix classification table

#bring in data if not still loaded
scene_with_FF_UTM <- read_csv("scene_with_FF_UTM_fix.csv")
classifications_raw <- read_csv("./2016-05-29_kelp_classifications.csv")
corrected_tiles_tidy <- scene_with_FF_UTM
classifications <- classifications_raw

#get rid of old, bad coords
classifications$upper_right_x <- NULL 
classifications$upper_right_y <- NULL
classifications$lower_left_x <- NULL
classifications$lower_left_y <- NULL

#join good coords back on
classifications <- left_join(classifications, corrected_tiles_tidy, by = c("subject_zooniverse_id"="zooniverse_id"))
#now get rid of everything that isn't part of the desired scene 
rm_na <- function(data, desiredCols) {
  completeVec <- complete.cases(data[, desiredCols])
  return(data[completeVec, ])
}

classifications <- rm_na(classifications, "upper_right_x_utm")

#save
write_csv(classifications, "./classifications_correct_cords_1_scene_fix.csv")
###############################################################################
#for plotting/validation, refer to spatial_polys_nest.R, in this repo


