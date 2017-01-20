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


######create a 20x20 matrix to simulate ff data
mat_test <- expand.grid(x = c(1:20),y = c(1:20))

#now each gets a unique row/col (might be obsolete)
mat_test <- mat_test %>%
  unite("x,y", x, y,  sep = ",", remove = FALSE)

#each row/col combo gets a tile #. I will use this to assign these to actual FF data
tile <- c(1:nrow(mat_test))
mat_test <- cbind(mat_test, tile)

#convert this to a matrix
matr <- matrix(mat_test$tile, 20)
matr

#transposing creates the matrix as set up for FF (left to right), but with correct positions
true_tiles <- matr <- t(matr)

#this creates the matrix as made by FF - this one is rotated 90 degrees counterclockwise 
ff_tiles <- rotate(rotate(rotate(matr)))

################
#load up subjects table and metadata table
#Clean up both, join subjects to metadata and then add correct row cols

mongosubs <- read_csv("mongosubjectsfix.csv")

#create column in subjects table to clean up sceneID
subjects_clean <- as.character(mutate(mongosubs, file = metadata.file))

#now convert this to a character
subjects_cleaner$clean <- as.character(subjects_cleaner$clean)

subjects_clean <- subjects_cleaner

#and remove.tar.gz

subjects_clean$clean <- as.character(gsub(".tar.gz", "", subjects_cleaner$clean))

#rename to clean to sceneID to prep for join

colnames(subjects_clean)[colnames(subjects_clean)=="clean"] <- "sceneID"

#make them both characters for join

subjects_clean$sceneID <- as.character(subjects_clean$sceneID)


meta$sceneID <- as.character(meta$sceneID)



# now join the tables

antisubmeta <- anti_join(subjects_clean, meta, by  = "sceneID")
submetaraw <- left_join(subjects_clean, meta, by  = "sceneID")



#row and cols start from 0, need to add 1 to each
submeta <- submetaraw %>%
  mutate(FFrow = as.numeric(metadata.row_no) +1) %>%
  mutate(FFcol = as.numeric(metadata.col_no) +1)


# now choose 1 scene and add correct grid coords. start with image AKP00016e6 because we have used it before

imagelookup <- filter(submeta, as.character(zooniverse_id) == "AKP00016e6")
scenelookup <- filter(submeta, sceneID == "LT50900881987245ASA00")

#this is outdated, use scenelookup from meta_compare.R
scene_with_FF_UTM_master <- read_csv("scene_with_FF_UTM_FIX.csv")

#have to use the adjusted up row/cols because the matrix doesn't start at 0. 
#IE in the actual metadata image AKP00016e6 shows up as 14,12, but in this analysis it is 15,13

scene_with_FF_UTM <- scenelookup %>%
  rowwise() %>%
  mutate(tile_ID = true_tiles[FFrow, FFcol])

#check the known image
imagelookup <- filter(scene_with_FF_UTM, as.character(zooniverse_id) == "AKP00016e6")

#looks good, each recieved the correct ID number

#Now I need to look up the coords of that ID in the true_tiles matrix

elem_lookup <- which(true_tiles == "293", arr.ind = TRUE)

scene_with_FF_UTM <- scene_with_FF_UTM %>%
  rowwise() %>%
  mutate(true_row = which(ff_tiles == tile_ID, arr.ind = TRUE)[1]) %>%
  mutate(true_col = which(ff_tiles == tile_ID, arr.ind = TRUE)[2]) 

write_csv(scene_with_FF_UTM, "scene_with_FF_UTM_correct_row_cols.csv" )

scene_with_FF_UTM <- read_csv("./scene_with_FF_UTM_correct_row_cols.csv")

######
corrected_tiles <- read_csv("./scene_with_FF_UTM_correct_row_cols.csv")
imagelookup <- filter(corrected_tiles, as.character(zooniverse_id) == "AKP00016e6")
#######

### some cleanup

#convert all UTM to lat long

#now take scene_with_FF_UTM, made by the cornercalc function and do this stuff.

#going the wrong way here
corrected_tiles_tidy_PH <- corrected_tiles_tidy

corrected_tiles <- corrected_tiles_tidy_PH

corrected_tiles <- scene_with_FF_UTM


#get UTM coordinates
upper_left_x <- corrected_tiles$upper_left_x_utm
upper_left_x <- unlist(upper_left_x)

upper_left_y <- corrected_tiles$upper_left_y_utm
upper_left_y <-  unlist(upper_left_y)

upper_right_x <- corrected_tiles$upper_right_x_utm
upper_right_x <-  unlist(upper_right_x)

upper_right_y <- corrected_tiles$upper_right_y_utm
upper_right_y <-  unlist(upper_right_y)

lower_left_x <- corrected_tiles$lower_left_x_utm
lower_left_x <-  unlist(lower_left_x)

lower_left_y <- corrected_tiles$lower_left_y_utm
lower_left_y <-  unlist(lower_left_y)

lower_right_x <- corrected_tiles$lower_right_x_utm
lower_right_x <-  unlist(lower_right_x)

lower_right_y <- corrected_tiles$lower_right_y_utm
lower_right_y <-  unlist(lower_right_y)


imageutm <- data.frame(upper_left_x, upper_left_y, 
                       upper_right_x, upper_right_y, 
                       lower_left_x, lower_left_y, 
                       lower_right_x, lower_right_y)


# prepare SpatialPoints objects
utmcoor_UR<-SpatialPoints(cbind(imageutm$upper_right_x, imageutm$upper_right_y),
                          proj4string=CRS("+proj=utm +zone=10"))

utmcoor_UL<-SpatialPoints(cbind(imageutm$upper_left_x,imageutm$upper_left_y),
                          proj4string=CRS("+proj=utm +zone=10"))


utmcoor_LR<-SpatialPoints(cbind(imageutm$lower_right_x, imageutm$lower_right_y),
                          proj4string=CRS("+proj=utm +zone=10"))


utmcoor_LL<-SpatialPoints(cbind(imageutm$lower_left_x, imageutm$lower_left_y),
                          proj4string=CRS("+proj=utm +zone=10"))


#now convert to lonlat
UR_lon_lat_sp<-spTransform(utmcoor_UR,CRS("+proj=longlat"))
UL_lon_lat_sp<-spTransform(utmcoor_UL,CRS("+proj=longlat"))
LR_lon_lat_sp<-spTransform(utmcoor_LR,CRS("+proj=longlat"))
LL_lon_lat_sp<-spTransform(utmcoor_LL,CRS("+proj=longlat"))

#convert to to a data.frame

UR_lon_lat <- data.frame(longitude = coordinates(UR_lon_lat_sp)[,1], 
                         latitude = coordinates(UR_lon_lat_sp)[,2])

UL_lon_lat <- data.frame(longitude = coordinates(UL_lon_lat_sp)[,1], 
                         latitude = coordinates(UL_lon_lat_sp)[,2])

LR_lon_lat <- data.frame(longitude = coordinates(LR_lon_lat_sp)[,1], 
                         latitude = coordinates(LR_lon_lat_sp)[,2])

LL_lon_lat <- data.frame(longitude = coordinates(LL_lon_lat_sp)[,1], 
                         latitude = coordinates(LL_lon_lat_sp)[,2])

corners_long_lat <- data.frame(UR_lon_lat, UL_lon_lat, LR_lon_lat, LL_lon_lat)

#set up destination data frame


corrected_tiles_tidy <- corrected_tiles %>%
  dplyr::select(sceneID, zooniverse_id, true_row, true_col, sceneID)

corrected_tiles_tidy <- corrected_tiles %>%
  dplyr::select(29,31,56,12)

#thats messed up, just do it in excel

corrected_tiles_tidy<-sapply(corrected_tiles_tidy,unlist)
corrected_tiles_tidy <- as.data.frame(corrected_tiles_tidy)


write_csv(scene_with_FF_UTM, path = "scene_with_FF_UTM.csv")

write_csv(corrected_tiles_tidy, "tidy_scene_with_FF_UTM_correct_row_cols.csv" )

corrected_tiles_tidy <- read_csv("tidy_scene_with_FF_UTM_correct_row_cols.csv")


#Jam those new coords on there

corrected_tiles_tidy <- corrected_tiles_tidy %>%
  dplyr::mutate(lower_left_x = corners_long_lat$longitude.3, lower_left_y = corners_long_lat$latitude.3,
                upper_right_x = corners_long_lat$longitude, upper_right_y = corners_long_lat$latitude)

write_csv(corrected_tiles_tidy, "tidy_scene_with_FF_UTM_correct_row_cols_don't_touch.csv" )

#########################################################
corrected_tiles_tidy <- read_csv("tidy_scene_with_FF_UTM_correct_row_cols.csv")



#first, convert lon/lat corners from metadata to UTM

newLongLatToUTM<-function(x,y,zone){
  xy <- data.frame(ID = 1:length(x), X = x, Y = y)
  coordinates(xy) <- c("X", "Y")
  proj4string(xy) <- CRS("+proj=longlat +datum=WGS84")  
  res <- spTransform(xy, CRS(paste("+proj=utm +zone=",zone,",+south, ellps=WGS84, +datum=WGS84 +units=m +no_defs",sep='')))
  return(as.data.frame(res))
}

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


sceneUTM <- newLongLatToUTM(x,y,55)


scenedf <- data.frame(x,y)

scenesp<- SpatialPoints(scenedf, proj4string=CRS("+proj=longlat +datum=WGS84"))

scenesp_utm <- spTransform(scenesp, ("+proj=utm +zone=55 +south +ellps=WGS84 +datum=WGS84 +units=m +no_defs"))


sceneUTM <- as.data.frame(scenesp_utm)

######################
#corner corrections for FF images
######################

no_rows  <-  20
no_colls <-  20
image_width  <- 7981
image_height <-  7271
image_chunk_width <- image_width / no_rows #364
image_chunk_height <- image_height / no_colls #399

#make a grid of latlongs by 30 meters using corners of scene as start/end points
lonlatpix <-expand.grid(x = seq(min(sceneUTM$x), max(sceneUTM$x), by = 30), y = seq(max(sceneUTM$y), min(sceneUTM$y), by = -30))



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
#can't simply add 1 because then there is the 21st row or col (which is correct) but it wouldnt have a value

lastlon <- lonpix[nrow(lonpix),]
Col <- nrow(longridindex) +1 
lonbind <- cbind(lastlon, Col)
longridindex <- rbind(longridindex,lonbind)


lastlat <- latpix[nrow(latpix),]
Row <- nrow(latgridindex) +1 
latbind <- cbind(lastlat, Row)

#pull in subject table with corrected row/cols, from tilerotation.r
data <- read_csv("scene_with_FF_UTM_correct_row_cols.csv")

#just want one scene
scenelookup <- filter(data, sceneID == "LT50900881987245ASA00")
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


