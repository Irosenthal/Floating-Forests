

#To rotate, you need to  reverse the columns and then transpose
rotate <- function(x) t(apply(x, 2, rev))


######create a 20x20 matrix to simulate ff data
x<-c(1:20)
y<-c(1:20)
#each row col combo
mat_test <- expand.grid(x = x,y = y)

#now each gets a unique row/col (might be obsolete)
mat_test <- mat_test %>%
  unite("x,y", x, y,  sep = ",", remove = FALSE)

#each row/col combo gets a tile #. I will use this to assign these to actual FF data
tile <- c(1:nrow(mat_test))
mat_test <- cbind(mat_test, tile)



#######################################
#####Do it with the image ID
#############################
#convert this to a matrix
matr <- matrix(mat_test$tile, 20)
matr

#transposing creates the matrix as set up for FF (left to right), but with correct positions
true_tiles <- matr <- t(matr)

#this creates the matrix as made by FF - this one is rotated 90 degrees counterclockwise 
ff_tiles <- rotate(rotate(rotate(matr)))


#can't do image ids because I don't have all of them - couldnt populate the entire matrix and there's no logical way to fill gaps with NAs or something
#to look up the row/col of a particular value
#trying with new dplyr stuff
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
