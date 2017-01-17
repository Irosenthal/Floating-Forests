library(rgdal)
library(dplyr)
library(tidyr)
library(sp)


#first, convert lon/lat corners from metadata to UTM

new_LongLatToUTM<-function(x,y,zone){
  xy <- data.frame(ID = 1:length(x), X = x, Y = y)
  coordinates(xy) <- c("X", "Y")
  proj4string(xy) <- CRS("+proj=longlat +datum=WGS84")  
  res <- spTransform(xy, CRS(paste("+proj=utm +zone=",zone," ellps=WGS84, +datum=WGS84 +units=m +no_defs",sep='')))
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

newsceneUTM <- new_LongLatToUTM(x,y,10)

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
lonlatpix <-expand.grid(x = seq(min(sceneUTM$X), max(sceneUTM$X), by = 30), y = seq(max(sceneUTM$Y), min(sceneUTM$Y), by = -30))



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
scenelookup <- filter(data, sceneID == "LE70440351999204EDC01")
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



##apply results of cornercalc to the rest of the data
rows <- nrow(scenelookup)
sceneslice_coords <- sapply(1:nrow(scenelookup), cornercalc)

#now transpose

sceneslice_coords_clean <- t(sceneslice_coords)

#bind back to scene table

scene_with_FF_UTM <- cbind(scene_with_FF_UTM, sceneslice_coords_clean)


###remove NAs
rm_na <- function(data, desiredCols) {
  completeVec <- complete.cases(data[, desiredCols])
  return(data[completeVec, ])
}


scene_with_FF_UTM_clean <- rm_na(scene_with_FF_UTM, "lower_left_y_utm")


#structure data for conversion from utm to lon lat

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

#now convert to lonlat
UTMcoor_LLSP<-spTransform(utmcoor_LL,CRS("+proj=longlat"))
#now convert to lonlat
UTMcoor_URSP<-spTransform(utmcoor_UR,CRS("+proj=longlat"))
#converto to a data.frame

UTMcoor_LLSP_df <- data.frame(LL_longitude = coordinates(UTMcoor_LLSP)[,1], LL_latitude = coordinates(UTMcoor_LLSP)[,2])


UTMcoor_URSP_df <- data.frame(UR_longitude = coordinates(UTMcoor_URSP)[,1], UR_latitude = coordinates(UTMcoor_URSP)[,2])


#add lon/lats

scene_with_FF_UTM <- cbind(scene_with_FF_UTM, UTMcoor_URSP_df, UTMcoor_LLSP_df)




scene_with_FF_UTM<-sapply(scene_with_FF_UTM,unlist)
scene_with_FF_UTM <- as.data.frame(scene_with_FF_UTM)

scene_with_FF_UTM <- rm_na(scene_with_FF_UTM, "lower_left_y_utm")


write_csv(scene_with_FF_UTM, path = "scene_with_FF_UTM_fix.csv")

scene_with_FF_UTM <- read_csv("scene_with_FF_UTM_fix.csv")
