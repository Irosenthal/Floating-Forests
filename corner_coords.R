####
library(dplyr)
library(tidyr)
##replicate matlab script

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
latgridindex <- rbind(latgridindex,latbind)

#enter row and col of desired image   (row 8 col 15 for test image) 
row <- 8
col <- 16


#indexing reminder: UTM coords are in first column of dataframe
#latresize and lonresize give the UTM for each pixel in the FF image. 
#The 1st and last value are the top/bottom or left/right respectively
difflat <- latgridindex[row, 1] - latgridindex[row +1, 1]
difflatE <- difflat/483 # utm degrees per pixel
latresize <- seq(latgridindex[row+1, 1], latgridindex[row, 1], by = difflatE)

difflon <- longridindex[col +1, 1] - longridindex[col, 1]
difflonE <- difflon/531
lonresize <- seq(longridindex[col, 1], longridindex[col + 1, 1], by = difflonE)
  

#now to clean up and convert back to lon lat

#pull out important points 
upper_left_x_utm <- min(lonresize)
upper_left_y_utm <- max(latresize)
lower_left_x_utm <- min(lonresize)
lower_left_y_utm <- min(latresize)
upper_right_x_utm <- max(lonresize)
upper_right_y_utm <- max(latresize)
lower_right_x_utm <-max(lonresize)
lower_right_y_utm <- min(latresize)

FFcorners_utm <- data.frame(upper_left_x_utm, upper_left_y_utm, 
                        lower_left_x_utm, lower_left_y_utm, 
                        upper_right_x_utm, upper_right_y_utm, 
                        lower_right_x_utm, lower_right_y_utm)

#structure data for conversion from utm to lon lat

lons <- c(upper_left_x_utm, upper_right_x_utm, lower_left_x_utm, lower_right_x_utm)
lats <- c(upper_left_y_utm, upper_right_y_utm, lower_left_y_utm, lower_right_y_utm)


imageutmlist <- dplyr::select(FFcorners_utm, center_x_utm, center_y_utm)

imageutmlist <- data.frame(lons, lats)
asnumbersx <- as.numeric(imageutmlist[[1]])
asnumbersy <- as.numeric(imageutmlist[[2]])

imagesutm <- data.frame(asnumbersx, asnumbersy)

library(rgdal)

# prepare UTM coordinates matrix
utmcoor<-SpatialPoints(cbind(imagesutm$asnumbersx,imagesutm$asnumbersy), proj4string=CRS("+proj=utm +zone=10"))

#utmdata$X and utmdata$Y are corresponding to UTM Easting and Northing, respectively.
#zone= UTM zone

#now convert to lonlat
lonlatcoorSP<-spTransform(utmcoor,CRS("+proj=longlat"))

#converto to a data.frame

lonlatcoor <- data.frame(longitude = coordinates(lonlatcoorSP)[,1], latitude = coordinates(lonlatcoorSP)[,2])

#clean up and label columns

upper_left_lon <- lonlatcoor[1,1]
upper_left_lat <- lonlatcoor[1,2]
lower_left_lon <- lonlatcoor[3,1]
lower_left_lat <- lonlatcoor[3,2]
upper_right_lon <- lonlatcoor[2,1]
upper_right_lat <- lonlatcoor[2,2]
lower_right_lon <- lonlatcoor[4,1]
lower_right_lat <- lonlatcoor[4,2]

clean_image_lon_lat <- data.frame(upper_right_lon, upper_left_lat, 
                                  lower_left_lon, lower_left_lat, 
                                  upper_right_lon, upper_right_lat,
                                  lower_right_lon, lower_right_lat)


#bind to main table
scene_with_FF_UTM <- cbind(scene_with_FF_UTM, lonlatcoor)

#check points on a map


library(ggmap)
#install.packages("ggmap")

#using this as a landmark, revisit once I have actual
#source of scene metadata

big_sur <- 'big_sur' 
scene_map <- qmap(big_sur, zoom = 7)

scene <- scene_map + 
  Notiosorex crawfordgeom_point(data = corrected_tiles_tidy, aes(x = upper_right_x, 
                                          y = upper_right_y)) +
  geom_point(data = scenelatlon, aes(x = x, y = y)) +
  geom_point(data = caKelp, aes(x = x, y = y), color = "red")


scene


caKelp <- data.frame( x = c(-121.8992, -120.8681),y = c(35.21637, 36.30411) )


