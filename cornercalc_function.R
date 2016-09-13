

#grab all row/cols from the scene

#calculate correct center coord based on my scene calculations, then plot them 
#with the correctly sliced scene and the row/col as text.
#the row cols will be where they think they should be but things might be out 
#of order



#finds the scene
scenelookup <- filter(submeta, sceneID == "LE70440351999204EDC01")


#function derived from corner_coords.R, but still needs several steps from that
#file to function. input is row and col number of desired image

cornercalc(1)


cornercalc <- function(arow){
row <- scenelookup[arow,'FFrow']
col <- scenelookup[arow,'FFcol']
  
  #indexing reminder: UTM coords are in first column of dataframe
  #latresize and lonresize give the UTM for each pixel in the FF image. 
  #The 1st and last value are the top/bottom or left/right respectively
  difflat <- latgridindex[row, 1] - latgridindex[row +1, 1]
  difflatE <- difflat/483 # utm degrees per pixel
  latresize <- seq(latgridindex[row+1, 1], latgridindex[row, 1], by = difflatE)
  
  difflon <- longridindex[col+1, 1] - longridindex[col, 1]
  difflonE <- difflon/531
  lonresize <- seq(longridindex[col, 1], longridindex[col+1, 1], by = difflonE)

  #pull out important points 
  upper_left_x_utm <- min(lonresize)
  upper_left_y_utm <- max(latresize)
  lower_left_x_utm <- min(lonresize)
  lower_left_y_utm <- min(latresize)
  upper_right_x_utm <- max(lonresize)
  upper_right_y_utm <- max(latresize)
  lower_right_x_utm <-max(lonresize)
  lower_right_y_utm <- min(latresize)
  center_x_utm <- (upper_left_x_utm + ((upper_right_x_utm - upper_left_x_utm)/2))
  center_y_utm <- (lower_left_y_utm + ((upper_left_y_utm - lower_left_y_utm)/2))
  
  FFcorners_utm_function <<- data.frame(upper_left_x_utm, upper_left_y_utm, 
                              lower_left_x_utm, lower_left_y_utm, 
                              upper_right_x_utm, upper_right_y_utm, 
                              lower_right_x_utm, lower_right_y_utm,
                              center_x_utm, center_y_utm)
  }



#########

#####
#structure data for conversion from utm to lon lat

center_lon <- scene_with_FF_UTM$center_x_utm
center_lat <- scene_with_FF_UTM$center_y_utm
center_lon <- unlist(center_lon)
center_lat <- unlist(center_lat)


imageutm <- data.frame(center_lon, center_lat)


library(rgdal)

# prepare UTM coordinates matrix
utmcoor<-SpatialPoints(cbind(imageutm$center_lon,imageutm$center_lat), proj4string=CRS("+proj=utm +zone=10"))
#utmdata$X and utmdata$Y are corresponding to UTM Easting and Northing, respectively.
#zone= UTM zone

#now convert to lonlat
centerlonlatcoorSP<-spTransform(utmcoor,CRS("+proj=longlat"))

#converto to a data.frame

centerlonlatcoor <- data.frame(longitude = coordinates(centerlonlatcoorSP)[,1], latitude = coordinates(centerlonlatcoorSP)[,2])





############
#set up a dataframe to populate
scene_with_FF_UTM <- scenelookup



##apply results of cornercalc to the rest of the data
rows <- nrow(scenelookup)
sceneslice_coords <- sapply(1:nrow(scenelookup), cornercalc)

#now transpose

sceneslice_coords_clean <- t(sceneslice_coords)

#bind back to scene table

scene_with_FF_UTM <- cbind(scene_with_FF_UTM, sceneslice_coords_clean)

#add a col combinig row/col, for labeling plots later

scene_with_FF_UTM <- unite(scene_with_FF_UTM, FFrow_col, FFrow, FFcol, sep = ",", remove = FALSE)

#add lon/lats

scene_with_FF_UTM <- cbind(scene_with_FF_UTM, centerlonlatcoor)






