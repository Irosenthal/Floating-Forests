#load functions needed
source("./FFmethods_fix.R")


library(ggplot2)

#load dataset to work on
load("../data/oneImage_data_only_AKP00016e6.RData")
proj <- "+proj=utm +zone=11 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"

#run the correction on corners
corners <- getCorrectCorners(oneImage$subject_zooniverse_id[1], proj=proj)

#fix the corners to the proper projection
oneImage$lower_left_x <- corners$ll[1]
oneImage$lower_left_y <- corners$ll[2]
oneImage$upper_right_x <- corners$ur[1]
oneImage$upper_right_y <- corners$ur[2]

oneImage <- out5



#FROM TOM
#Upper Left: 36.315309, -122.000823
#Lower Left: 36.217145, -122.002073
#Upper Right: 36.314121, -121.867502
#Lower Right: 36.215961, -121.868919

oneImage$lower_left_x <- -122.002
oneImage$lower_left_y <- 36.217
oneImage$upper_right_x <- -121.868
oneImage$upper_right_y <- 36.314

# mine based on resize
#oneImage$lower_left_x <- -122.0022451
#oneImage$lower_left_y <- 36.21704645
#oneImage$upper_right_x <- -121.8676758
#oneImage$upper_right_y <- 36.31398551



#mine based on nonresized
oneImage$lower_left_x <- -122.0022
oneImage$lower_left_y <-  36.21701
oneImage$upper_right_x <- -121.8677
oneImage$upper_right_y <- 36.31399

row 8 
col 15


#turn into a polygon
polysData <- getSpatialPolysDataFrameForOneImage(oneImage, 
                                                 proj="+proj=utm +zone=10 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")

#plot(polysData)

tileBrick <- rasterizeFFImage(oneImage[1,],
                              proj="+proj=utm +zone=10 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")


plotRGB(tileBrick)
plot(polysData, add=T)



####
#load at the real coastline
####
library(rgdal)
coasts <- readOGR("./data/GSHHS_shp/h", layer="GSHHS_h_L1",  useC=FALSE)

#plot the image
plotRGB(tileBrick)

#plot the cropped coastline on top
coastCrop <- crop(coasts, extent(tileBrick))
plot(coastCrop, add=T, lwd=4)
