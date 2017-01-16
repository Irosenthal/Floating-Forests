library(sp)
library(rgdal)
library(plyr)
library(dplyr)

#Function to convert wgs84 to utm
LongLatToUTM<-function(x,y,zone){
  xy <- data.frame(ID = 1:length(x), X = x, Y = y)
  coordinates(xy) <- c("X", "Y")
  proj4string(xy) <- CRS("+proj=longlat +datum=WGS84")  
  res <- spTransform(xy, CRS(paste("+proj=utm +zone=",zone," ellps=WGS84, +datum=WGS84 +units=m +no_defs",sep='')))
  return(as.data.frame(res))
}#



# test example, scene: LE70440351999204EDC01

#x <- c(-123.374, -121.264, -123.822, -121.754) from metadata
#y <-c(37.003, 36.6948, 35.3808, 35.0789)

y <- c(37.00333, #from tom, where did they come from?
       36.99319,
       35.08046,
       35.07101)

x <- c(-123.87555,
       -121.21886,
       -123.85451,
       -121.26163)

sceneUTM <- LongLatToUTM(x,y,10)

#for later plotting
scenelatlon <- data.frame(x,y)



#the following code adds UTM zones to the metadata table based on upper left longigtude

metadata$UTMzone <- ifelse(metadata$upperLeftCornerLongitude >= -120 & metadata$upperLeftCornerLongitude <=-114, 11, ifelse(metadata$upperLeftCornerLongitude >= 140 & metadata$upperLeftCornerLongitude <= 150, 55, 10))

write.csv(metadata, file = 'landsat_scenes_filtered_metadata_with_UTM.csv')






