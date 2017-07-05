library(sp)
library(rgdal)
library(dplyr)

#Function to convert wgs84 to utm
LongLatToUTM<-function(x,y,zone){
  xy <- data.frame(ID = 1:length(x), X = x, Y = y)
  coordinates(xy) <- c("X", "Y")
  proj4string(xy) <- CRS("+proj=longlat +datum=WGS84")  
  res <- spTransform(xy, CRS(paste("+proj=utm +zone=",zone," ellps=WGS84",sep='')))
  return(as.data.frame(res))
}



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


#these came from the geotif
-123.8547 35.08033
-123.8757 37.00346
-121.2615 35.07088
-121.2187 36.99333

x <- c(cors_new$longitude)






sceneUTM <- LongLatToUTM(cors_new$longitude,cors_new$latitude,10)

#for later plotting
scenelatlon <- data.frame(x,y)



#the following code adds UTM zones to the metadata table based on upper left longigtude

metadata$UTMzone <- ifelse(metadata$upperLeftCornerLongitude >= -120 & metadata$upperLeftCornerLongitude <=-114, 11, ifelse(metadata$upperLeftCornerLongitude >= 140 & metadata$upperLeftCornerLongitude <= 150, 55, 10))

write.csv(metadata, file = 'landsat_scenes_filtered_metadata_with_UTM.csv')



prepare UTM coordinates matrix
utmcoor_new<-SpatialPoints(cbind(utm_new$x,utm_new$y), proj4string=CRS("+proj=utm +zone=10"))
#utmdata$X and utmdata$Y are corresponding to UTM Easting and Northing, respectively.
#zone= UTM zone

#now convert to lonlat
cors<-spTransform(utmcoor_new,CRS("+proj=longlat"))

#converto to a data.frame

cors_new <- data.frame(longitude = coordinates(cors)[,1], latitude = coordinates(cors)[,2])

422085,3882285
422085,4095615
658515,3882285
658515,4095615

utm_new <- data.frame(x = c(422085, 422085, 658515, 658515), 
                      y =c(3882285, 4095615,3882285, 4095615) )

