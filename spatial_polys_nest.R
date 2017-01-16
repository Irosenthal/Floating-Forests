source("./FFmethods_fix.R")
library(R.matlab)


#First, create a dataframe to nest spatial poly data frames in
#dont need to do this each time
scene_data <- read_csv("tidy_scene_with_FF_UTM_correct_row_cols_don't_touch.csv")
sp_data <- corrected_tiles_tidy %>%
  dplyr::select(zooniverse_id, upper_right_x, upper_right_y, lower_left_x, lower_left_y)



#need to clean up classifications data table by removing any NA coordinates
rm_na <- function(data, desiredCols) {
  completeVec <- complete.cases(data[, desiredCols])
  return(data[completeVec, ])
}

classifications <- read_csv("./classifications_correct_cords_1_scene.csv")

classifications_clean <- rm_na(classifications, "relPath")
classifications_clean <- rm_na(classifications, "startingPoint_x")
classifications_clean <- rm_na(classifications, "startingPoint_y")


#need to group each image into it's own data frame based on image id
#make a dataframe where each image has its data nested within it is a frame.
#run get spatial polys on that dataframe

out <- split(classifications_clean , f = classifications_clean$subject_zooniverse_id)

#now lapply

sp_classifications_list <- lapply(out, getSpatialPolysDataFrameForOneImage)




#load the SBCC data and reprocess into a SpatialPoints object
#this is the same as Oneimage
caKelp <- readMat("./data/kelp_043035_7_24_1999.mat")
caKelp <- caKelp$kelp[which(caKelp$kelp[,3]>0),]

#this is slightly different - need the proj4string for an element of my list
#first put the cakelp spatial points into a dataframe
sp_classifications_df <- data.frame(matrix(ncol = 1, nrow = 12))

sp_classifications_df$SPDF <- sp_classifications_list

#the proj4string is the same for all of them, so I only need to do this once - is there a better way?

caKelp.spoints <- SpatialPoints(caKelp[,2:1], 
                                proj4string=CRS(proj4string(sp_classifications_df[[6,2]])))

#Now convert to a list and add it into the dataframe - might not even need it in there but there we go
sp_classifications_df$caKelp.spoints <- rep(list(caKelp.spoints), 12)



sp_classifications_df$caKelp.spoints <- crop(caKelp.spoints, extent(sp_classifications_df$SPDF))



################Here's where I do each image.
#one at a time for now
#replace the row number in sp_classifications_df[[i,2]] for each image
x <- 6

sp_classifications_df[[x,2]]

caKelp.spoints_crop <- crop(caKelp.spoints, extent(sp_classifications_df[[x,2]]))

#caKelp.spoints <- spTransform(caKelp.spoints, CRS("+init=epsg:3857"))

#get the bounds for plotting from the polygons data frame
longBounds <- bbox(sp_classifications_df[[x,2]])[1,]
latBounds <- bbox(sp_classifications_df[[x,2]])[2,]

#plot them together
plot(caKelp.spoints, xlim=longBounds, ylim=latBounds, pch=20, cex=0.5)
plot(sp_classifications_df[[x,2]], add=T)


######################

##ok, it looks like this will only work for the southern sites, starting at sp_classifications_df[[5,2]] and going to sp_classifications_df[[12,2]]

#lets proceed with calbrations

#forging ahead with AKP00016em

lims <- as.numeric(sp_classifications_df[[x,2]]@data[1,c(10,8,11,9)])
coasts <- readOGR("./data/GSHHS_shp/h", layer="GSHHS_h_L1",  useC=FALSE)


plot(coasts, xlim=lims[1:2], ylim=lims[3:4], lwd=4)
plot(sp_classifications_df[[x,2]], add=T)

tileBrick <- rasterizeFFImage(sp_classifications_df[[x,2]][1,])
plotRGB(tileBrick)
#plot(coasts, xlim=lims[1:2], ylim=lims[3:4], lwd=4, add=T)
plot(caKelp.spoints, xlim=longBounds, ylim=latBounds, pch=20, cex=0.5, 
     add=T, col=rgb(1,0,0,alpha=0.1))

plot(sp_classifications_df[[x,2]], add=T)



###################################
#rasterise!

r <- raster(crs=sp_classifications_df[[6,2]]@proj4string, ext=extent(sp_classifications_df[[x,2]]), ncols = 400, nrows=364)
rastLayer <- rasterize(SpatialPolygons(sp_classifications_df[[x,2]]@polygons), r, fun="count")


plotRGB(tileBrick)
plot(rastLayer,
     legend.args=list(text='# of Users\nSelecting',
                      side=3, font=7.2, line=0.5, cex=0.8), add=T)


longBounds <- bbox(rastLayer)[1,]
latBounds <- bbox(rastLayer)[2,]

#for the tight shot!
zoomLongBounds <- bbox(SpatialPolygons(sp_classifications_df[[x,2]]@polygons))[1,]
zoomLatBounds <- bbox(SpatialPolygons(sp_classifications_df[[x,2]]@polygons))[2,]

hasData <- which(!is.na(as.matrix(rastLayer)), arr.ind=TRUE)
rastZoomXBounds <- c(min(hasData[,2]), max(hasData[,2]))
rastZoomYBounds <- c(min(hasData[,1]), max(hasData[,1]))
zoomExtent <- extent(c(zoomLongBounds, zoomLatBounds))

#plot them together

plot(crop(rastLayer, extent(c(zoomLongBounds, zoomLatBounds))),
     legend.args=list(text='# of Users\nSelecting', 
                      side=3, font=2.2, line=0.5, cex=0.8)
)

plot(caKelp.spoints, xlim=zoomLongBounds, ylim=zoomLatBounds, pch=20, cex=0.5, add=T)

###########
caKelp.raster <- rasterize(caKelp.spoints, r, fun="count")


#See what this looks like
combined.raster <- overlay(caKelp.raster, rastLayer, fun="sum")

plot(crop(combined.raster, zoomExtent))
hist(as.matrix(combined.raster), breaks=1:16)
############
# Functions to caculate areas
#############
getKelpPixelsFromRaster <- function(rast, threshold=1){
  length(which(as.matrix(rast)>=threshold))
}

getKelpPixelsFromRasterV <- Vectorize(getKelpPixelsFromRaster, "threshold")

getUsers <- function(a,b) {
  if(is.na(a)) return(NA)
  b
}


kelpPixels <- data.frame(users=1:15, 
                         kelpPixels = getKelpPixelsFromRasterV(rastLayer, 1:15),
                         goodKelpPixels = getKelpPixelsFromRasterV(combined.raster, 1:15))
kelpPixels$kelpArea <- kelpPixels$kelpPixels*0.03*0.03

kelpPixels <- data.frame(users=1:15, 
                         kelpPixels = getKelpPixelsFromRasterV(rastLayer, 1:15))
kelpPixels$kelpArea <- kelpPixels$kelpPixels*0.03*0.03


plot(goodKelpPixels ~ kelpPixels, data=kelpPixels, type="n")
text(kelpPixels$kelpPixels, kelpPixels$goodKelpPixels, labels=1:15)
plot(goodKelpPixels ~ users, data=kelpPixels)


plot(kelpPixels ~ users, data=kelpPixels)
abline(h=getKelpPixelsFromRaster(caKelp.raster))

###S
library(ggplot2)
qplot(users, kelpPixels, data=kelpPixels, geom=c("point"), size=I(10)) +
  xlab("\nMinimum # of Users Selecting a Pixel") + ylab("Total Pixels of Kelp\n") +
  geom_abline(intercept=getKelpPixelsFromRaster(caKelp.raster), lwd=2, lty=2, col="red") +
  theme_bw(base_size=24) +
  ylim(c(0,4000))



qplot(users, kelpArea, data=kelpPixels, geom=c("point"), size=I(10)) +
  xlab("# of Users Selecting a 30 x 30m Pixel") + ylab("Total Square Km. of Kelp\n") +
  geom_abline(intercept=0.03*0.03*getKelpPixelsFromRaster(caKelp.raster), slope=0, lwd=2, lty=2, col="red") +   theme_bw(base_size=24) +
  scale_y_continuous(breaks=c(0,2,4,6,8,12)) +
   theme(text = element_text(size=30))


#Maybe a binomial model
goodIDX <- which(!is.na(as.matrix(caKelp.raster)))


calDF <- data.frame(calibration = as.vector(as.matrix(caKelp.raster)), 
                    FF = as.vector(as.matrix(rastLayer)))

calDF[which(is.na(calDF[,1])),1] <- 0
calDF[,1] <- as.numeric(calDF[,1]>0)
calDF[which(is.na(calDF[,2])),2] <- 0
#Only things that were kelp in either the calibration of FF set
calDF <- calDF[-which(rowSums(calDF)==0),]

calGLM <- glm(calibration ~ FF, data=calDF, family=binomial)
summary(calGLM)

ggplot(data=calDF, mapping=aes(x=FF, y=calibration)) +
  geom_point(alpha=0) +
  #stat_smooth(method=glm, family="binomial", color="red", lwd=2) +
  geom_jitter(position = position_jitter(width = .5, height=0.01)) +
  theme_bw(base_size=24) +
  xlab("\n# of People Selecting Pixel") + ylab("Pixel Included in Calibration Set\n(1=yes, 0=no)\n")

cdfReduced <- calDF %>%
  group_by(FF, calibration) %>%
  summarise(`Number of Pixels`=n())

qplot(FF, calibration, size=`Number of Pixels`, data=cdfReduced) +
  theme_bw(base_size=24) +
  xlab("\n# of People Selecting Pixel") + ylab("Pixel Included in Calibration Set\n(1=yes, 0=no)\n")






