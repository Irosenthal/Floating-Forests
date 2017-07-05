library(raster)



coast_tif <- brick("../data/scenes/LC80400372013110LGN01_B6.tif")
coast_tif_r <- raster("../scenes/LC80400372013142LGN01/LC80400372013142LGN01_B1.tif")
coast_tif_r
plotRGB(coast_tif)

sr <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")


#crs(coast_tif_r) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"



#reproject to lonlat
coast_lonlat_fix <- projectRaster(coast_tif_r, sr)
write.raster(coast_lonlat, "./projected_scene")


plotRGB(coast_lonlat)
plotRGB(coast_tif)


proj4string(coast_tif) <- sr

coast_tif_r

#reproject to longlat from merc
source("./FFmethods_fix.R")

imageInfo <- getImageInfo("AKP0000001")
tileBrick <- rasterizeFFImage(oneImage[1,],
                             proj="+proj=utm +zone=11 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")


#crop to same size
scenecrop <- crop(coast_tif, extent(395940,407910,3727050,3737940))
coastCrop <- crop(coasts, extent(tileBrick))
kelp_crop <- crop(caKelp.spoints, extent(tileBrick))


plot(scenecrop)
plotRGB(scenecrop)



plotRGB(tileBrick)
plot(coasts, add=T, lwd=4)
plot(caKelp.spoints, add=T)





