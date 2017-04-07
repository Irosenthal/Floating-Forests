##########
##workflow
###########
#take the full subjects table (1 row per subject)
#group by scene
#create the corners of each subject and then join them to the appropriate subjects. have tried do() and nesting, 
#I don't think issues here are caused by either choice.
#ultimate goal is for each scene, create the corner cordinate grid and attach correct cooridnates to each subject
#the code used within the function works on a one scene at a time basis, issues are due to the scale-up.
################
library(dplyr)
library(tidyr)
test_subjects <- read.csv("./test_subjects.csv")

#arow comes from test_subjects.csv
scene_grid <- function(arow){ #also thought about doing this by scene
  LLX <- unlist(test_subjects[arow,'CORNER_LL_PROJECTION_X_PRODUCT'])
  LRX <- unlist(test_subjects[arow,'CORNER_LR_PROJECTION_X_PRODUCT'])
  LLY <- unlist(test_subjects[arow,'CORNER_LL_PROJECTION_Y_PRODUCT'])
  ULY <- unlist(test_subjects[arow,'CORNER_UL_PROJECTION_Y_PRODUCT'])
  
  
  lonlatpix <-expand.grid(x = seq(as.numeric(LLX), as.numeric(LRX), by = 30),  
                          y = seq(as.numeric(ULY), as.numeric(LLY), by = -30))
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
  
  lastlon <- lonpix[nrow(lonpix),]
  Col <- nrow(longridindex) +1 
  lonbind <- cbind(lastlon, Col)
  longridindex <- rbind(longridindex,lonbind)
  
  
  lastlat <- latpix[nrow(latpix),]
  Row <- nrow(latgridindex) +1 
  latbind <- cbind(lastlat, Row)
  latgridindex <- rbind(latgridindex,latbind)
  
  #indexing reminder: UTM coords are in first column of dataframe
  #pull out important points 
  upper_left_x_utm <- longridindex[Col, 1]
  upper_left_y_utm <- latgridindex[Row, 1]
  lower_left_x_utm <- longridindex[Col, 1]
  lower_left_y_utm <- latgridindex[Row + 1, 1]
  upper_right_x_utm <- longridindex[Col + 1, 1]
  upper_right_y_utm <- latgridindex[Row, 1]
  lower_right_x_utm <-longridindex[Col + 1, 1]
  lower_right_y_utm <- latgridindex[Row + 1, 1]
  center_x_utm <- (upper_left_x_utm + ((upper_right_x_utm - upper_left_x_utm)/2))
  center_y_utm <- (lower_left_y_utm + ((upper_left_y_utm - lower_left_y_utm)/2))
  
  row_col <- paste(Row, Col, sep="_") 
  
  #this might need tweaking, but should at least run
  subjects_corners$scene_corners <- data.frame(upper_left_x_utm, upper_left_y_utm, 
                                            lower_left_x_utm, lower_left_y_utm, 
                                            upper_right_x_utm, upper_right_y_utm, 
                                            lower_right_x_utm, lower_right_y_utm,
                                            center_x_utm, center_y_utm, row_col)
  
  
  #This needs to only join within one scene. better to do that with groups outside of the function, or 
  #just include scene in the join to make each set of row_cols unique?
  test_subjects_with_corners <- left_join(test_subjects, subjects_corners, by = "row_col") 
  
}



#group by scene and do across the entire df
test_subjects_do <- test_subjects %>% 
  group_by(scene) %>% 
  do(scene_grid(.))
