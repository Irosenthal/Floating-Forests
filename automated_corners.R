
full_subjects <- read_csv("./../full_subjects.csv")

no_rows  <-  20
no_colls <-  20
image_width  <- 7981
image_height <-  7271
image_chunk_width <- image_width / no_rows #364
image_chunk_height <- image_height / no_colls #399
######################
#group by scene, get all corners, join to original df and re-expand
#comp cheap, high memory - for each scene make a df that is scene, row, col, all corners, join. one df all row cols all scenes

subject_corners <- function(scene){
  #make a grid of latlongs by 30 meters using corners of scene as start/end points
  lonlatpix <-expand.grid(x = seq(full_subjects$CORNER_LL_PROJECTION_X_PRODUCT, full_subjects$CORNER_LR_PROJECTION_X_PRODUCT, by = 30),  
                          y = seq(full_subjects$CORNER_LL_PROJECTION_Y_PRODUCT, full_subjects$CORNER_UL_PROJECTION_Y_PRODUCT, by = -30))
  
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
  
  test_subjects$scene_corners <- data.frame(upper_left_x_utm, upper_left_y_utm, 
                                       lower_left_x_utm, lower_left_y_utm, 
                                       upper_right_x_utm, upper_right_y_utm, 
                                       lower_right_x_utm, lower_right_y_utm,
                                       center_x_utm, center_y_utm)
  
}




#make a test df, 2 scenes, 2 subjects per secene

LC80400372013110LGN01
LC80400372013254LGN00

test_subjects <- full_subjects[1:100,]


test_subjects <- test_subjects %>%
  filter(zooniverse_id == "AKP000001k"|
          zooniverse_id == "AKP000001n"|
          zooniverse_id == "AKP0000001"|
          zooniverse_id == "AKP0000002")

test_subjects <- test_subjects %>%
  dplyr::select(c(-activated_at, -classification_count, -coords, -created_at, -group.name
         -group.zooniverse_id, -group._id, -group_id, -location.standard, -metadata.base_file_name, 
         -metadata.file, -metadata.timestamp, -metadata.row_no, -metadata.col_no,
         -metadata.lower_left1, -metadata.lower_left2, -metadata.upper_right1, -metadata.upper_right2,
         -updated_at, -metadata.marking_count))
###################################################################

scene_grid <- function(arow){
LLX <- unlist(test_subjects[arow,'CORNER_LL_PROJECTION_X_PRODUCT'])
LRX <- unlist(test_subjects[arow,'CORNER_LR_PROJECTION_X_PRODUCT'])
LLY <- unlist(test_subjects[arow,'CORNER_LL_PROJECTION_Y_PRODUCT'])
ULY <- unlist(test_subjects[arow,'CORNER_UL_PROJECTION_Y_PRODUCT'])


lonlatpix <-expand.grid(x = seq(as.numeric(LLX), as.numeric(LRX), by = 30),  
                          y = seq(as.numeric(LLY), as.numeric(ULY), by = -30))
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
  
  test_subjects$scene_corners <- data.frame(upper_left_x_utm, upper_left_y_utm, 
                                       lower_left_x_utm, lower_left_y_utm, 
                                       upper_right_x_utm, upper_right_y_utm, 
                                       lower_right_x_utm, lower_right_y_utm,
                                       center_x_utm, center_y_utm)
  
}


test_subjects_do <- test_subjects %>% 
  group_by(scene) %>% 
  do(scene_grid(.))

#just in case
#lonlatpix <-expand.grid(x = seq(test_subjects$CORNER_LL_PROJECTION_X_PRODUCT, test_subjects$CORNER_LR_PROJECTION_X_PRODUCT, by = 30),  
                       # y = seq(test_subjects$CORNER_LL_PROJECTION_Y_PRODUCT, test_subjects$CORNER_UL_PROJECTION_Y_PRODUCT, by = -30))


LLX <- unlist(test_subjects[1,'CORNER_LL_PROJECTION_X_PRODUCT'])
LRX <- unlist(test_subjects[1,'CORNER_LR_PROJECTION_X_PRODUCT'])
LLY <- unlist(test_subjects[1,'CORNER_LL_PROJECTION_Y_PRODUCT'])
ULY <- unlist(test_subjects[1,'CORNER_UL_PROJECTION_Y_PRODUCT'])


lonlatpix <-expand.grid(x = seq(LLX, LRX, by = 30),  
                        y = seq(LLY, ULY, by = 30))
