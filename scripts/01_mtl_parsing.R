library(tidyr)
library(dplyr)
#create a table of scenes
scenes <- as.data.frame(list.files("./Floating-Forests/data/all_MTLs"))
colnames(scenes) <- "scene"
scenes$scene <- as.character(scenes$scene)

#make a column with filename/path to feed into function
pre <- "./Floating-Forests/data/all_MTLs/"

#read in all data from all mtls
corner_parse <- function(scene){
  coords <- as.data.frame(read.table(file = paste0(pre,scene), fill	
 = T))
  coords <- filter(coords, 
                     V1 == "CORNER_UL_LAT_PRODUCT"|
                     V1 == "CORNER_UL_LON_PRODUCT"|
                     V1 == "CORNER_UR_LAT_PRODUCT"|
                     V1 == "CORNER_UR_LON_PRODUCT"|
                     V1 == "CORNER_LL_LAT_PRODUCT"|
                     V1 == "CORNER_LL_LON_PRODUCT"|
                     V1 == "CORNER_LR_LAT_PRODUCT"|
                     V1 == "CORNER_LR_LON_PRODUCT"|
                     V1 == "CORNER_UL_PROJECTION_X_PRODUCT"|
                     V1 == "CORNER_UL_PROJECTION_Y_PRODUCT"|
                     V1 == "CORNER_UR_PROJECTION_X_PRODUCT"|
                     V1 == "CORNER_UR_PROJECTION_Y_PRODUCT"|
                     V1 == "CORNER_LL_PROJECTION_X_PRODUCT"|
                     V1 == "CORNER_LL_PROJECTION_Y_PRODUCT"|
                     V1 == "CORNER_LR_PROJECTION_X_PRODUCT"|
                     V1 == "CORNER_LR_PROJECTION_Y_PRODUCT"|
                     V1 == "PRODUCT_UL_CORNER_LAT"|
                     V1 == "PRODUCT_UL_CORNER_LON"|
                     V1 == "PRODUCT_UR_CORNER_LAT"|
                     V1 == "PRODUCT_UR_CORNER_LON"|
                     V1 == "PRODUCT_LL_CORNER_LAT"|
                     V1 == "PRODUCT_LL_CORNER_LON"|
                     V1 == "PRODUCT_LR_CORNER_LAT"| 
                     V1 == "PRODUCT_LR_CORNER_LON"|
                     V1 == "PRODUCT_UL_CORNER_MAPX"|
                     V1 == "PRODUCT_UL_CORNER_MAPY"|
                     V1 == "PRODUCT_UR_CORNER_MAPX"| 
                     V1 == "PRODUCT_UR_CORNER_MAPY"|
                     V1 == "PRODUCT_LL_CORNER_MAPX"| 
                     V1 == "PRODUCT_LL_CORNER_MAPY"| 
                     V1 == "PRODUCT_LR_CORNER_MAPX"| 
                     V1 == "PRODUCT_LR_CORNER_MAPY")
  coords <- coords[,3]
}



#run it on all rows and rename columns to be meaningful
parsed_coords <- as.data.frame(lapply(scenes$scene, corner_parse))
parsed_coords <- as.data.frame(t(parsed_coords))
colnames(parsed_coords) <- c("CORNER_UL_LAT_PRODUCT",
                      "CORNER_UL_LON_PRODUCT",
                      "CORNER_UR_LAT_PRODUCT",
                      "CORNER_UR_LON_PRODUCT",
                      "CORNER_LL_LAT_PRODUCT",
                      "CORNER_LL_LON_PRODUCT",
                      "CORNER_LR_LAT_PRODUCT",
                      "CORNER_LR_LON_PRODUCT",
                      "CORNER_UL_PROJECTION_X_PRODUCT",
                      "CORNER_UL_PROJECTION_Y_PRODUCT",
                      "CORNER_UR_PROJECTION_X_PRODUCT",
                      "CORNER_UR_PROJECTION_Y_PRODUCT",
                      "CORNER_LL_PROJECTION_X_PRODUCT",
                      "CORNER_LL_PROJECTION_Y_PRODUCT",
                      "CORNER_LR_PROJECTION_X_PRODUCT",
                      "CORNER_LR_PROJECTION_Y_PRODUCT")


correct_coords_from_MTLs <- cbind(scenes, parsed_coords)




#now join to subjects.
#as of 6/9/2017 mongosubjectsfix.csv is still the main subject list, but has bad meta corners and row/cols on it.
#here we will join correct scenes

correct_coords_from_MTLs <- read_csv("../data/products/correct_coords_from_MTLs.csv")


scenes_trimmed <- as.data.frame(str_sub(scenes$scene, end = -9))
correct_coords_from_MTLs <- cbind(scenes_trimmed, correct_coords_from_MTLs)
colnames(correct_coords_from_MTLs)[1] <- "scene_trimmed"
####################
write.csv(correct_coords_from_MTLs, "./Floating-Forests/data/products/correct_coords_from_MTLs.csv")
####################

subjects <- read_csv("./Floating-Forests/data/mongosubjectsfix.csv")
subject_scenes_trimmed <- as.data.frame(str_sub(subjects$metadata.file , end = -8))
subjects <- cbind(subject_scenes_trimmed, subjects)
colnames(subjects)[1] <- "scene_trimmed"

#make a new set of columns that are complete row/cols
#these will be joined with my lookup table to stick on the correct row/col, and then get the right metadata corners stuck on

subjects <- subjects %>%
  mutate("bad_row"= paste0(metadata.row_no, metadata.row))
subjects <- subjects %>%
  mutate("bad_col"= paste0(metadata.col_no, metadata.col))
subjects <- subjects %>%
  mutate(bad_rows = gsub("NA", "", bad_row))
subjects <- subjects %>%
  mutate(bad_cols = gsub("NA", "", bad_col))
subjects$bad_rows <- as.numeric(subjects$bad_rows) +1
subjects$bad_cols <- as.numeric(subjects$bad_cols) +1
subjects <- subjects %>%
  select(-bad_row, -bad_col)
subjects <- subjects %>%
  unite("row_col",c(bad_rows, bad_cols), remove = FALSE)
#clean up subjects to free up RAM
subjects <- subjects %>%
  dplyr::select(-metadata.source_rows, -metadata.source_cols, -random, -project_id, -workflow_ids)



write.csv(subjects, "./Floating-Forests/data/products/subjects_from_MTLs.csv")
#this should now have the cleaned up scene IDs (in order to join to correct coordinates) as well as fixed row/col columns (previously there were 2 half finished ones)


subhead <- head(correct_coords_from_MTLs)
