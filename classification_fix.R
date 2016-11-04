####add correct coordinates into the kelp classification table

classifications <- read_csv("./2016-05-29_kelp_classifications.csv")
corrected_tiles_tidy <- read_csv("./corrected_tiles_tidy.csv")
#get rid of bad coords

classifications$upper_right_x <- NULL 
classifications $upper_right_y <- NULL
classifications$lower_left_x <- NULL
classifications$lower_left_y <- NULL

#join good coords back on

classifications <- left_join(classifications, corrected_tiles_tidy, by = c("subject_zooniverse_id"="zooniverse_id"))


#now get rid of everything that isn't part of the scene 
rm_na <- function(data, desiredCols) {
  completeVec <- complete.cases(data[, desiredCols])
  return(data[completeVec, ])
}

classifications <- rm_na(classifications, "upper_right_x")


write_csv(classifications, "./classifications_correct_cords_1_scene.csv")
