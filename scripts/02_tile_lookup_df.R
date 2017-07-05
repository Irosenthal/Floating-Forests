library(dplyr)
library(readr)
library(tidyr)
setwd("C:/Users/irose/OneDrive/FF/Floating-Forests")

rotate <- function(x) t(apply(x, 2, rev))
#make tile IDs
x<-c(1:20)
y<-c(1:20)
#each row col combo
mat_test <- expand.grid(x = x,y = y)

#now each gets a unique row/col (might be obsolete)
mat_test <- mat_test %>%
  unite("x,y", x, y,  sep = ",", remove = FALSE)

#each row/col combo gets a tile #. I will use this to assign these to actual FF data
tile <- c(1:nrow(mat_test))
mat_test <- cbind(mat_test, tile)
#############################
#convert this to a matrix
ID <- matrix(mat_test$tile, 20)
ID



matr <- matrix(c(1:20), 20, 20)


true_tiles_rows <- matrix(c(1:20), 20, 20) 
true_tiles_cols <- t(true_tiles_rows)


ff_tiles_rows <- rotate(rotate(rotate(true_tiles_rows)))
ff_tiles_cols <-- rotate(rotate(rotate(true_tiles_cols)))

true_row_vec <- as.vector(true_tiles_rows)
true_col_vec <- as.vector(true_tiles_cols)
ff_row_vec <- rev(as.vector(ff_tiles_rows))
ff_col_vec <- rev(as.vector(abs(ff_tiles_cols)))


#grid_compare <- data.frame(true_row = true_row_vec, true_col = true_col_vec, bad_rows_from_meta = ff_row_vec, bad_cols_from_meta = ff_col_vec)
#the previous ended up backwords. I dont understand why right now, but the following works properly.

grid_compare <- data.frame(bad_rows_from_meta = true_row_vec, bad_cols_from_meta = true_col_vec,  true_row = ff_row_vec,  true_col = ff_col_vec)

####
#Match these to the subjects that we have left in the system

subjects <- read_csv("../data/products/subjects_from_MTLs.csv")
sub_check <- head(subjects)
#add 1 to get rid of zeros

subjects$row <- as.numeric(subjects$row)
subjects$col <- as.numeric(subjects$col)

subjects <- subjects %>%
  mutate(adjusted_row = row + 1) %>%
  mutate(adjusted_col = col + 1)


subjects <- subjects %>%
  unite("row_col", c(adjusted_row, adjusted_col), sep = "_", remove = FALSE)

grid_compare <- grid_compare %>%
  unite("row_col", c(bad_rows_from_meta, bad_cols_from_meta), sep = "_", remove = FALSE)

write.csv(grid_compare, "./data/products/grid_compare.csv")

subjects <- as.data.frame(subjects)
grid_compare <- as.data.frame(grid_compare)

correct_subjects <- left_join(subjects, grid_compare, by = "row_col")

correct_subjects <- correct_subjects %>%
  mutate(bad_row = adjusted_row) %>%
  mutate(bad_col = adjusted_col) %>%
  dplyr::select(c(-row, -col, -row_col, -bad_rows_from_meta, -bad_cols_from_meta, -adjusted_row, -adjusted_col))

write.csv(correct_subjects, "remaining_kelp_subjects_correct_row_col")


#join correct tile corners to correct metadata

corrected_tiles <- read_csv("../data/products/full_subjects.csv")
correct_coords_from_MTLs <- read_csv("../data/products/correct_coords_from_MTLs.csv")

#prepare a column to join by
corrected_tiles <- corrected_tiles %>%
  mutate(scene_ID = gsub(".tar.gz", "", metadata.file))

correct_coords_from_MTLs <- correct_coords_from_MTLs %>%
  mutate(scene_ID = gsub("_MTL.txt", "", scene))


full_subjects <- left_join(corrected_tiles, correct_coords_from_MTLs, by = "scene_ID" )
write.csv(full_subjects, "full_subjects_row_col.csv")

#create test subject table for quick testing

subjects <- read_csv("../data/products/full_subjects_row_col.csv")
  
test_subjects <- subjects %>%
  filter(zooniverse_id == "AKP0000001"|
        zooniverse_id == "AKP0000002"|
        zooniverse_id == "AKP000001k"|
        zooniverse_id == "AKP000001n"
           )

write.csv(test_subjects, "../data/products/test_subjects.csv" )


subhead <- head(subjects)
sub_corn <- read_csv("../data/products/subjects_from_MTLs.csv" )
  )

subcornhead <- head(sub_corn)
