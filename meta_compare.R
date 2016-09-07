library(dplyr)
library(tidyr)
setwd("C:/Users/irose/OneDrive/FF/Floating-Forests")

#load metadata file
meta <- read.csv("landsat_scenes_filtered_metadata_with_UTM.csv")

#load subjects table 
mongosubs <- read.csv("mongosubjectsfix.csv")

#create column in subjects table to clean up sceneID


subjects_cleaner <- mutate(mongosubs, clean = metadata.file)

#now convert this to a character
subjects_cleaner$clean <- as.character(subjects_cleaner$clean)

subjects_clean <- subjects_cleaner

#and remove.tar.gz

subjects_clean$clean <- as.character(gsub(".tar.gz", "", subjects_cleaner$clean))

#rename to clean to sceneID to prep for join

colnames(subjects_clean)[colnames(subjects_clean)=="clean"] <- "sceneID"

#make them both characters for join

subjects_clean$sceneID <- as.character(subjects_clean$sceneID)


meta$sceneID <- as.character(meta$sceneID)



# now join the tables

antisubmeta <- anti_join(subjects_clean, meta, by  = "sceneID")
submetaraw <- left_join(subjects_clean, meta, by  = "sceneID")



#row and cols start from 0, need to add 1 to each
submeta <- submetaraw %>%
  mutate(FFrow = metadata.row_no +2) %>%
  mutate(FFcol = metadata.col_no +2)


# now choose 1 scene and add correct grid coords. start with image AKP00016e6 because we have used it before

imagelookup <- filter(scene_with_FF_UTM, as.character(zooniverse_id) == "AKP00016e6")
scenelookup <- filter(submeta, sceneID == "LE70440351999204EDC01")
rowlookup <- filter(scenelookup, as.character(metadata.row_no) == "2")

#should show that it is part of scene LE70440351999204EDC01
#now create a table for scene LE70440351999204EDC01 which will include sceneID and 
#row/column numbers for each image, with their correct corners

scenetestrough <- filter(submeta, as.character(sceneID) == "LE70440351999204EDC01")

#####testing with a tas site LT50900901994088ASA00

#scenetestrough <- filter(submeta, as.character(sceneID) == "LT50900901994088ASA00")

scenetestx <- left_join(scenetestrough, corners_x, by = "metadata.col_no")

scenetest <- left_join(scenetestx, corners_y, by = "metadata.row_no")


#eliminated unnecessary columns
scenetest_tidy <- scenetest %>%
  select(sceneID, zooniverse_id, metadata.row_no, metadata.col_no, UTMzone, upper_left_x, upper_left_y, 
         upper_right_x, upper_right_y, lower_left_x, lower_left_y, lower_right_x, lower_right_y, metadata.file)


#to find one image with the correct coords

coordlookup <- filter(scenetest_tidy, as.character(zooniverse_id) == "AKP00016e6")


