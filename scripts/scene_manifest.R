library(readr)
library(dplyr)
library(tidyr)
library(stringr)


metadata <- as.data.frame(list.files("./MTL_round3"))
metadata <- as.data.frame(gsub("_MTL.txt", "", metadata[,1]))
colnames(metadata) <- "file"

#renamed some of the older CA MTL's that were collected before USGS
#changed the naming format. I put these in the correct format, but they are missing the 3 letters near the end.
#So, only going to use sceneID/date/DOY to do comparison - last bit is not correct and would miss some scenes

metadata_trimmed <- as.data.frame(str_sub(metadata$file, end = -6))
colnames(metadata_trimmed) <- "file"



all_scenes <- read_csv("./all_scenes.csv")
all_scenes_trimmed <- as.data.frame(str_sub(all_scenes$file, end = -6))
colnames(all_scenes_trimmed) <- "file"



#this generates the bad list, just keeping to compare as i test new code
missing_scenes <- anti_join(all_scenes_trimmed, metadata_trimmed, by = "file")

#this should create a correct list.
missing_scenes_trimmed <- left_join(all_scenes_trimmed, metadata_trimmed, by = "file")



write_csv(missing_scenes_trimmed, ("./missing_scenes.csv"))

missing_scenes <- read_csv("./missing_scenes_full_filenames.csv")
colnames(missing_scenes) <- "meta_file"
missing_trimmed <- as.data.frame(str_sub(missing_scenes$meta_file, end = -6))
colnames(missing_trimmed) <- "file"
missing_trimmed <- (cbind(missing_scenes, missing_trimmed))


missing_scenes_trimmed_more <- anti_join(missing_trimmed, metadata_trimmed, by = "file")
missing_scenes_trimmed_more <- missing_scenes_trimmed_more %>%
  dplyr::select(-file)

write_csv(missing_scenes_trimmed_more, ("./missing_scenes_update.csv"))
####narrowing in on last 4 scenes
#load in updated missing scene list

missing_scenes <- read_csv("./missing_scenes_update.csv")

missing_scenes_trimmed <- as.data.frame(str_sub(missing_scenes$meta_file, end = -6))
colnames(missing_scenes_trimmed) <- "file"
missing_scenes_trimmed <- cbind(missing_scenes_trimmed, missing_scenes)

#the join
remaining_scenes <- anti_join(missing_scenes_trimmed, metadata_trimmed, by = "file")

#search all_scenes for the ones we need

remaining_scenes[,2]

