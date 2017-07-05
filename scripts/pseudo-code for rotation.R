###tracking down row/cols and if they start at 0 or 1
#If you rotate the image matrix by -90 degrees row 8 column 15 becomes row 15, column 13. 
#If their loops start at 0 (matlab starts at 1) this would make it row 14, column 12 (FF position).

imagelookup <- filter(correct_subjects, as.character(zooniverse_id) == "AKP00016e6")

#ok. looks like the ORIGINAL COLUMN from metadata starts at 0, but the updated "bad_rows" and "bad_cols"columns start at 1
#IE the form it is usable in is 15, 13. SO, whatever transformation we do needs to go from 15, 13 to 8, 15
#we want to have everything run 1-20

#right now, grid_compare is actually matching up correctly.
#Tuesday to do:
#the actual join - looks good for the example
write.csv(correct_subjects, "./data/products/full_subjects.csv")

#verify through slicing an actual scene
#check another one