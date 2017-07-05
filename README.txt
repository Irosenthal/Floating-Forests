mtl_parsing.R: take folder of MTLs and pull out relavent corners. 
tile_rotation.R: OLD METHOD matrix rotations to fix tile IDs - see tile_lookup_df.R for updated method
tile_lookup_df.R: rotate ff tile designations to correct ones, creates grid_compare.csv
automated_corrections: adds correct tiles to subjects table, creates corrected_subjects_full.csv
classification_fix.R: creates an updated classification table based on corrected subject spatial data - does once per scene, TO DO: automate for all scenes at once.
density_plot.R: creates density plot of optimal user #
spatial_polys_nest.R: adds classifications to subjects and creates spatial dataframes for plotting and validations
01_ll2utm.R: OLD METHOD converts lon/lat to utm. 
meta_compare.R:OLD joins metadata to subject table to find missing metadata. since switch to MTL parses, this is depriciated and has been moved to "old_code"
corner_coords.R: OLD but still should be correct, stepwise calculations of subject corners
corner_calc_function.R: OLD but still should be correct, function to calculate subject corners for one scene
FFmethods_fix.R: updated FFmethods file, MAKE SURE TO ADJUST ALL PROJECTIONS PROPERLY WHEN SWITCHING UTM ZONES
tryToFixOneImageProjectionfix.R: for testing only, allows plotting of one subject & its classifications at a time

up to date products & intermediates are found in Floating-Forests/data/products
subjects_from_MTLs.csv is the master subjects file. It has scene corners taken directly from MTLs, and has the row_col column for joining later. UPDATE: moved to data folder
UPDATE: full_subjects.csv, created in pseudo-code for rotation, saved in products folder, is the most up to date and has fixed row/cols for each subject.
UPDATE: full_subjects.csv moved to data, current master subjects is full_subjects_row_col.csv in products folder