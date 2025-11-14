# GOA/AI DATA REPORT - use this to get all the values you need for the report
# THIS SCRIPT TAKES A LONG TIME TO RUN BECAUSE IT BUILDS ALL THE REPORT CONTENTS!

# Get report settings -----------------------------------------------------
source("R/00_report_settings.R")

# Functions, packages, directories ---------------------------------------------
source("R/01_directories.R")
source("R/02_load_packages.R")
source("R/03_functions.R") # May not need all these functions.

# Get text from Google Drive --------------------------------------------------
y <- TRUE
if (y) {
  source("R/04_get_gdrive_chapters.R")
}

# Get data from RACEBASE and AI/GOA schemas ------------------------------------
x <- TRUE
if (x) {
  source("R/05_download_data_from_oracle.R")
}

# Functions and data prep -------------------------------------------------
source("R/06_prep_data.R") # Make all the tables and stuff needed for report and pres. Includes internal lookup tables. Uses Oracle again because gapindex is used to create material for the complexes.
source("R/06b_appendix_b.R") # Make the table for Appendix B (also used in main text for species richness summary) - ignore warnings.

# Create tables and figures -----------------------------------------------
# Each of these source() lines will save a list of outputs as an rdata file for later. And it will print out png's for figs and csvs for tables to output/
# Do you want to build all the tables right now?
source(here::here("R", "07_make_tabs.R"))

# Run this if you want to build all the figures right now? NOTE: This may take a while.
# z <- askYesNo(msg = "Do you want to create all the figures right now? Note: this may take a while.")
z <- TRUE
if (z) {
  source(here::here("R", "08_make_figures.R"))
}

# Clean up environment before saving just the values (highest_elasmos, etc) --------------
# Figures and printed tables
rm(list = c(
  "list_tables",
  "top_CPUE",
  "table3s_list",
  "table4s_list",
  "list_cpue_bubbles_strata",
  "list_joy_length",
  "list_ldscatter",
  "list_temperature"
))

# Make a table of all the objects in the environment and sort them by size to make sure we are removing the biggest ones! Time/space saver!
sizes <- vector()
for (i in ls()) {
  sizes[i] <- object.size(get(i))
}
sort(sizes, decreasing = TRUE)


rm(list = c(
  "x", "cpue_raw", "catch", "S",
  "biomass_gp", "catch_haul",
  "reg_dat_ai", "reg_data",
  "haul", "all",
  "complexes_data",
  "sizecomp_subareas_complexes",
  "length_maxyr_ldscatter",
  "mod1",
  "ai_central", "ai_east", "ai_west",
  "surface_temp_plot", "bottom_temp_plot",
  "length_maxyr_species",
  "p2", "plotdat", "biomass_total",
  "biomass_total_filtered"
))

# Save all the values needed to make the report so you can easily load later --
# The items present in this list are either intermediary (i.e., not objects used in the final report, like ai_central) or they are saved as rdata objects separately (e.g., list_cpue_bubbles)
save(list = ls(), file = paste0(dir_out_srvy_yr, "reportvalues.rdata"))
