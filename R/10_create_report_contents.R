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
# x <- askYesNo(msg = "Do you want to download local versions of RACEBASE tables now? Skip this if you already have updated local tables.")
x <- TRUE
if (x) {
  source("R/05_download_data_from_oracle.R")
}

# Functions and data prep -------------------------------------------------
source("R/06_prep_data.R") # Make all the tables and stuff needed for report and pres.
source("R/06a_appendix_b.R") # Make the table for Appendix B (also used in main text for species richness summary) - ignore warnings.

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


# Save all the values needed to make the report so you can easily load later --
# clean up global env before saving. Can add to this when I find more stuff that we don't actually need in the report Markdown file.
# The items present in this list are either intermediary (i.e., not objects used in the final report, like ai_central) or they are saved as rdata objects separately (e.g., list_cpue_bubbles)
if(SRVY=="AI"){ai_map_layers <- c("ai_central", "ai_east", "ai_west")}else{ai_map_layers <- NULL}

rm(list = c(
  ai_map_layers, "bartheme", "bubbletheme", "catch", "fig", "haul",
  "joyplot", "L2", "L3", "linetheme", 
  "list_cpue_bubbles", "list_joy_length","list_temperature", "list_ldscatter",
  "list_tables", "S", "table3s_list", "table4s_list",  "tabledate", 
  "figuredate", "reportvaluesdate"
))

save(list = ls(), file = paste0(dir_out_todaysrun, "reportvalues.rdata"))
