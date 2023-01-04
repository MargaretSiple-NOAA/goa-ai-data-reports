# GOA/AI DATA REPORT - use this to get all the values you need for the report
# THIS SCRIPT TAKES A LONG TIME TO RUN BECAUSE IT BUILDS ALL THE REPORT CONTENTS!

# Get report settings -----------------------------------------------------
source("R/00_report_settings.R")

# Functions, packages, directories ---------------------------------------------
source("R/01_directories.R")
source("R/02_load_packages.R") 

# Get data from RACEBASE and AI/GOA schemas ------------------------------------
x <- askYesNo(msg = "Do you want to download local versions of RACEBASE tables now? Skip this if you already have updated local tables.")
if (x) {
  source("R/03_download_data_from_oracle.R")
}

# Get text from Google Drive ----------------------------------------------
y <- askYesNo(msg = "Do you want to re-download Google Drive files now?")
if (y) {
  source("R/04_get_gdrive_chapters.R")
}

# Functions and data prep -------------------------------------------------
source("R/05_functions.R") # May not need all these functions.
source("R/06_prep_data.R") # Make all the tables and stuff needed for report and pres.

# Create tables and figures -----------------------------------------------
# Do you want to build all the tables right now? NOTE: This may take a while.
source(here::here("R", "07_make_tabs.R"))

# Run this if you want to build all the figures right now? NOTE: This may take a while.
z <- askYesNo(msg = "Do you want to create all the figures right now? Note: this may take a while.")
if(z) {source(here::here("R", "08_make_figures.R"))}


# Save all the values needed to make the report so you can easily load later --
# clean up global env before saving. Can add to this when I find more stuff that we don't actually need in the report Markdown file.
rm(list = c(
  "ai_central", "ai_east", "ai_west", "bartheme", "bubbletheme", "catch", "fig", "haul",
  "joyplot", "L", "length2", "length3", "linetheme", "list_cpue_bubbles", "list_joy_length", 
  "list_tables", "S", "table3s_list", "table4s_list"
))

save(list = ls(), file = paste0(dir_out_todaysrun, "reportvalues.rdata"))

