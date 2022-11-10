#quartosetup.R

# Report settings -------------------------------------------------------------
usePNGPDF <- "png"
maxyr <- 2022 # Change this for the year!
compareyr <- 2016 # Change this for the year!
print_figs <- FALSE # Do you want to print out PNGs of each figure?
pres_or_report <- "report" # if "pres" the scripts will only make a subset of the figures and tables, the ones that we show for the GPT meeting

# When did you save the last version of the figures and tables you want to use?
tabledate <- "2022-11-03"
figuredate <- "2022-11-07"

# Survey information ------------------------------------------------------
survnumber <- "sixteenth" 
dates_conducted <- "June 6th through August 14th, 2018" # Change this for the year!
SRVY <- "AI" # Options: "GOA", "AI"

YEAR <- maxyr
vessel1 <- "FV Ocean Explorer"
vessel2 <- "FV Alaska Provider"
captain1 <- "Dan Carney"
captain2 <- "Loren Reynolds"

ref_compareyr <- "@von_szalay_data_2017" # should be included in cite/bibliography.bib
# Link to folder:  https://drive.google.com/drive/folders/1UAQKChSuKohsRJ5enOloHPk3qFtk5kVC
# This is where all the text files live and are edited.

if (SRVY == "AI") {
  dir_googledrive <- "11RBHMEQtkq4BsuzY7AeNdX8IQPr5bv_J"
  dir_blurbs <- "1jl0YXfsUNi-bKRoIKiaa3b0opLbmBTIg"
  survname_long <- "Aleutian Islands"
}
# Link to folder: https://drive.google.com/drive/folders/11RBHMEQtkq4BsuzY7AeNdX8IQPr5bv_J

# Report info -------------------------------------------------------------
report_title <- paste0(
  "Data Report: ", maxyr, " ", SRVY,
  " Bottom Trawl Survey"
)
report_authors <- "P. von Szalay, N. Raring, W. Palsson, B. Riggle, M. Siple"
report_yr <- maxyr

# Functions, packages, directories ---------------------------------------------
source("R/01_directories.R")
source("R/02_load_packages.R") 

# Functions and data prep -------------------------------------------------
source("R/04_functions.R") # May not need all these functions.
source("R/03_prep_data.R") # Make all the tables and stuff needed for report and pres.
source("R/plot_idx_xbyx.R") # *** move to functions script when ready

# Create tables and figures -----------------------------------------------\
load(file = paste0(dir_in_tables,"report_tables.rdata")) # object: list_tables

# Static map of region 
img1_path <- "img/AleutiansMap.png" 
img1 <- png::readPNG(img1_path)

load(file = paste0(
  dir_in_figures,"cpue_bubbles.rdata")) # object: list_cpue_bubbles
load(file = paste0(
  dir_in_figures,"joy_length.rdata")) # object: list_joy_length
load(file = paste0(
  dir_in_figures,"biomass_ts.rdata")) # object: list_biomass_ts

