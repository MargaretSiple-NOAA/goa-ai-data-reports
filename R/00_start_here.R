# GOA/AI DATA REPORT

# Report settings -------------------------------------------------------------
usePNGPDF <- "png"
maxyr <- 2022 # Change this for the year!
compareyr <- 2016 # Change this for the year!
print_figs <- FALSE # Do you want to print out PNGs of each figure?
pres_or_report <- "report" # if "pres" the scripts will only make a subset of the figures and tables, the ones that we show for the GPT meeting

# When did you save the last version of the figures and tables you want to use?
tabledate <- "2022-11-16"
figuredate <- "2022-11-16"

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

if (SRVY == "GOA") {
  dir_googledrive <- "1UAQKChSuKohsRJ5enOloHPk3qFtk5kVC"
  survname_long <- "Gulf of Alaska"
}
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

# Get data from RACEBASE and AI/GOA schemas ------------------------------------
x <- askYesNo(msg = "Do you want to download local versions of RACEBASE tables now? Skip this if you already have updated local tables.")
if (x) {
  source("R/05_download_data_from_oracle.R")
}

# Get text from Google Drive ----------------------------------------------
y <- askYesNo(msg = "Do you want to re-download Google Drive files now?")
if (y) {
  source("R/06_get_gdrive_chapters.R")
}

# Functions and data prep -------------------------------------------------
source("R/04_functions.R") # May not need all these functions.
source("R/03_prep_data.R") # Make all the tables and stuff needed for report and pres.
source("R/plot_idx_xbyx.R") # *** move to functions script when ready
source("R/markobubble.R") # Emily M rendition of bubble map. *** move to functions script when ready 


################################################################################
################################################################################
################################################################################
# EVERYTHING BELOW THIS LINE IS FOR REPORTS
################################################################################
################################################################################
################################################################################


# Create tables and figures -----------------------------------------------

z <- askYesNo(msg = "Do you want to build all the tables right now? NOTE: This may take a while.")
if (z) {
  source(here::here("R", "make_tabs.R"))
} else {
  load(
    file =
      paste0(dir_in_tables, "report_tables.rdata")
  ) # object: list_tables
  load(
    file =
      paste0(dir_in_tables, "table3s_list.rdata")
  ) # object: table3s_list
  load(
    file =
      paste0(dir_in_tables, "table4s_list.rdata")
  ) # object: table4s_list
  top_CPUE <- read.csv(file = 
                         paste0(dir_in_tables,"top_CPUE_",maxyr,".csv")
                       ) #/topcpue
}


aa <- askYesNo(msg = "Do you want to build all the figures right now? NOTE: This may take a while.")
if (aa) {
  source(here::here("R", "make_figures.R"))
}else{
  # Static map of region 
  img1_path <- "img/AleutiansMap.png" 
  img1 <- png::readPNG(img1_path)
  
  load(file = paste0(
    dir_in_figures,"cpue_bubbles.rdata")) # object: list_cpue_bubbles 
  load(file = paste0(
    dir_in_figures,"cpue_bubbles_strata.rdata")) # object: list_cpue_bubbles_strata 
  load(file = paste0(
    dir_in_figures,"joy_length.rdata")) # object: list_joy_length
  load(file = paste0(
    dir_in_figures,"biomass_ts.rdata")) # object: list_biomass_ts
}


# Render the markdown doc! -----------------------------------------------------

starttime <- Sys.time()
rmarkdown::render(paste0(dir_markdown, "/PARENT.Rmd"),
                  output_dir = dir_out_chapters,
                  output_file = "PARENT.docx"
)
Sys.time() - starttime


# SAVE METADATA ----------------------------------------------------------------

con <- file(paste0(dir_out_todaysrun, "metadata.log"))
sink(con, append = TRUE)
sessionInfo()
sink() # Restore output to console
# cat(readLines("notes.log"), sep="\n") # Look at the log
