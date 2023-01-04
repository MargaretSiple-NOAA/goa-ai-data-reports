# 00_report_settings

# Report settings -------------------------------------------------------------
usePNGPDF <- "png"
maxyr <- 2022 # Change this for the year!
compareyr <- 2016 # Change this for the year!
print_figs <- FALSE # Do you want to print out PNGs of each figure?
pres_or_report <- "report" # if "pres" the scripts will only make a subset of the figures and tables, the ones that we show for the GPT meeting

# When did you save the last version of the figures and tables you want to use?
tabledate <- "2022-12-08"
figuredate <- "2022-12-08"
reportvaluesdate <- "2022-12-08"

# Survey information ------------------------------------------------------
#survnumber <- "twentieth" 
# charter start and end dates (From Ned: these dates should represent the inclusive vessel charter dates (we stagger start the vessels now) and not just the dates when we began and ended towing. The dates in the present report appear to capture the correct date range.)
dates_conducted <- "June 6th through August 14th, 2022" # Change this for the year!
SRVY <- "AI" # Options: "GOA", "AI"
nstationsassigned <- 420

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