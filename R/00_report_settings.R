# 00_report_settings

# Report settings -------------------------------------------------------------
usePNGPDF <- "png"
maxyr <- 2022 # Change this for the year!
minyr <- 1991 # This is the min year for the "data stanza"
compareyr <- 2018 # Change this for the year!
pres_or_report <- "report" # if "pres" the scripts will only make a subset of the figures and tables, the ones that we show for the GPT meeting
use_sql_cpue <- FALSE # Do we want to use CPUE values from BIOMASS_TOTAL or "build our own" from RACEBASE catch, haul, etc tables? The default, TRUE, uses CPUE values from BIOMASS_TOTAL.

# When did you save the last version of the figures and tables you want to use?
tabledate <-"2023-09-28"  #"2023-11-10"
figuredate <- "2023-09-28"
reportvaluesdate <- "2023-09-28"

# Survey information ------------------------------------------------------
# charter start and end dates (From Ned: these dates should represent the inclusive vessel charter dates (we stagger start the vessels now) and not just the dates when we began and ended towing. The dates in the present report appear to capture the correct date range.)

SRVY <- "AI" # Options: "GOA", "AI"

if (SRVY == "AI") {
  dates_conducted <- "6 June through 14 August, 2022" # Change this for the year!
} else {
  dates_conducted <- "18 May through 6 August, 2023"
}


# Pre-assigned stations
if (SRVY == "GOA") {
  # all_allocation <- read.csv(here::here("data", "local_goa", "goa_station_allocation.csv"))
  preassignedstationstatement <- "This year, we pre-assigned XX% of the total XXX stations allocated as “new” meaning the each vessel had to trawl around a dozen previously untrawled stations last summer
and this will become a permanent feature of our station allocations in the future."
} else {
  # all_allocation <- read.csv(here::here("data", "local_ai", "ai_station_allocation.csv"))
  preassignedstationstatement <- ""
}

YEAR <- maxyr

vessel1 <- "Ocean Explorer"
vessel2 <- "Alaska Provider"
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
  survname_long <- "Aleutian Islands"
}

# Folder containing blurb files
dir_blurbs <- "1jl0YXfsUNi-bKRoIKiaa3b0opLbmBTIg"

# Link to folder: https://drive.google.com/drive/folders/11RBHMEQtkq4BsuzY7AeNdX8IQPr5bv_J

# Order of districts (for plots and tables) -------------------------------
if (SRVY == "AI") {
  district_order <- c(
    "Southern Bering Sea",
    "Eastern Aleutians",
    "Central Aleutians",
    "Western Aleutians"
  ) # order of districts in the results
}
if (SRVY == "GOA") {
  district_order <- c(
    "Shumagin",
    "Chirikof",
    "Kodiak",
    "Yakutat",
    "Southeastern"
  )
}


# Report info -------------------------------------------------------------
report_title <- paste0(
  "Data Report: ", maxyr, " ", SRVY,
  " Bottom Trawl Survey"
)
report_authors <- "M. Siple, P. von Szalay, N. Raring, W. Palsson, B. Riggle"
report_yr <- maxyr


# Vessel specs ------------------------------------------------------------
vessel1_spec_chunk <- paste("The Alaska Provider is 53.6 m in overall length (LOA) and is powered by two main engines with 2,200 continuous horsepower (HP). It is equipped with two forward and one stern net reel, but only the aft reel was used during the survey", maxyr, "bottom trawl survey.")

vessel2_spec_chunk <- paste("The Ocean Explorer is 47.2 m LOA with a 1,500 HP main engine. It is equipped with two forward net reels, both of which were used during the survey", maxyr, "bottom trawl survey.")


# Species to include ------------------------------------------------------
# Get species table
if (SRVY == "AI") report_species <- read.csv("data/ai_report_specieslist.csv")
if (SRVY == "GOA") report_species <- read.csv("data/goa_report_specieslist.csv")

report_species <- report_species |>
  dplyr::arrange(-species_code) |>
  dplyr::filter(report == 1)

# Reorder based on specified spps order
report_species <- report_species[order(report_species$reportorder), ]
