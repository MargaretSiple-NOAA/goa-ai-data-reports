# 00_report_settings

# Report settings -------------------------------------------------------------
usePNGPDF <- "png"
maxyr <- 2024 # Change this for the year!
minyr <- 1991 # This is the min year for the "data stanza" - 1991 for AI and 1990 for GOA
compareyr <- 2022 # Change this for the year!
pres_or_report <- "report" # if "pres" the scripts will only make a subset of the figures and tables, the ones that we show for the GPT meeting
use_gapindex <- FALSE # If TRUE will calculate total biomass and cpue_raw using the gapindex pkg. If FALSE, will use GAP_PRODUCTS schema.
complexes <- TRUE # If TRUE will produce figures and tables for species complexes in addition to the basic single species (for AI this is OROX, REBS, OFLATS)

# When did you save the last version of the figures and tables you want to use?
tabledate <- "2024-12-16" # 
figuredate <- "2024-12-16"
reportvaluesdate <- "2024-12-16"


# Survey information ------------------------------------------------------
# charter start and end dates (From Ned: these dates should represent the inclusive vessel charter dates (we stagger start the vessels now) and not just the dates when we began and ended towing. The dates in the present report appear to capture the correct date range.)

SRVY <- "AI" # Options: "GOA", "AI"
survname_long <- ifelse(SRVY == "GOA", "Gulf of Alaska", "Aleutian Islands")

# Check survey year and region combo:
if (maxyr %% 2 == 0 && SRVY == "GOA") {
  print("Even year should not be for GOA survey. Double check your maxyr and SRVY designations in 00_report_settings.R")
}
if (maxyr %% 2 != 0 && SRVY == "AI") {
  print("Odd year should not be used for AI survey. Double check your maxyr and SRVY designations in 00_report_settings.R")
}


if (SRVY == "AI") {
  dates_conducted <- "the 5th of June through the 3rd of August, 2024" # Change this for the year!
} else {
  dates_conducted <- "the 18th of May through the 6th of August, 2021"
}


# Pre-assigned stations
if (SRVY == "GOA") {
  preassignedstationstatement <- "This year, we pre-assigned XX% of the total XXX stations allocated as “new” meaning the each vessel had to trawl around a dozen previously untrawled stations last summer
and this will become a permanent feature of our station allocations in the future."
} else {
  preassignedstationstatement <- ""
}

YEAR <- maxyr

# Vessels and captains
vessel1 <- "FV&nbsp;Ocean Explorer"
vessel2 <- "FV&nbsp;Alaska Provider"
captain1 <- "Dan Carney"
captain2 <- "Brian Beaver"

# Internal reviewers
reviewer1 <- "Sarah T. Friedman"
reviewer2 <- "Christopher Anderson"

ref_compareyr <- "@von_szalay_data_2017" # should be included in cite/bibliography.bib

if (SRVY == "GOA") {
  dir_googledrive <- "1UAQKChSuKohsRJ5enOloHPk3qFtk5kVC"
}
# Link to folder:  https://drive.google.com/drive/folders/1UAQKChSuKohsRJ5enOloHPk3qFtk5kVC
# This is where all the text files live and are edited.

if (SRVY == "AI") {
  dir_googledrive <- "11RBHMEQtkq4BsuzY7AeNdX8IQPr5bv_J"
}

# Folder containing oto tables
dir_googledrive_otos <- "1jl0YXfsUNi-bKRoIKiaa3b0opLbmBTIg"

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
report_authors <- "A. Dowlin, M. Siple, P. von Szalay, N. Raring, B. Riggle"
report_yr <- maxyr


# Vessel specs ------------------------------------------------------------
vessel1_spec_chunk <- paste("The Alaska Provider is 53.6 m in overall length (LOA) and is powered by two main engines with 2,200 continuous horsepower (HP). It is equipped with two forward and one stern net reel, but only the aft reel was used during the", maxyr, "bottom trawl survey.")

vessel2_spec_chunk <- paste("The Ocean Explorer is 47.2 m LOA with a 1,500 HP main engine. It is equipped with two forward net reels, both of which were used during the", maxyr, "bottom trawl survey.")


# Species to include ------------------------------------------------------
# Get species table
if (SRVY == "AI") report_species0 <- read.csv("data/ai_report_specieslist.csv")
if (SRVY == "GOA") report_species0 <- read.csv("data/goa_report_specieslist.csv")


if (pres_or_report == "pres") {
  report_species <- report_species0 |> dplyr::filter(presentation == 1)
} else {
  report_species <- report_species0 |> dplyr::filter(report == 1)
}

# Reorder based on specified spps order
report_species <- report_species[order(report_species$reportorder), ]


# Define complexes
complex_lookup0 <- read.csv("data/complex_lookup.csv")
complex_lookup <- complex_lookup0 |>
  dplyr::filter(region == SRVY)
