# 00_report_settings

# Check if local version of repo is up to date with remote ------------------------
# This is experimental and built to make sure we aren't using an outdated version of the repo.
if(!"jsonlite" %in% installed.packages()){
  install.packages("jsonlite")
}

if(!"git2r" %in% installed.packages()){
  install.packages("jsonlite")
}

library(jsonlite)
library(git2r)


api_url <- "https://api.github.com/repos/MargaretSiple-NOAA/goa-ai-data-reports/commits/main"
remote_sha <- jsonlite::fromJSON(api_url)$sha

local_sha <- git2r::last_commit(getwd())$sha

is_up_to_date <- identical(local_sha, remote_sha)
cat("Local SHA:", local_sha, "\nGitHub SHA:   ", remote_sha, "\nUp to date:   ", is_up_to_date, "\n")


# Report settings -------------------------------------------------------------
usePNGPDF <- "png"
maxyr <- 2026 # Change this for the year!
minyr <- 1991 # This is the min year for the "data stanza" - 1991 for AI and 1990 for GOA
compareyr <- 2024 # Change this for the year!
pres_or_report <- "pres" # if "pres" the scripts will only make a subset of the figures and tables, the ones that we show for the GPT meeting
use_gapindex <- TRUE # If TRUE will calculate total biomass and cpue_raw using the gapindex pkg. If FALSE, will use GAP_PRODUCTS schema.
complexes <- TRUE # If TRUE will produce figures and tables for species complexes in addition to the basic single species (for AI this is OROX, REBS, OFLATS)
tablefont <- "Arial"

YEAR <- maxyr # probably can simplify this later
# Survey information ------------------------------------------------------
# charter start and end dates (From Ned: these dates should represent the inclusive vessel charter dates (we stagger start the vessels now) and not just the dates when we began and ended towing. The dates in the present report appear to capture the correct date range.)

SRVY <- "AI" # Options: "GOA", "AI"
survname_long <- ifelse(SRVY == "GOA", "Gulf of Alaska", "Aleutian Islands")
goa_redesign <- ifelse(maxyr >= 2025 & SRVY == "GOA", TRUE, FALSE)
goa_backlog <- ifelse(maxyr < 2025 & SRVY == "GOA", TRUE, FALSE)
ai_report <- ifelse(SRVY == "AI", TRUE, FALSE)

design_year <- ifelse(SRVY == "AI", 1991, 1984) # used to be 1980; I changed to 1991 for table merging
if(SRVY == "GOA" & maxyr >= 2025){
  design_year <- 2025
}

# Check survey year and region combo:
if (maxyr %% 2 == 0 && SRVY == "GOA") {
  print("Even year should not be for GOA survey. Double check your maxyr and SRVY designations in 00_report_settings.R")
}
if (maxyr %% 2 != 0 && SRVY == "AI") {
  print("Odd year should not be used for AI survey. Double check your maxyr and SRVY designations in 00_report_settings.R")
}


if (SRVY == "AI") {
  dates_conducted <- "the 30th of May through the 8th of August"
} else {
  dates_conducted <- "the 18th of May through the 6th of August" # 
}


# Pre-assigned stations
if (SRVY == "GOA") {
  preassignedstationstatement <- "This year, we pre-assigned XX% of the total XXX stations allocated as “new” meaning the each vessel had to trawl around a dozen previously untrawled stations last summer
and this will become a permanent feature of our station allocations in the future."
} else {
  preassignedstationstatement <- ""
}

# Vessels and captains
vessel1 <- "FV&nbsp;*Ocean Explorer*"
vessel2 <- "FV&nbsp;*Alaska Provider*"
captain1 <- "Dan Carney"
captain2 <- "Loren Reynolds"

# Internal reviewers
reviewer1 <- "INSERT REVIEWER NAME 1"
reviewer2 <- "INSERT REVIEWER NAME 2"

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
  if (maxyr >= 2025) {
    district_order <- c( # new GOA 2025+ areas (NMFS regulatory areas)
      "Shumagin", # This is the same as western GOA is the same as Western Regulatory Area (the INPFC designation)
      "Chirikof",
      "Kodiak",
      "West Yakutat",
      "Southeast Outside"
    )
  } else { # INPFC areas (for anything pre-2025)
    district_order <- c(
      "Shumagin",
      "Chirikof",
      "Kodiak",
      "Yakutat",
      "Southeastern"
    )
  }
}

# from redesign project:
# Western Regulatory Area: 159-170 longitude
# Chirikof: 154-159 longitude
# Kodiak: 147-154 longitude
# Yakutat:147-140 longitude
# Southeast: 132-140 longitude

# Report info -------------------------------------------------------------
report_title <- paste0(
  "Data Report: ", maxyr, " ", SRVY,
  " Bottom Trawl Survey"
)
report_authors <- "A. Dowlin, M.Siple, B. Riggle"
report_yr <- maxyr


# Vessel specs ------------------------------------------------------------
vessel1_spec_chunk <- paste("The Alaska Provider is 53.6 m in overall length and is powered by two main engines with 2,200 continuous horsepower. It is equipped with two forward and one stern net reel.")

vessel2_spec_chunk <- paste("The Ocean Explorer is 47.2 m overall length with a 1,500 horsepower main engine. It is equipped with two forward net reels.")


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

if (nrow(complex_lookup |> dplyr::count(species_code) |> dplyr::filter(n > 1)) > 0) {
  print("Stop! Duplicates in complex species lookup table")
}

# Load species_year table
if(file.exists("data/local_gap_products/species_year.csv")){
  species_year <- read.csv("data/local_gap_products/species_year.csv")
}

# add complexes to species_year for easy lookup
species_year <- species_year |>
  dplyr::mutate(SPECIES_CODE = as.character(SPECIES_CODE)) |>
  dplyr::add_row(
    SPECIES_CODE = c("NRSSRS", "REBS"),
    YEAR_STARTED = c(1996, 2006)
  )

# Did we have an IPHC person aboard? 
if(maxyr < 2023){
  iphc_sentence <- "When halibut were retained onboard, sample sizes were set in consultation with the IPHC. Halibut sample sizes are available in the otolith collection schedule and approved scientific collections requests."}else{
    iphc_sentence <- ""}

# Random figure settings we need, annoyingly:
# for length-depth scatter:
lscale <- 10
dscale <- 100

# For labels on length diagrams
left_labels <- c(30420, 30152) # species for which you want the label on the left instead of the right!

