# 00_report_settings

# Report settings -------------------------------------------------------------
usePNGPDF <- "png"
maxyr <- 2025 # Change this for the year!
minyr <- 1990 # This is the min year for the "data stanza" - 1991 for AI and 1990 for GOA
compareyr <- 2023 # Change this for the year!
pres_or_report <- "report" # if "pres" the scripts will only make a subset of the figures and tables, the ones that we show for the GPT meeting
use_gapindex <- FALSE # If TRUE will calculate total biomass and cpue_raw using the gapindex pkg. If FALSE, will use GAP_PRODUCTS schema.
complexes <- TRUE # If TRUE will produce figures and tables for species complexes in addition to the basic single species (for AI this is OROX, REBS, OFLATS)
tablefont <- "Arial"

# Survey information ------------------------------------------------------
# charter start and end dates (From Ned: these dates should represent the inclusive vessel charter dates (we stagger start the vessels now) and not just the dates when we began and ended towing. The dates in the present report appear to capture the correct date range.)

SRVY <- "GOA" # Options: "GOA", "AI"
survname_long <- ifelse(SRVY == "GOA", "Gulf of Alaska", "Aleutian Islands")
redesign <- ifelse(maxyr >= 2025 & SRVY == "GOA", TRUE, FALSE)

# Check survey year and region combo:
if (maxyr %% 2 == 0 && SRVY == "GOA") {
  print("Even year should not be for GOA survey. Double check your maxyr and SRVY designations in 00_report_settings.R")
}
if (maxyr %% 2 != 0 && SRVY == "AI") {
  print("Odd year should not be used for AI survey. Double check your maxyr and SRVY designations in 00_report_settings.R")
}


if (SRVY == "AI") {
  dates_conducted <- "the 5th of June through the 3rd of August" # 2024 - Change this for the year!
} else {
  dates_conducted <- "the 18th of May through the 6th of August" # 2021
}


# Pre-assigned stations
if (SRVY == "GOA") {
  preassignedstationstatement <- "This year, we pre-assigned XX% of the total XXX stations allocated as “new” meaning the each vessel had to trawl around a dozen previously untrawled stations last summer
and this will become a permanent feature of our station allocations in the future."
} else {
  preassignedstationstatement <- ""
}

YEAR <- maxyr
design_year <- ifelse(SRVY == "GOA" & maxyr >= 2025, 2025, 1984)

# Vessels and captains
vessel1 <- "FV&nbsp;Ocean Explorer"
vessel2 <- "FV&nbsp;Alaska Provider"
captain1 <- "Dan Carney"
captain2 <- "Paulo da Cruz"

# Internal reviewers
reviewer1 <- "Thaddaeus Buser"
reviewer2 <- "Susanne McDermott"

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
report_authors <- "B. Riggle, A. Dowlin, M. Siple"
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

# Load species_year table
species_year <- read.csv("data/local_gap_products/species_year.csv")

# add complexes to species_year for easy lookup
species_year <- species_year |>
  dplyr::mutate(SPECIES_CODE = as.character(SPECIES_CODE)) |>
  dplyr::add_row(
    SPECIES_CODE = c("NRSSRS", "REBS"),
    YEAR_STARTED = c(1996, 2006)
  )

# Random figure settings we need, annoyingly:
# for length-depth scatter:
lscale <- 10
dscale <- 100

