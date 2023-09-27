# 05_download_data_from_oracle
# This script contains everything you need an Oracle connection to run for the reports. You should be able to connect, download local versions of all the stuff you need, and then disconnect (or whatever) and 

# Setup folders for local files -------------------------------------------
if (!file.exists("data/local_racebase")) dir.create("data/local_racebase", recursive = TRUE)
if (!file.exists("data/local_race_data")) dir.create("data/local_race_data", recursive = TRUE)
if (!file.exists("data/local_nodc")) dir.create("data/local_nodc", recursive = TRUE)
if (!file.exists("data/local_ai")) dir.create("data/local_ai", recursive = TRUE)
if (!file.exists("data/local_goa")) dir.create("data/local_goa", recursive = TRUE)
if (!file.exists("data/local_ai_processed")) dir.create("data/local_ai_processed", recursive = TRUE)
if (!file.exists("data/local_goa_processed")) dir.create("data/local_goa_processed", recursive = TRUE)

# Setup channel to connect to Oracle --------------------------------------

source("R/setup_channel.R")

# The setup_channel.R script sets up a channel using your Oracle username and pw.


################## DOWNLOAD TABLES##########################################
# RACEBASE ----------------------------------------------------------------

a <- RODBC::sqlQuery(channel, "SELECT * FROM RACEBASE.CATCH")
write.csv(x = a, "./data/local_racebase/catch.csv", row.names = FALSE)

print("Finished downloading CATCH")

a <- RODBC::sqlQuery(channel, "SELECT * FROM RACEBASE.HAUL")
a <- RODBC::sqlQuery(
  channel,
  paste0(
    "SELECT ",
    paste0(names(a)[names(a) != "START_TIME"],
      sep = ",", collapse = " "
    ),
    " TO_CHAR(START_TIME,'MM/DD/YYYY HH24:MI:SS') START_TIME  FROM RACEBASE.HAUL"
  )
)
write.csv(x = a, "./data/local_racebase/haul.csv", row.names = FALSE)

print("Finished downloading HAUL")

a <- RODBC::sqlQuery(channel, "SELECT * FROM RACEBASE.LENGTH")
write.csv(x = a, "./data/local_racebase/length.csv", row.names = FALSE)

print("Finished downloading LENGTH")

a <- RODBC::sqlQuery(channel, "SELECT * FROM RACEBASE.SPECIMEN")
write.csv(x = a, "./data/local_racebase/specimen.csv", row.names = FALSE)

a <- RODBC::sqlQuery(channel, "SELECT * FROM RACEBASE.STRATUM")
write.csv(x = a, "./data/local_racebase/stratum.csv", row.names = FALSE)

a <- RODBC::sqlQuery(channel, "SELECT * FROM RACEBASE.STATIONS")
write.csv(x = a, "./data/local_racebase/stations.csv", row.names = FALSE)

a <- RODBC::sqlQuery(channel, "SELECT * FROM RACEBASE.SPECIES")
write.csv(x = a, "./data/local_racebase/species.csv", row.names = FALSE)

a <- RODBC::sqlQuery(channel, "SELECT * FROM RACEBASE.CRUISE")
write.csv(x = a, "./data/local_racebase/cruise.csv", row.names = FALSE)

a <- RODBC::sqlQuery(channel, "SELECT * FROM RACEBASE.SPECIES_CLASSIFICATION")
write.csv(x = a, "./data/local_racebase/species_classification.csv", row.names = FALSE)

print("Finished downloading SPECIMEN, STRATUM, SPECIES, CRUISE and SPECIES_CLASSIFICATION")


# RACE_DATA ---------------------------------------------------------------

a <- RODBC::sqlQuery(channel, "SELECT * FROM RACE_DATA.HAULS")
write.csv(x = a, "./data/local_race_data/hauls.csv", row.names = FALSE)

a <- RODBC::sqlQuery(channel, "SELECT * FROM RACE_DATA.RACE_SPECIES_CODES")
write.csv(x = a, "./data/local_race_data/race_species_codes.csv", row.names = FALSE)

a <- RODBC::sqlQuery(channel, "SELECT * FROM RACE_DATA.VESSELS")
write.csv(x = a, "./data/local_race_data/vessels.csv", row.names = FALSE)

a <- RODBC::sqlQuery(channel, "SELECT * FROM RACE_DATA.TAXONOMIC_RANKS")
write.csv(x = a, "./data/local_race_data/taxonomic_ranks.csv", row.names = FALSE)

a <- RODBC::sqlQuery(channel, "SELECT * FROM RACE_DATA.SPECIES_TAXONOMICS")
write.csv(x = a, "./data/local_race_data/species_taxonomics.csv", row.names = FALSE)

a <- RODBC::sqlQuery(channel, "SELECT * FROM RACE_DATA.V_CRUISES")
write.csv(x = a, "./data/local_race_data/cruises.csv", row.names = FALSE)

print("Finished downloading RACE_DATA tables. We may not need all of these forever.")


# ADFG --------------------------------------------------------------------

a <- RODBC::sqlQuery(channel, "SELECT * FROM RACEBASE.LENGTH_ADFG")
write.csv(x = a, "./data/length_ADFG.csv", row.names = FALSE)

a <- RODBC::sqlQuery(channel, "SELECT * FROM RACEBASE.SPECIMEN_ADFG")
write.csv(x = a, "./data/specimen_ADFG.csv", row.names = FALSE)

print("Finished downloading ADF&G tables")


# GOA ---------------------------------------------------------------------

# Need goa_strata because it contains both GOA and AI strata
a <- RODBC::sqlQuery(channel, "SELECT * FROM GOA.GOA_STRATA")
write.csv(x = a, "./data/goa_strata.csv", row.names = FALSE)

print("Finished downloading strata file.")


if (SRVY == "GOA") {
  a <- RODBC::sqlQuery(channel, "SELECT * FROM GOA.BIOMASS_TOTAL")
  write.csv(x = a, "./data/local_goa/biomass_total.csv", row.names = FALSE)
  
  a <- RODBC::sqlQuery(channel, "SELECT * FROM GOA.BIOMASS_STRATUM")
  write.csv(x = a, "./data/local_goa/biomass_stratum.csv", row.names = FALSE)
  
  a <- RODBC::sqlQuery(channel, "SELECT * FROM GOA.STATION_ALLOCATION")
  write.csv(x = a, "./data/local_goa/goa_station_allocation.csv", row.names = FALSE)
  
  a <- RODBC::sqlQuery(channel, "SELECT * FROM GOA.CPUE")
  write.csv(x = a, "./data/local_goa/cpue.csv", row.names = FALSE)
  
  a <- RODBC::sqlQuery(channel, "SELECT * FROM GOA.SIZECOMP_TOTAL")
  write.csv(x = a, "./data/local_goa/sizecomp_total.csv", row.names = FALSE)
  
  a <- RODBC::sqlQuery(channel, "SELECT * FROM GOA.GOAGRID")
  write.csv(x = a, "./data/local_goa/goagrid.csv", row.names = FALSE)
  
  a <- RODBC::sqlQuery(channel, "SELECT * FROM GOA.STATION_ALLOCATION")
  write.csv(x = a, "./data/local_goa/goa_station_allocation.csv", row.names = FALSE)
  
  print("Finished downloading GOA schema tables")
}


# AI ----------------------------------------------------------------------
if (SRVY == "AI") {
  a <- RODBC::sqlQuery(channel, "SELECT * FROM AI.BIOMASS_TOTAL")
  write.csv(x = a, "./data/local_ai/biomass_total.csv", row.names = FALSE)

  a <- RODBC::sqlQuery(channel, "SELECT * FROM AI.BIOMASS_STRATUM")
  write.csv(x = a, "./data/local_ai/biomass_stratum.csv", row.names = FALSE)

  a <- RODBC::sqlQuery(channel, "SELECT * FROM AI.STATION_ALLOCATION")
  write.csv(x = a, "./data/local_ai/ai_station_allocation.csv", row.names = FALSE)

  a <- RODBC::sqlQuery(channel, "SELECT * FROM AI.CPUE")
  write.csv(x = a, "./data/local_ai/cpue.csv", row.names = FALSE)
  
  a <- RODBC::sqlQuery(channel, "SELECT * FROM AI.SIZECOMP_TOTAL")
  write.csv(x = a, "./data/local_ai/sizecomp_total.csv", row.names = FALSE)
  
  a <- RODBC::sqlQuery(channel, "SELECT * FROM AI.STATION_ALLOCATION")
  write.csv(x = a, "./data/local_ai/ai_station_allocation.csv", row.names = FALSE)
  
  print("Finished downloading AI schema tables")
}


# Ex-vessel prices --------------------------------------------------------

if (SRVY == "AI") {
  a <- read.csv("G:/ALEUTIAN/Survey Planning/AI_planning_species_2020.csv")
  write.csv(x = a, "./data/AI_planning_species_2020.csv", row.names = FALSE)
}

if (SRVY == "GOA") {
  # GOA_planning_species_2021.csv
  # These are Sheet 1 from G:/GOA/Survey Planning/goa_planning_species_01052023.xlsx
  print("Using GOA_planning_species_2021.csv, which is based on goa_planning_species_01052023.xlsx. This is the most recent version of the ex-vessel prices for GOA species.")
}

################## BUILD TABLES FROM ORACLE ####################################

# Get species table
if (SRVY == "AI") report_species <- read.csv(here::here("data", "ai_report_specieslist.csv"))
if (SRVY == "GOA") report_species <- read.csv(here::here("data", "goa_report_specieslist.csv"))

report_species <- report_species |>
  dplyr::arrange(-species_code) |>
  dplyr::filter(report == 1) 

# Reorder based on specified spps order
report_species <- report_species[order(report_species$reportorder), ]


# Table 4's (built w SQL) -------------------------------------------------
#' Make a rough draft of Table 4
#'
#' @param species_code species code (numeric)
#' @param survey survey code, "AI" or "GOA" (character)
#' @param year survey year (numeric)
#'
#' @return writes a csv file for all the 
#' @export
#'
#' @examples
#' make_tab4(species_code = 30060, survey = "GOA", year = 2023)
#'
make_tab4 <- function(species_code = NULL, survey = NULL, year = NULL) {
  a <- RODBC::sqlQuery(channel, paste0(
    "SELECT DISTINCT INPFC_AREA SURVEY_DISTRICT, MIN_DEPTH||'-'||MAX_DEPTH DEPTH_M, DESCRIPTION SUBDISTRICT_NAME, HAUL_COUNT NUMBER_OF_HAULS, CATCH_COUNT HAULS_W_CATCH, MEAN_WGT_CPUE/100 CPUE_KG_HA, STRATUM_BIOMASS BIOMASS_T, MIN_BIOMASS LCL_T, MAX_BIOMASS UCL_T FROM GOA.GOA_STRATA a, ", survey, ".BIOMASS_STRATUM b WHERE a.SURVEY = \'", survey, "\' and b.YEAR = ", year,
    " and b.SPECIES_CODE = ", species_code,
    " and a.STRATUM = b.STRATUM order by -CPUE_KG_HA"
  ))

  dir_out <- paste0("data/local_", tolower(survey), "_processed/table4_", species_code, "_", survey, "_", year, ".csv")

  write.csv(x = a, file = dir_out, row.names = FALSE)
}

#lapply(X = unique(report_species$species_code), FUN = make_tab4, survey=SRVY, year = maxyr)


# Table 3's (built w SQL) -------------------------------------------------



# All done!
print("Finished downloading local versions of tables! Yay.")

