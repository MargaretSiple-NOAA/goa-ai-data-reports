# 05_download_data_from_oracle
# This script contains everything you need an Oracle connection to run for the reports. You should be able to connect, download local versions of all the stuff you need, and then disconnect (or whatever) and continue to work on the report.

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
  # a <- RODBC::sqlQuery(channel, "SELECT * FROM GOA.BIOMASS_TOTAL")
  # write.csv(x = a, "./data/local_goa/biomass_total.csv", row.names = FALSE)
  # 
  # a <- RODBC::sqlQuery(channel, "SELECT * FROM GOA.BIOMASS_STRATUM")
  # write.csv(x = a, "./data/local_goa/biomass_stratum.csv", row.names = FALSE)

  a <- RODBC::sqlQuery(channel, "SELECT * FROM GOA.STATION_ALLOCATION")
  write.csv(x = a, "./data/local_goa/goa_station_allocation.csv", row.names = FALSE)

  # a <- RODBC::sqlQuery(channel, "SELECT * FROM GOA.CPUE")
  # write.csv(x = a, "./data/local_goa/cpue.csv", row.names = FALSE)
  # 
  # a <- RODBC::sqlQuery(channel, "SELECT * FROM GOA.SIZECOMP_TOTAL")
  # write.csv(x = a, "./data/local_goa/sizecomp_total.csv", row.names = FALSE)

  a <- RODBC::sqlQuery(channel, "SELECT * FROM GOA.GOAGRID")
  write.csv(x = a, "./data/local_goa/goagrid.csv", row.names = FALSE)

  print("Finished downloading GOA schema tables")
}


# AI ----------------------------------------------------------------------
if (SRVY == "AI") {
  # a <- RODBC::sqlQuery(channel, "SELECT * FROM AI.BIOMASS_TOTAL")
  # write.csv(x = a, "./data/local_ai/biomass_total.csv", row.names = FALSE)
  # 
  # a <- RODBC::sqlQuery(channel, "SELECT * FROM AI.BIOMASS_STRATUM")
  # write.csv(x = a, "./data/local_ai/biomass_stratum.csv", row.names = FALSE)

  a <- RODBC::sqlQuery(channel, "SELECT * FROM AI.STATION_ALLOCATION")
  write.csv(x = a, "./data/local_ai/ai_station_allocation.csv", row.names = FALSE)

  # a <- RODBC::sqlQuery(channel, "SELECT * FROM AI.CPUE")
  # write.csv(x = a, "./data/local_ai/cpue.csv", row.names = FALSE)
  # 
  # a <- RODBC::sqlQuery(channel, "SELECT * FROM AI.SIZECOMP_TOTAL")
  # write.csv(x = a, "./data/local_ai/sizecomp_total.csv", row.names = FALSE)

  print("Finished downloading AI schema tables")
}

# GAP_PRODUCTS ------------------------------------------------------------
# area
a <- RODBC::sqlQuery(channel, "SELECT * FROM GAP_PRODUCTS.AREA")
a <- a |>
  dplyr::filter(SURVEY_DEFINITION_ID == ifelse(SRVY == "GOA", 47, 52))
# &     DESIGN_YEAR == ifelse(SRVY == "GOA", 1984, 1980))
# print(paste("Using design year(s):", unique(a$DESIGN_YEAR)))

write.csv(x = a, "./data/local_gap_products/area.csv", row.names = FALSE)


# biomass
a <- RODBC::sqlQuery(channel, "SELECT * FROM GAP_PRODUCTS.BIOMASS")
a <- dplyr::filter(
  a,
  SURVEY_DEFINITION_ID == ifelse(SRVY == "GOA", 47, 52) # &
  # YEAR == maxyr
)

write.csv(x = a, "./data/local_gap_products/biomass.csv", row.names = FALSE)

# size comps - recreate the sizecomp table as it was in AI and GOA schemas
# MAY NEED TO WORK ON THIS MORE LATER. THIS SHOULD DOWNLOAD THE RAW TABLE AND THAT SHOULD BE PROCESSED LATER.

a <- RODBC::sqlQuery(channel, "SELECT * FROM GAP_PRODUCTS.SIZECOMP")
a <- dplyr::filter(
  a, SURVEY_DEFINITION_ID == ifelse(SRVY == "GOA", 47, 52) #&
    #AREA_ID == ifelse(SRVY == "GOA", 99903, 99904)
) |>
  dplyr::mutate(SURVEY = SRVY, SEX = dplyr::case_when(
    SEX == 1 ~ "MALES",
    SEX == 2 ~ "FEMALES",
    SEX == 3 ~ "UNSEXED"
  )) |>
  dplyr::rename(LENGTH = LENGTH_MM) |>
  tidyr::pivot_wider(
    names_from = "SEX",
    values_from = "POPULATION_COUNT",
    values_fill = 0
  ) |>
  dplyr::mutate(
    TOTAL = MALES + FEMALES + UNSEXED,
    SUMMARY_AREA = 999
  )

write.csv(x = a, "./data/local_gap_products/sizecomp.csv", row.names = FALSE)

# stratum groups
a <- RODBC::sqlQuery(channel, "SELECT * FROM GAP_PRODUCTS.STRATUM_GROUPS")
a <- dplyr::filter(
  a,
  SURVEY_DEFINITION_ID == ifelse(SRVY == "GOA", 47, 52)
)

write.csv(x = a, "./data/local_gap_products/stratum_groups.csv", row.names = FALSE)

# CPUE table
a <- RODBC::sqlQuery(channel, "SELECT * FROM GAP_PRODUCTS.CPUE")
cpue <- a

write.csv(x = a, "./data/local_gap_products/cpue.csv", row.names = FALSE)

print("Finished downloading GAP_PRODUCTS tables.")

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


################## USE GAPINDEX TO GET SIZECOMPS ###############################
# Use gapindex to get size comps
# sql_channel <- gapindex::get_connected()
# 
# xx <- gapindex::get_data(
#   year_set = maxyr,
#   haul_type = 3,
#   survey_set = SRVY,
#   spp_codes = report_species$species_code,
#   abundance_haul = "Y",
#   sql_channel = sql_channel,
#   pull_lengths = TRUE
# )
# 
# cpue <- gapindex::calc_cpue(racebase_tables = xx)
# 
# biomass_stratum <- gapindex::calc_biomass_stratum(
#   racebase_tables = xx,
#   cpue = cpue
# )
# 
# biomass_subarea <- gapindex::calc_biomass_subarea(
#   racebase_tables = xx,
#   biomass_strata = biomass_stratum
# )
# 
# sizecomp_stratum <- gapindex::calc_sizecomp_stratum(
#   racebase_cpue = cpue,
#   racebase_stratum_popn = biomass_stratum,
#   racebase_tables = xx
# )
# 
# # Save to the local folder for SRVY:
# write.csv(sizecomp_stratum,
#   file = paste0("./data/local_", tolower(SRVY), "/sizecomp_stratum.csv"),
#   row.names = FALSE
# )
# 
# print("Finished downloading local versions of all tables.")

