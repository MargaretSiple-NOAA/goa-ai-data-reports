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
  a, SURVEY_DEFINITION_ID == ifelse(SRVY == "GOA", 47, 52) &
    AREA_ID == ifelse(SRVY == "GOA", 99903, 99904)
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
sql_channel <- gapindex::get_connected()

xx <- gapindex::get_data(
  year_set = maxyr,
  haul_type = 3,
  survey_set = SRVY,
  spp_codes = report_species$species_code,
  abundance_haul = "Y",
  sql_channel = sql_channel,
  pull_lengths = TRUE
)

cpue <- gapindex::calc_cpue(racebase_tables = xx)

biomass_stratum <- gapindex::calc_biomass_stratum(
  racebase_tables = xx,
  cpue = cpue
)

biomass_subarea <- gapindex::calc_biomass_subarea(
  racebase_tables = xx,
  biomass_strata = biomass_stratum
)

sizecomp_stratum <- gapindex::calc_sizecomp_stratum(
  racebase_cpue = cpue,
  racebase_stratum_popn = biomass_stratum,
  racebase_tables = xx
)

# Save to the local folder for SRVY:
write.csv(sizecomp_stratum,
  file = paste0("./data/local_", tolower(SRVY), "/sizecomp_stratum.csv"),
  row.names = FALSE
)

print("Finished downloading local versions of all tables.")

################## BUILD TABLES FROM ORACLE ####################################
# Table 3 is built with GAP_PRODUCTS
# Table 3 ingredients -----------------------------------------------------
# Add a column to the CPUE table with mean individual fish weight (for 'Table 3')
cpue$ind_wt_kg <- cpue$WEIGHT_KG / cpue$COUNT
cpue$ind_wt_kg[which(cpue$WEIGHT_KG == 0 & cpue$COUNT == 0)] <- 0

# AREA_IDs for each survey + AREA_ID for the total (need for several tables)
# This is really messy. Basically I have to separate out the spatial IDs and use them as a key, then do the same with the depth IDs and with the total region. I wish there was a better way! But I can't think of one.
area <- read.csv("data/local_gap_products/area.csv") # already subsetted to region SRVY.

inpfcdepth_ids <- area |>
  dplyr::filter(AREA_TYPE %in% c("INPFC BY DEPTH")) |>
  dplyr::select(AREA_ID) |>
  dplyr::pull()

inpfc_ids <- area |>
  dplyr::filter(AREA_TYPE %in% c("INPFC")) |>
  dplyr::select(AREA_ID) |>
  dplyr::pull()

depth_ids <- area |>
  dplyr::filter(AREA_TYPE %in% c("DEPTH")) |>
  dplyr::select(AREA_ID) |>
  dplyr::pull()

# Specific AREA IDS to deal with totals
if (SRVY == "GOA") {
  design_yr_filter <- 1984
  area_id_region <- 99903
} else {
  design_yr_filter <- 1980
  area_id_region <- 99904
}

# Make a lookup table for which INPFC AREAS each stratum is in
raw_stratum_groups <- read.csv("./data/local_gap_products/stratum_groups.csv") |>
  dplyr::filter(DESIGN_YEAR == design_yr_filter)

inpfc_groups <- raw_stratum_groups |>
  dplyr::filter(AREA_ID %in% inpfc_ids)

depth_groups <- raw_stratum_groups |>
  dplyr::filter(AREA_ID %in% depth_ids)

inpfcdepth_groups <- raw_stratum_groups |>
  dplyr::filter(AREA_ID %in% inpfcdepth_ids)

nrow(inpfc_groups) == nrow(depth_groups)

# add various groupings to CPUE table - i don't know how to do this in a non-clunky way
cpue_inpfc <- cpue |>
  dplyr::left_join(inpfc_groups, by = c("SURVEY_DEFINITION_ID", "STRATUM", "DESIGN_YEAR"), keep = FALSE) |>
  dplyr::filter(WEIGHT_KG > 0) |>
  dplyr::group_by(AREA_ID, SPECIES_CODE) |>
  dplyr::summarize(mean_ind_wt_kg = mean(WEIGHT_KG / COUNT)) |>
  dplyr::ungroup()

cpue_depth <- cpue |>
  dplyr::left_join(depth_groups, by = c("SURVEY_DEFINITION_ID", "STRATUM", "DESIGN_YEAR"), keep = FALSE) |>
  dplyr::filter(WEIGHT_KG > 0) |>
  dplyr::group_by(AREA_ID, SPECIES_CODE) |>
  dplyr::summarize(mean_ind_wt_kg = mean(WEIGHT_KG / COUNT)) |>
  dplyr::ungroup()

cpue_inpfcdepth <- cpue |>
  dplyr::left_join(inpfcdepth_groups, by = c("SURVEY_DEFINITION_ID", "STRATUM", "DESIGN_YEAR"), keep = FALSE) |>
  dplyr::filter(WEIGHT_KG > 0) |>
  dplyr::group_by(AREA_ID, SPECIES_CODE) |>
  dplyr::summarize(mean_ind_wt_kg = mean(WEIGHT_KG / COUNT)) |>
  dplyr::ungroup()

cpue_region <- cpue |>
  dplyr::left_join(raw_stratum_groups |> dplyr::filter(AREA_ID == area_id_region)) |>
  dplyr::filter(WEIGHT_KG > 0) |>
  dplyr::group_by(AREA_ID, SPECIES_CODE) |>
  dplyr::summarize(mean_ind_wt_kg = mean(WEIGHT_KG / COUNT)) |>
  dplyr::ungroup()

mean_sp_wts <- dplyr::bind_rows(cpue_inpfc, cpue_depth, cpue_inpfcdepth, cpue_region)

# write.csv(mean_sp_wts,
#   file = paste0("./data/local_", tolower(SRVY), "_processed/mean_sp_weights.csv"),
#   row.names = FALSE
# )

# Second piece: cpue, biomass, and confidence intervals for each area, depth, area+depth, and the whole survey area.
x <- biomass_subarea |>
  dplyr::filter(AREA_ID %in% unique(mean_sp_wts$AREA_ID)) |>
  dplyr::left_join(mean_sp_wts, by = c("AREA_ID", "SPECIES_CODE")) |>
  dplyr::rowwise() |>
  dplyr::mutate(
    LCL = lognorm_ci(mean = BIOMASS_MT, variance = BIOMASS_VAR)[1],
    HCL = lognorm_ci(mean = BIOMASS_MT, variance = BIOMASS_VAR)[2]
  ) |>
  dplyr::left_join(area, by = c("SURVEY_DEFINITION_ID", "AREA_ID"), relationship = "many-to-many") |>
  dplyr::select(
    SURVEY, YEAR, SPECIES_CODE, AREA_NAME, DESCRIPTION, AREA_TYPE,
    N_HAUL, N_COUNT, CPUE_KGKM2_MEAN,
    BIOMASS_MT, LCL, HCL, mean_ind_wt_kg, BIOMASS_VAR
  )

write.csv(x,
  file = paste0("./data/local_", tolower(SRVY), "_processed/table_3_allspps.csv"),
  row.names = FALSE
)

print("Finished processing local tables to draft table 3.")

# Table 4's (built w SQL) -------------------------------------------------
# make_tab4 function comes from the 03_functions.R file
lapply(X = unique(report_species$species_code), FUN = make_tab4, survey = SRVY, year = maxyr)
print("Finished creating table 4 for each species.")

################## CHECK LOCAL FOLDERS FOR RODBC ERRORS ########################
# This doesn't work yet and I can't figure it out yet-- need to use system() to look for error text.
# search_phrase <- "RODBC"
#
# # List all CSV files in the directory
# csv_files <- list.files(path = here::here('data'), pattern = "\\.csv$", full.names = TRUE)
# #csv_files <- gsub(" ",replacement = "[ ]",x = csv_files)
#
# # Iterate through each CSV file and search for the phrase
# for (file in csv_files) {
#
#   # Use grep in Unix-like systems or findstr in Windows
#   cmd <- paste0("findstr /c:", shQuote(search_phrase)," ", shQuote(file))
#
#   # Execute the command and capture the output
#   output <- system(cmd, intern = TRUE)
#
#   # Check if the phrase was found
#   if (length(output) > 0) {
#     message(paste("Phrase found in file:", file))
#     # You can add more actions here, like setting a flag or further processing
#   }
# }
