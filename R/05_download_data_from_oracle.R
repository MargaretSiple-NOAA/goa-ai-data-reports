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

channel <- gapindex::get_connected(db = "AFSC")

################## DOWNLOAD TABLES##########################################
# RACEBASE ----------------------------------------------------------------

a <- RODBC::sqlQuery(channel, "SELECT * FROM RACEBASE.CATCH")
write.csv(x = a, "./data/local_racebase/catch.csv", row.names = FALSE)

print("Finished downloading CATCH")

haul <- RODBC::sqlQuery(channel, "SELECT * FROM RACEBASE.HAUL")
# haul <- RODBC::sqlQuery(
#   channel,
#   paste0(
#     "SELECT ",
#     paste0(names(a)[names(a) != "START_TIME"],
#       sep = ",", collapse = " "
#     ),
#     " TO_CHAR(START_TIME,'MM/DD/YYYY HH24:MI:SS') START_TIME  FROM RACEBASE.HAUL"
#   )
# )

write.csv(x = haul, "./data/local_racebase/haul.csv", row.names = FALSE)

if(!exists("haul")){
  haul <- read.csv("./data/local_racebase/haul.csv")
}

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
  a <- RODBC::sqlQuery(channel, "SELECT * FROM GOA.STATION_ALLOCATION")
  write.csv(x = a, "./data/local_goa/goa_station_allocation.csv", row.names = FALSE)

  a <- RODBC::sqlQuery(channel, "SELECT * FROM GOA.GOAGRID")
  write.csv(x = a, "./data/local_goa/goagrid.csv", row.names = FALSE)

  print("Finished downloading GOA schema tables")
}


# AI ----------------------------------------------------------------------
if (SRVY == "AI") {
  a <- RODBC::sqlQuery(channel, "SELECT * FROM AI.STATION_ALLOCATION")
  write.csv(x = a, "./data/local_ai/ai_station_allocation.csv", row.names = FALSE)

  print("Finished downloading AI schema tables")
}

# GAP_PRODUCTS ------------------------------------------------------------

# * TAXONOMIC_CLASSIFICATION ----------------------------------------------
a <- RODBC::sqlQuery(channel, "SELECT * FROM GAP_PRODUCTS.TAXONOMIC_CLASSIFICATION WHERE SURVEY_SPECIES = 1")

write.csv(x = a, "./data/local_gap_products/taxonomic_classification.csv", row.names = FALSE)

# * AREA ------------------------------------------------------------------
a <- RODBC::sqlQuery(channel, "SELECT * FROM GAP_PRODUCTS.AREA")
a <- a |>
  dplyr::filter(SURVEY_DEFINITION_ID == ifelse(SRVY == "GOA", 47, 52))

write.csv(x = a, "./data/local_gap_products/area.csv", row.names = FALSE)

# * CPUE ------------------------------------------------------------------
# CPUE table
cpue <- RODBC::sqlQuery(channel, "SELECT * FROM GAP_PRODUCTS.CPUE")
write.csv(x = cpue, "./data/local_gap_products/cpue.csv", row.names = FALSE)
print("Finished downloading GAP_PRODUCTS.CPUE")

# ** species --------------------------------------------------
# complex_lookup is defined in report_settings.
# So far, this uses the cpue and the haul tables from above.
# Filter and rename some columns
if(!exists("cpue")){
  cpue <- read.csv("./data/local_gap_products/cpue.csv")
}

cpue_raw <- cpue |>
  dplyr::right_join(haul) |> # SHOULD THIS BE LEFT_JOIN?
  dplyr::filter(REGION == SRVY) |>
  dplyr::mutate(year = as.numeric(substr(CRUISE, 1, 4))) |>
  dplyr::rename(
    survey = "REGION",
    longitude_dd_start = "START_LONGITUDE",
    latitude_dd_start = "START_LATITUDE"
  ) |>
  janitor::clean_names() |>
  dplyr::mutate(species_code = as.character(species_code))

yrs_to_pull <- minyr:maxyr

# ** complexes -------------------------------------------------
## Pull data
complexes_data <- gapindex::get_data(
  year_set = yrs_to_pull,
  survey_set = SRVY,
  spp_codes = data.frame(
    SPECIES_CODE = complex_lookup$species_code,
    GROUP_CODE = complex_lookup$complex #  GROUP has to be numeric
  ),
  haul_type = 3,
  abundance_haul = "Y",
  pull_lengths = TRUE,
  channel = channel
)

cpue_raw_complexes <- gapindex::calc_cpue(gapdata = complexes_data) |>
  dplyr::left_join(haul)

# glue together the species cpue from the gap_products table and the complexes cpue calculated from gapindex
cpue_processed <- dplyr::bind_rows(
  cpue_raw,
  janitor::clean_names(cpue_raw_complexes)
)
head(cpue_processed)
unique(cpue_processed$species_code)
any(is.na(cpue_processed$haul))

# write.csv(cpue_raw, file = paste0(dir_out_srvy_yr, "tables/cpue_raw.csv"))
write.csv(cpue_processed, file = paste0(dir_out_srvy_yr, "tables/cpue_all.csv")) # use this in place of cpue_raw, everywhere

# * BIOMASS  --------------------------------------------------------------
# biomass
# ** species --------------------------------------------------
biomass <- RODBC::sqlQuery(channel, "SELECT * FROM GAP_PRODUCTS.BIOMASS")
biomass <- dplyr::filter(
  biomass,
  SURVEY_DEFINITION_ID == ifelse(SRVY == "GOA", 47, 52) # &
  # YEAR == maxyr
)
write.csv(x = biomass, "./data/local_gap_products/biomass.csv", row.names = FALSE)

print("Finished downloading GAP_PRODUCTS.BIOMASS")


# Biomass for all species (not complexes)
if(!exists("biomass")){
  biomass <- read.csv("./data/local_gap_products/biomass.csv")
}

biomass_total <- biomass |>
  dplyr::filter(AREA_ID == ifelse(SRVY == "GOA", 99903, 99904)) |> # total B only
  mutate(
    MIN_BIOMASS = BIOMASS_MT - 2 * (sqrt(BIOMASS_VAR)),
    MAX_BIOMASS = BIOMASS_MT + 2 * (sqrt(BIOMASS_VAR))
  ) |>
  mutate(MIN_BIOMASS = ifelse(MIN_BIOMASS < 0, 0, MIN_BIOMASS)) |>
  mutate_at("SPECIES_CODE", as.character)

biomass_subarea_species <- biomass |>
  mutate_at("SPECIES_CODE", as.character)


# ** complexes --------------------------------------------
# Stratum biomass and stuff
biomass_stratum_complexes <- gapindex::calc_biomass_stratum(
  gapdata = complexes_data,
  cpue = cpue_raw_complexes
)

biomass_subarea_complexes <- gapindex::calc_biomass_subarea(
  gapdata = complexes_data,
  biomass_stratum = biomass_stratum_complexes
) # no need to save

biomass_total_complexes <- biomass_subarea_complexes |>
  dplyr::filter(AREA_ID == ifelse(SRVY == "GOA", 99903, 99904)) |> # total B only
  mutate(
    MIN_BIOMASS = BIOMASS_MT - 2 * (sqrt(BIOMASS_VAR)),
    MAX_BIOMASS = BIOMASS_MT + 2 * (sqrt(BIOMASS_VAR))
  ) |>
  mutate(MIN_BIOMASS = ifelse(MIN_BIOMASS < 0, 0, MIN_BIOMASS))


biomass_total <- dplyr::bind_rows(
  biomass_total,
  biomass_total_complexes
)

biomass_subarea <- dplyr::bind_rows(
  biomass_subarea_species,
  biomass_subarea_complexes
)

rm(list = c(
  "biomass_subarea_species",
  "biomass_subarea_complexes"
))

print("Created cpue_table_complexes and biomass_total_complexes.")

write.csv(biomass_stratum_complexes, file = paste0(dir_out_srvy_yr,"tables/biomass_stratum_complexes.csv")) # used in tables
write.csv(biomass_subarea, file = paste0(dir_out_srvy_yr,"tables/biomass_subarea_all.csv"))
write.csv(biomass_total, file = paste0(dir_out_srvy_yr,"tables/biomass_total_all.csv"))


# Stratum groups ----------------------------------------------------------
a <- RODBC::sqlQuery(channel, "SELECT * FROM GAP_PRODUCTS.STRATUM_GROUPS")
a <- subset(a, SURVEY_DEFINITION_ID == ifelse(SRVY == "GOA", 47, 52))
if (SRVY == "GOA") {
  a <- subset(a, DESIGN_YEAR == ifelse(maxyr < 2025, 1984, 2025))
}

write.csv(x = a, "./data/local_gap_products/stratum_groups.csv", row.names = FALSE)

# Table of when each species was first positively identified
a <- RODBC::sqlQuery(channel, "SELECT * FROM GAP_PRODUCTS.SPECIES_YEAR")
write.csv(x = a, "./data/local_gap_products/species_year.csv", row.names = FALSE)


# STRATUM_GROUPS table - to lookup what region a stratum is in
a <- RODBC::sqlQuery(channel, "SELECT * FROM GAP_PRODUCTS.STRATUM_GROUPS")
a <- subset(a, SURVEY_DEFINITION_ID == ifelse(SRVY == "GOA", 47, 52))

if (SRVY == "GOA") {
  a <- subset(a, DESIGN_YEAR == ifelse(maxyr < 2025, 1984, 2025))
}


write.csv(x = a, "./data/local_gap_products/stratum_groups.csv", row.names = FALSE)

print("Finished downloading GAP_PRODUCTS.CPUE")
print("Finished downloading GAP_PRODUCTS tables.")


# * SIZECOMP --------------------------------------------------------------
# size comps - recreate the sizecomp table as it was in AI and GOA schemas
# MAY NEED TO WORK ON THIS MORE LATER. THIS SHOULD DOWNLOAD THE RAW TABLE AND THAT SHOULD BE PROCESSED LATER.
# ** species ----------------------------------------------
sizecomp <- RODBC::sqlQuery(channel, "SELECT * FROM GAP_PRODUCTS.SIZECOMP")
sizecomp <- dplyr::filter(
  sizecomp, SURVEY_DEFINITION_ID == ifelse(SRVY == "GOA", 47, 52) # &
  # AREA_ID == ifelse(SRVY == "GOA", 99903, 99904)
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
  ) |>
  dplyr::filter(YEAR >= minyr) |>
  dplyr::mutate(SPECIES_CODE = as.character(SPECIES_CODE)) |>
  dplyr::filter(SPECIES_CODE %in% report_species$species_code)

write.csv(x = sizeecomp, "./data/local_gap_products/sizecomp.csv", row.names = FALSE)

print("Finished downloading GAP_PRODUCTS.SIZECOMP")


# ** complexes ------------------------------------------------------------
# Regardless of whether gapindex=TRUE or not, you will always need gapindex to calculate biomass, cpue, sizecomp tables for the complexes.
## Pull data.

# cpue_raw_caps_complexes <- gapindex::calc_cpue(gapdata = complexes_data) |>
# janitor::clean_names()

sizecomp_stratum_complexes <- gapindex::calc_sizecomp_stratum(
  gapdata = complexes_data,
  cpue = cpue_raw_complexes,
  abundance_stratum = biomass_stratum_complexes,
  spatial_level = "stratum",
  fill_NA_method = "AIGOA"
)

## Calculate aggregated size composition across subareas, management areas, and
## regions
sizecomp_subareas_complexes <- gapindex::calc_sizecomp_subarea(
  gapdata = complexes_data,
  sizecomp_stratum = sizecomp_stratum_complexes
)

sizecomp_complexes <- sizecomp_subareas_complexes |>
  dplyr::filter(
    SURVEY_DEFINITION_ID == ifelse(SRVY == "GOA", 47, 52) &
      AREA_ID == ifelse(SRVY == "GOA", 99903, 99904)
  ) |>
  dplyr::mutate(SURVEY = SRVY, SEX = case_when(
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
  ) |>
  as.data.frame()

# Full sizecomp table with species and complexes:
sizecomp <- dplyr::bind_rows(sizecomp, sizecomp_complexes)

rm(list = c("sizecomp_stratum_complexes"))

write.csv(sizecomp, file = paste0(dir_out_srvy_yr, "tables/sizecomp_all.csv"))

# Create 'pseudolengths' table used for length comp figures ----------------
# Janky but not sure how else to do it, so will have to deal. See notes below. This table is needed for joy division figs. Only make pseudolengths file if it isn't already there.
if (!file.exists(paste0(dir_out_srvy_yr, "tables/report_pseudolengths.csv"))) {
  
  if(!exists("sizecomp")){
    sizecomp <- read.csv(file = paste0(dir_out_srvy_yr,"tables/sizecomp_all.csv"))
  }
  
  report_pseudolengths <- data.frame()
  
  for (i in 1:nrow(report_species)) {
    sp_code <- report_species$species_code[i]
    
    # Is the species code for a complex?
    if (grepl(x = sp_code, "[A-Za-z]")) {
      sizecomp1 <- sizecomp[grepl("[A-Za-z]",sizecomp$SPECIES_CODE),]
    } else {
      sizecomp1 <- sizecomp[grepl("[0-9]",sizecomp$SPECIES_CODE),]
    }
    
    if (nrow(sizecomp1) == 0) {
      stop(paste("No size comps for species", sp_code))
    }
    
    males <- sizecomp1 |>
      dplyr::filter(YEAR <= maxyr & YEAR >= minyr) |>
      dplyr::filter(SPECIES_CODE == sp_code) |>
      dplyr::group_by(YEAR) |>
      dplyr::mutate(prop_10k = (MALES / sum(MALES)) * 10000) |>
      dplyr::mutate(prop_10k = round(prop_10k)) |>
      arrange(-YEAR, LENGTH) |>
      dplyr::mutate(prop_10k = ifelse(MALES == 0, 0, prop_10k)) |>
      tidyr::uncount(prop_10k, .id = "id") |>
      dplyr::select(SURVEY, YEAR, SPECIES_CODE, LENGTH, id) |>
      mutate(Sex = "Male")
    
    females <- sizecomp1 |>
      dplyr::filter(YEAR <= maxyr & YEAR >= minyr) |>
      dplyr::filter(SPECIES_CODE == sp_code) |>
      dplyr::group_by(YEAR) |>
      dplyr::mutate(prop_10k = (FEMALES / sum(FEMALES)) * 10000) |> # this is just a way to recreate the proportions in each length category with a smaller total number for figs and stuff.
      dplyr::mutate(prop_10k = round(prop_10k)) |>
      arrange(-YEAR, LENGTH) |>
      dplyr::mutate(prop_10k = ifelse(FEMALES == 0, 0, prop_10k)) |>
      uncount(prop_10k, .id = "id") |>
      dplyr::select(SURVEY, YEAR, SPECIES_CODE, LENGTH, id) |>
      mutate(Sex = "Female")
    
    unsexed <- sizecomp1 |>
      dplyr::filter(YEAR <= maxyr & YEAR >= minyr) |>
      dplyr::filter(SPECIES_CODE == sp_code) |>
      dplyr::group_by(YEAR) |>
      dplyr::mutate(prop_10k = (UNSEXED / sum(UNSEXED)) * 10000) |>
      dplyr::mutate(prop_10k = round(prop_10k)) |>
      arrange(-YEAR, LENGTH) |>
      dplyr::mutate(prop_10k = ifelse(UNSEXED == 0, 0, prop_10k)) |>
      uncount(prop_10k, .id = "id") |>
      dplyr::select(SURVEY, YEAR, SPECIES_CODE, LENGTH, id) |>
      mutate(Sex = "Unsexed")
    all <- bind_rows(males, females, unsexed)
    
    report_pseudolengths <- rbind(report_pseudolengths, all)
  }
  
  
  write.csv(report_pseudolengths, paste0(dir_out_srvy_yr, "tables/report_pseudolengths.csv"), row.names = FALSE)
  
  # Cleanup
  rm(list = c("males", "females", "unsexed", "report_pseudolengths"))
}


# Ex-vessel prices --------------------------------------------------------
# Filenames are misleading
if (SRVY == "AI") {
  a <- read.csv("G:/ALEUTIAN/Survey Planning/AI_planning_species_2020.csv")
  write.csv(x = a, "./data/AI_planning_species_2020.csv", row.names = FALSE)
}

if (SRVY == "GOA") {
  # GOA_planning_species_2021.csv
  # These are Sheet 1 from G:/GOA/Survey Planning/goa_planning_species_01052023.xlsx
  print("Using GOA_planning_species_2021.csv, which is based on goa_planning_species_01052023.xlsx. This is the most recent version of the ex-vessel prices for GOA species.")
}

print("Finished downloading all the Oracle tables.")

############ OPTIONAL: GAPINDEX TO GET CPUE AND BIOMASS TABLES ###########
# STILL NEED TO WORK ON THIS PART
# You can use gapindex to make tables like biomass_total if the GAP_PRODUCTS routine has not been run yet. This should be preliminary and not used for the final "gold standard" products. I have used it before for making Plan Team slides.
if (use_gapindex) {
  library(gapindex)

  ## Connect to Oracle
  sql_channel <- gapindex::get_connected()

  yrs_to_pull <- minyr:maxyr

  ## Pull data.
  rpt_data <- gapindex::get_data(
    year_set = yrs_to_pull,
    survey_set = SRVY,
    spp_codes =
      rbind(
        # species
        data.frame(
          GROUP_CODE = report_species[which(grepl("[0-9]", report_species$species_code)), "species_code"],
          SPECIES_CODE = report_species[which(grepl("[0-9]", report_species$species_code)), "species_code"]
        ),
        # complexes
        data.frame(
          GROUP_CODE = complex_lookup$complex,
          SPECIES_CODE = complex_lookup$species_code
        )
      ),
    haul_type = 3,
    abundance_haul = "Y",
    pull_lengths = TRUE,
    channel = sql_channel
  )

  cpue_raw_caps <- gapindex::calc_cpue(gapdata = rpt_data)

  biomass_stratum <- gapindex::calc_biomass_stratum(
    gapdata = rpt_data,
    cpue = cpue_raw_caps
  )
  # May need to use biomass_stratum to calculate CIs for total biomass. These are not currently included in gapindex.
  biomass_subarea <- gapindex::calc_biomass_subarea(
    gapdata = rpt_data,
    biomass_stratum = biomass_stratum
  )

  biomass_df <- biomass_subarea |>
    dplyr::filter(AREA_ID == ifelse(SRVY == "GOA", 99903, 99904)) |> # total B only
    mutate(
      MIN_BIOMASS = BIOMASS_MT - 2 * (sqrt(BIOMASS_VAR)),
      MAX_BIOMASS = BIOMASS_MT + 2 * (sqrt(BIOMASS_VAR))
    ) |>
    mutate(MIN_BIOMASS = ifelse(MIN_BIOMASS < 0, 0, MIN_BIOMASS))

  head(biomass_df)

  sizecomp_stratum <- gapindex::calc_sizecomp_stratum(
    gapdata = rpt_data,
    cpue = cpue_raw_caps,
    abundance_stratum = biomass_stratum,
    spatial_level = "stratum",
    fill_NA_method = "AIGOA"
  )

  ## Calculate aggregated size compositon across subareas, management areas, and
  ## regions
  sizecomp_subareas <- gapindex::calc_sizecomp_subarea(
    gapdata = rpt_data,
    sizecomp_stratum = sizecomp_stratum
  )

  sizecomp_gapindex <- sizecomp_subareas |>
    dplyr::filter(
      SURVEY_DEFINITION_ID == ifelse(SRVY == "GOA", 47, 52) &
        AREA_ID == ifelse(SRVY == "GOA", 99903, 99904)
    ) |>
    dplyr::mutate(SURVEY = SRVY, SEX = case_when(
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
    ) |>
    as.data.frame()

  # if you're using gapindex for comps, assign the gapindex table instead of the GAP_PRODUCTS table:
  sizecomp <- sizecomp_gapindex

  # cpue table
  cpue_raw <- cpue_raw_caps |>
    janitor::clean_names() # This table is used for lots of stuff

  # total biomass table
  biomass_total <- biomass_df

  # sizecomps will be assigned later

  rm(list = c("biomass_subarea"))
  print("Created biomass_total and cpue_raw with gapindex. This is a preliminary option and if the GAP_PRODUCTS routines have already been run this year, you should set use_gapindex=FALSE and use the GAP_PRODUCTS tables instead.")
}


# 
# if (!use_gapindex) { # NOTE: GOING TO HAVE TO UPDATE THIS TO LOAD PROCESSED TABLES
#   x <- read.csv(here::here("data", "local_gap_products", "biomass.csv"))
# 
#   # Biomass for all species (not complexes)
#   biomass_total <- x |>
#     dplyr::filter(AREA_ID == ifelse(SRVY == "GOA", 99903, 99904)) |> # total B only
#     mutate(
#       MIN_BIOMASS = BIOMASS_MT - 2 * (sqrt(BIOMASS_VAR)),
#       MAX_BIOMASS = BIOMASS_MT + 2 * (sqrt(BIOMASS_VAR))
#     ) |>
#     mutate(MIN_BIOMASS = ifelse(MIN_BIOMASS < 0, 0, MIN_BIOMASS)) |>
#     mutate_at("SPECIES_CODE", as.character)
# 
#   biomass_subarea_species <- x |>
#     mutate_at("SPECIES_CODE", as.character)
# 
#   x <- read.csv(here::here("data", "local_gap_products", "cpue.csv")) # this table contains all the cpue for all vessels, regions, etc!
# 
#   # Filter and rename some columns
#   cpue_raw <- x |>
#     dplyr::right_join(haul) |> # SHOULD THIS BE LEFT_JOIN?
#     dplyr::filter(REGION == SRVY) |>
#     dplyr::mutate(year = as.numeric(substr(CRUISE, 1, 4))) |>
#     dplyr::rename(
#       survey = "REGION",
#       longitude_dd_start = "START_LONGITUDE",
#       latitude_dd_start = "START_LATITUDE"
#     ) |>
#     janitor::clean_names() # still need cpue of complexes
# }