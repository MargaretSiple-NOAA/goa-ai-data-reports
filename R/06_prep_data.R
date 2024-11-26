# 06_prep_data
# Read in local copies of all the "stuff" needed for making tables and figs.
# This takes a while because a lot of these are big tables/files/etc.
# Note: if you've downloaded all the local versions of the tables, you should be able to run this script without internet access or anything. If it fails, make an issue!

# RACEBASE tables ----------------------------------------------------
# haul info (source: RACEBASE)
haul <- read.csv(here::here("data", "local_racebase", "haul.csv"))


# species ID info (source: RACEBASE) --------------------------------------
species_names0 <- read.csv(here::here("data", "local_racebase", "species.csv"), header = TRUE)
species_names <- species_names0 %>%
  janitor::clean_names() %>%
  dplyr::rename(scientific_name = species_name) %>%
  dplyr::select(-year_added) %>%
  dplyr::mutate(
    major_group = dplyr::case_when(
      species_code >= 10000 & species_code <= 19999 ~ "Flatfish",
      species_code >= 20000 & species_code <= 29999 ~ "Roundfish",
      species_code >= 30000 & species_code <= 36999 ~ "Rockfish",
      species_code >= 40000 & species_code <= 99990 ~ "Invertebrates",
      species_code >= 00150 & species_code <= 00799 ~ "Chondrichthyans"
    ),
    species_code = as.character(species_code)
  ) |> # add column for species category
  add_row(
    species_code = c("OROX", "OFLATS", "REBS"),
    scientific_name = c("Several species", "Several species", "Sebastes aleutianus and Sebastes melanosticus"),
    common_name = c("Other rockfish complex", "Other flatfish complex", "Rougheye/balckspotted complex"),
    major_group = c("Rockfish", "Flatfish", "Rockfish")
  )

# This year's haul data
haul_maxyr <- haul %>%
  mutate(YEAR = as.numeric(gsub("(^\\d{4}).*", "\\1", CRUISE))) %>% # extract year
  filter(REGION == SRVY & YEAR == maxyr)

# This year's survey number
cruises <- read.csv(here::here("data", "local_race_data", "cruises.csv"))

survnumber <- cruises %>%
  filter(SURVEY_NAME == ifelse(SRVY == "AI",
    "Aleutian Islands Bottom Trawl Survey",
    "Gulf of Alaska Bottom Trawl Survey"
  )) %>%
  filter(YEAR >= ifelse(SRVY == "AI", 1991, 1990)) %>% # Per Ned, "ABUNDANCE_HAUL = 'Y' should return the standardized survey stanza (1990-present for Gulf...after Chris Anderson runs the update I've proposed) and 1991 to present for AI"
  # filter(CRUISE != 199309) %>%
  distinct(CRUISE) %>%
  arrange(CRUISE) %>%
  nrow() %>%
  scales::ordinal()

# Temp data --------------------------------------------------------
# length(which(is.na(haul_maxyr$GEAR_TEMPERATURE)))
# length(which(is.na(haul_maxyr$SURFACE_TEMPERATURE)))

haul_maxyr %>%
  filter(GEAR_TEMPERATURE == 0)

minbottomtemp <- min(haul_maxyr$GEAR_TEMPERATURE[which(haul_maxyr$GEAR_TEMPERATURE > 0)],
  na.rm = T
)
maxbottomtemp <- max(haul_maxyr$GEAR_TEMPERATURE,
  na.rm = T
)

minsurfacetemp <- min(haul_maxyr$SURFACE_TEMPERATURE[which(haul_maxyr$SURFACE_TEMPERATURE > 0)],
  na.rm = T
)
maxsurfacetemp <- max(haul_maxyr$SURFACE_TEMPERATURE,
  na.rm = T
)

nhauls_no_stemp <- haul_maxyr %>%
  filter(is.na(SURFACE_TEMPERATURE)) %>%
  nrow()

nhauls_no_btemp <- haul_maxyr %>%
  filter(is.na(GEAR_TEMPERATURE)) %>%
  nrow()

# write.csv(file = "hauls_no_stemp.csv",x = hauls_no_stemp)
# write.csv(file = "hauls_no_btemp.csv",x = hauls_no_btemp)

# Econ info ---------------------------------------------------------------
dat <- read.csv("data/AI_planning_species_2020.csv")
sp_prices <- dat %>%
  dplyr::select(-species.code, common.name, species.name, include, ex.vessel.price, source) %>%
  dplyr::rename(
    `Scientific name` = species.name,
    `Common name` = common.name,
    `Included in design` = include,
    `Ex-vessel price` = ex.vessel.price,
    `Source` = source
  )

pricespeciescount <- nrow(sp_prices[which(!is.na(sp_prices$`Ex-vessel price`)), ])

# GAP_PRODUCTS: biomass, cpue --------------------------------------------------
x <- read.csv(here::here("data", "local_gap_products", "biomass.csv"))

biomass_total <- x |>
  dplyr::filter(AREA_ID == ifelse(SRVY == "GOA", 99903, 99904)) |> # total B only
  mutate(
    MIN_BIOMASS = BIOMASS_MT - 2 * (sqrt(BIOMASS_VAR)),
    MAX_BIOMASS = BIOMASS_MT + 2 * (sqrt(BIOMASS_VAR))
  ) |>
  mutate(MIN_BIOMASS = ifelse(MIN_BIOMASS < 0, 0, MIN_BIOMASS)) |>
  mutate_at("SPECIES_CODE", as.character)

biomass_subarea_species <- x |>
  mutate_at("SPECIES_CODE", as.character)

x <- read.csv(here::here("data", "local_gap_products", "cpue.csv")) # this table contains all the cpue for all vessels, regions, etc!

# Filter and rename some columns
cpue_raw <- x |>
  dplyr::right_join(haul) |>
  dplyr::filter(REGION == SRVY) |>
  dplyr::mutate(year = as.numeric(substr(CRUISE, 1, 4))) |>
  dplyr::rename(
    survey = "REGION",
    longitude_dd_start = "START_LONGITUDE",
    latitude_dd_start = "START_LATITUDE"
  ) |>
  janitor::clean_names()

# GAP_PRODUCTS: sizecomps ---------------------------------------
# Expand length table to make freqs -- these should be used for joy division and other length hist plots

if (!use_gapindex) {
  sizecomp0 <- read.csv("data/local_gap_products/sizecomp.csv", header = TRUE)

  sizecomp <- sizecomp0 |>
    dplyr::filter(SURVEY == SRVY & YEAR >= minyr) |>
    dplyr::mutate(SPECIES_CODE = as.character(SPECIES_CODE)) |>
    dplyr::filter(SPECIES_CODE %in% report_species$species_code)
}

# gapindex: Complexes. create biomass_total and cpue tables from gapindex ----
complex_lookup0 <- read.csv("data/complex_lookup.csv")
complex_lookup <- complex_lookup0 |>
  dplyr::filter(region == SRVY)

## Connect to Oracle
sql_channel <- gapindex::get_connected()

yrs_to_pull <- minyr:maxyr

## Pull data.
complexes_data <- gapindex::get_data(
  year_set = yrs_to_pull,
  survey_set = SRVY,
  spp_codes = data.frame(
    SPECIES_CODE = complex_lookup$species_code,
    GROUP = complex_lookup$complex #  GROUP has to be numeric
  ),
  haul_type = 3,
  abundance_haul = "Y",
  pull_lengths = TRUE,
  sql_channel = sql_channel
)

cpue_table_complexes <- gapindex::calc_cpue(racebase_tables = complexes_data)

biomass_stratum_complexes <- gapindex::calc_biomass_stratum(
  racebase_tables = complexes_data,
  cpue = cpue_table_complexes
)

biomass_subarea_complexes <- gapindex::calc_biomass_subarea(
  racebase_tables = complexes_data,
  biomass_strata = biomass_stratum_complexes
)

biomass_df_complexes <- biomass_subarea_complexes |>
  dplyr::filter(AREA_ID == ifelse(SRVY == "GOA", 99903, 99904)) |> # total B only
  mutate(
    MIN_BIOMASS = BIOMASS_MT - 2 * (sqrt(BIOMASS_VAR)),
    MAX_BIOMASS = BIOMASS_MT + 2 * (sqrt(BIOMASS_VAR))
  ) |>
  mutate(MIN_BIOMASS = ifelse(MIN_BIOMASS < 0, 0, MIN_BIOMASS))

# head(biomass_df_complexes)

biomass_total_complexes <- biomass_df_complexes

biomass_total <- dplyr::bind_rows(
  biomass_total,
  biomass_total_complexes
)

biomass_subarea <- dplyr::bind_rows(
  biomass_subarea_species,
  biomass_subarea_complexes
)

print("Created cpue_table_complexes and biomass_total_complexes.")

# Complexes: create sizecomps ---------------------------------------------
## Pull data.
#cpue_raw_caps_complexes <- gapindex::calc_cpue(racebase_tables = complexes_data)

sizecomp_stratum_complexes <- gapindex::calc_sizecomp_stratum(
  racebase_tables = complexes_data,
  racebase_cpue = cpue_table_complexes,
  racebase_stratum_popn = biomass_stratum_complexes,
  spatial_level = "stratum",
  fill_NA_method = "AIGOA"
)

## Calculate aggregated size composition across subareas, management areas, and
## regions
sizecomp_subareas_complexes <- gapindex::calc_sizecomp_subarea(
  racebase_tables = complexes_data,
  size_comps = sizecomp_stratum_complexes
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

sizecomp <- rbind(sizecomp, sizecomp_complexes)

# Just need to check that total species now in sizecomp is the number of individual species codes plus the number of complexes
if (length(unique(sizecomp$SPECIES_CODE)) != length(unique(report_species$species_code))) {
  print("Different numbers of stocks in report list compared to new sizecomp table. Check sizecomp code and report/presentation settings.")
}


############ OPTIONAL: GAPINDEX TO GET CPUE AND BIOMASS TABLES ###########
# You can use gapindex to make tables like biomass_total if the GAP_PRODUCTS routine has not been run yet. This should be preliminary and not used for the final "gold standard" products.
if (use_gapindex) {
  library(gapindex)

  ## Connect to Oracle
  sql_channel <- gapindex::get_connected()

  yrs_to_pull <- minyr:maxyr

  ## Pull data.
  rpt_data <- gapindex::get_data(
    year_set = yrs_to_pull,
    survey_set = SRVY,
    spp_codes = data.frame(
      SPECIES_CODE = report_species$species_code,
      GROUP = report_species$species_code
    ),
    haul_type = 3,
    abundance_haul = "Y",
    pull_lengths = TRUE,
    sql_channel = sql_channel
  )

  cpue_raw_caps <- gapindex::calc_cpue(racebase_tables = rpt_data)

  biomass_stratum <- gapindex::calc_biomass_stratum(
    racebase_tables = rpt_data,
    cpue = cpue_raw_caps
  )
  # May need to use biomass_stratum to calculate CIs for total biomass. These are not currently included in gapindex.
  biomass_subarea <- gapindex::calc_biomass_subarea(
    racebase_tables = rpt_data,
    biomass_strata = biomass_stratum
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
    racebase_tables = rpt_data,
    racebase_cpue = cpue_raw_caps,
    racebase_stratum_popn = biomass_stratum,
    spatial_level = "stratum",
    fill_NA_method = "AIGOA"
  )

  ## Calculate aggregated size compositon across subareas, management areas, and
  ## regions
  sizecomp_subareas <- gapindex::calc_sizecomp_subarea(
    racebase_tables = rpt_data,
    size_comps = sizecomp_stratum
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

  print("Created biomass_total and cpue_raw with gapindex. This is a preliminary option and if the GAP_PRODUCTS routines have already been run this year, you should set use_gapindex=FALSE and use the GAP_PRODUCTS tables instead.")
}


# Station allocation, counts, etc. ----------------------------------------
# Station allocation table (source: AI or GOA schema)
if (SRVY == "GOA") {
  all_allocation <- read.csv(here::here("data", "local_goa", "goa_station_allocation.csv"))
} else {
  all_allocation <- read.csv(here::here("data", "local_ai", "ai_station_allocation.csv"))
}

# Get a table of the strata and depths / regions (source: AI or GOA schema)
# This like a lookup table for allocating strata to the correct area and depth
dat <- read.csv(here::here("data", "goa_strata.csv"), header = TRUE)

region_lu <- dat |>
  dplyr::filter(SURVEY == SRVY) |>
  dplyr::select(
    SURVEY, STRATUM, INPFC_AREA, MIN_DEPTH, MAX_DEPTH,
    REGULATORY_AREA_NAME, AREA, DESCRIPTION
  ) |>
  dplyr::filter(STRATUM <= 794) |>
  tidyr::unite("Depth range", MIN_DEPTH:MAX_DEPTH, sep = " - ", remove = FALSE) |>
  dplyr::mutate(`Depth range` = paste0(`Depth range`, " m")) |>
  dplyr::mutate(INPFC_AREA = str_trim(INPFC_AREA))

# For AI years, add abbreviated area names:
region_lu2 <- region_lu |>
  dplyr::group_by(INPFC_AREA) |>
  dplyr::summarize(INPFC_AREA_AREA_km2 = sum(AREA, na.rm = T)) |>
  dplyr::ungroup() |>
  mutate(INPFC_AREA_ABBREV = case_when(
    INPFC_AREA == "Central Aleutians" ~ "Central AI",
    INPFC_AREA == "Eastern Aleutians" ~ "Eastern AI",
    INPFC_AREA == "Western Aleutians" ~ "Western AI",
    INPFC_AREA == "Southern Bering Sea" ~ "SBS"
  ))

# If it's an AI year, add Aleutian areas:
if (SRVY == "AI") {
  INPFC_areas <- region_lu2 %>%
    tibble::add_row(
      INPFC_AREA = "All Aleutian Districts",
      INPFC_AREA_AREA_km2 = sum(filter(region_lu2, INPFC_AREA != "Southern Bering Sea")$INPFC_AREA_AREA_km2)
    ) %>%
    tibble::add_row(
      INPFC_AREA = "All Districts",
      INPFC_AREA_AREA_km2 = sum(filter(region_lu2)$INPFC_AREA_AREA_km2)
    )
} else {
  INPFC_areas <- region_lu2 %>%
    tibble::add_row(
      INPFC_AREA = "All Districts",
      INPFC_AREA_AREA_km2 = sum(region_lu2$INPFC_AREA_AREA_km2)
    )
}

nyears <- length(unique(filter(haul, REGION == SRVY)$CRUISE))

haul2 <- haul %>%
  mutate(YEAR = stringr::str_extract(CRUISE, "^\\d{4}")) %>%
  filter(YEAR == maxyr & REGION == SRVY)

avg_net_height <- haul %>%
  filter(REGION == SRVY, CRUISE >= 199101 & ABUNDANCE_HAUL == "Y") %>%
  summarize(mean(NET_HEIGHT, na.rm = T)) %>%
  round(digits = 1) %>%
  as.numeric()

avg_net_width <- haul %>%
  filter(REGION == SRVY, CRUISE >= 199101 & ABUNDANCE_HAUL == "Y") %>%
  summarize(mean(NET_WIDTH, na.rm = T)) %>%
  round(digits = 1) %>%
  as.numeric()

nstationsassigned <- all_allocation %>%
  filter(YEAR == maxyr) %>%
  nrow()

nnewstations <- all_allocation %>%
  filter(YEAR == maxyr & is.na(STATIONID)) %>%
  nrow()

if (nnewstations == 0) {
  print("Code says no new stations were sampled this year. Is this correct?")
}

# Of the new stations allocated to the different vessels, which ones were successfully sampled?
hist_stations <- haul %>%
  mutate(YEAR = as.numeric(gsub("(^\\d{4}).*", "\\1", CRUISE))) %>%
  filter(REGION == SRVY) %>%
  filter(YEAR != maxyr) %>%
  distinct(STATIONID) %>%
  as.vector()
hist_stations <- hist_stations$STATIONID

test <- haul_maxyr %>%
  mutate(newstation = ifelse(STATIONID %in% hist_stations, "no", "yes")) %>%
  filter(newstation == "yes")

new_successfully_sampled <- test %>%
  filter(ABUNDANCE_HAUL == "Y") %>%
  nrow()

# In the AI, we assign boat to investigate new stations that haven't been trawled before. In the GOA survey, that doesn't happen.
if (SRVY == "AI") {
  newstationsentence <- paste(nnewstations / 2, "previously untrawled locations were assigned to each vessel in", maxyr, ". Among the", nnewstations, "total new stations assigned in the survey,", new_successfully_sampled, "were found and successfully trawled.")
} else {
  newstationsentence <- ""
}

# Number of stations "successfully sampled"
# Subset maxyr HAUL table to abundance_haul=="Y", count the number of unique stations.
nstations <- haul2 %>%
  filter(ABUNDANCE_HAUL == "Y") %>%
  distinct(STATIONID, STRATUM) %>%
  nrow()

# Number of "successful hauls":
#   Subset maxyr HAUL table to abundance_haul=="Y", count number of rows (i.e. the unique number of hauls).
nsuccessfulhauls <- haul2 %>%
  filter(ABUNDANCE_HAUL == "Y") %>%
  nrow()

# Number of attempted tows:
nattemptedhauls <- haul2 %>%
  filter(HAUL_TYPE == 3) %>%
  nrow() 

# Number of stations attempted:
nattemptedstations <- haul2 %>%
  distinct(STATIONID, STRATUM) %>%
  nrow() 

# Number of stations for which Marport net spread was successfully recorded:
nstations_w_marport_data <- haul2 %>%
  filter(ABUNDANCE_HAUL == "Y" & NET_MEASURED == "Y") %>%
  distinct(STATIONID, STRATUM) %>%
  nrow() 

nestimatedspreads <- haul2 %>%
  filter(ABUNDANCE_HAUL == "Y" & NET_MEASURED == "N") %>%
  distinct(STATIONID, STRATUM) %>%
  nrow()


# Number of "failed tows":
nfailedtows <- haul2 %>%
  filter(HAUL_TYPE == 3 & PERFORMANCE < 0) %>%
  nrow()

# Number of stations with no marport data - this is a phrase
no_marport_data <- paste(
  nestimatedspreads,
  ifelse(nestimatedspreads == 1, "station", "stations")
)

# Sentence about estimating haul width and height where needed
if (any(is.na(haul2$NET_WIDTH))) {
  marportpredsentence <- "For the ~1% of trawl hauls without net width, net spread was predicted from a generalized additive model (GAM) parameterized with successful trawl hauls of similar depth and wire out."
} else {
  marportpredsentence <- "Net width data were collected for all hauls using a Marport net spread sensor."
}


# Lengths and otos sampled -------------------------------------------
L0 <- read.csv(here::here("data/local_racebase/length.csv"))
L <- L0 %>%
  mutate(YEAR = as.numeric(gsub("(^\\d{4}).*", "\\1", CRUISE)))

length_maxyr_species <- filter(L, YEAR == maxyr & REGION == SRVY) |>
  dplyr::mutate_at(.vars = "SPECIES_CODE", as.character)

length_maxyr_complexes <- length_maxyr_species |>
  dplyr::filter(SPECIES_CODE %in% complex_lookup$species_code) |>
  dplyr::mutate(SPECIES_CODE = case_when(SPECIES_CODE %in% complex_lookup$species_code[which(complex_lookup$complex == "OROX")] ~ "OROX",
    SPECIES_CODE %in% complex_lookup$species_code[which(complex_lookup$complex == "REBS")] ~ "REBS",
    SPECIES_CODE %in% complex_lookup$species_code[which(complex_lookup$complex == "OFLATS")] ~ "OFLATS",
    .default = as.character(SPECIES_CODE)
  ))
unique(length_maxyr_complexes$SPECIES_CODE)

length_maxyr <- bind_rows(length_maxyr_species, length_maxyr_complexes)

# Number of lengths collected per area
lengths_collected <- sum(length_maxyr_species$FREQUENCY) %>%
  format(big.mark = ",")

nfishlengths <- sum(length_maxyr_species %>%
  filter(LENGTH_TYPE %in% c(1, 5, 11)) %>% dplyr::select(FREQUENCY)) %>%
  format(big.mark = ",")

nsquidlengths <- sum(length_maxyr_species %>%
  filter(LENGTH_TYPE == 12) %>% dplyr::select(FREQUENCY)) %>%
  format(big.mark = ",")

# Number of otoliths sampled per area
S <- read.csv(here::here("data", "local_racebase", "specimen.csv"))

specimen_maxyr <- S %>%
  mutate(YEAR = as.numeric(gsub("(^\\d{4}).*", "\\1", CRUISE))) %>%
  filter(YEAR == maxyr & REGION == SRVY)

otos_collected <- specimen_maxyr %>%
  filter(SPECIMEN_SAMPLE_TYPE == 1) %>% # this means it's an oto collection
  dplyr::left_join(haul_maxyr, by = c(
    "CRUISEJOIN", "HAULJOIN", "HAUL",
    "REGION", "VESSEL", "YEAR"
  )) %>%
  dplyr::left_join(region_lu, by = c("STRATUM")) %>%
  group_by(INPFC_AREA, `Depth range`) %>%
  dplyr::summarize("Pairs of otoliths collected" = n()) %>%
  ungroup() %>%
  arrange(factor(INPFC_AREA, levels = district_order))

otos_by_species <- specimen_maxyr %>%
  filter(SPECIMEN_SAMPLE_TYPE == 1) %>% # this means it's an oto collection
  dplyr::left_join(haul_maxyr, by = c(
    "CRUISEJOIN", "CRUISE", "HAULJOIN", "HAUL",
    "REGION", "VESSEL", "YEAR"
  )) %>%
  dplyr::group_by(SPECIES_CODE) |>
  dplyr::summarize("Pairs of otoliths collected" = n()) |>
  ungroup()

# This is for the presentation only
lengths_species <- length_maxyr_species |>
  dplyr::left_join(haul_maxyr, by = c(
    "CRUISEJOIN", "HAULJOIN",
    "REGION", "VESSEL", "CRUISE"
  )) |>
  dplyr::left_join(region_lu, by = c("STRATUM")) |>
  group_by(SPECIES_CODE) |>
  dplyr::summarize(
    "N" = sum(FREQUENCY, na.rm = TRUE)
  ) |>
  ungroup() |>
  dplyr::filter(SPECIES_CODE %in% report_species$species_code) |>
  dplyr::mutate(SPECIES_CODE = as.character(SPECIES_CODE)) |>
  dplyr::left_join(report_species, by = c("SPECIES_CODE" = "species_code")) |>
  dplyr::select(spp_name_informal, N) |>
  dplyr::rename(
    "Common name" = spp_name_informal,
    "Lengths collected" = N
  )

meanlengths_area <- length_maxyr %>%
  dplyr::left_join(haul_maxyr, by = c(
    "CRUISEJOIN", "HAULJOIN",
    "REGION", "VESSEL", "CRUISE"
  )) %>%
  dplyr::left_join(region_lu, by = c("STRATUM")) %>%
  group_by(SPECIES_CODE, INPFC_AREA) %>% # , `Depth range`
  dplyr::summarize(
    "N" = sum(FREQUENCY, na.rm = TRUE),
    "Mean length" = weighted.mean(LENGTH, w = FREQUENCY, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  dplyr::left_join(region_lu2)

meanlengths_depth <- length_maxyr %>%
  dplyr::left_join(haul_maxyr, by = c(
    "CRUISEJOIN", "HAULJOIN",
    "REGION", "VESSEL", "CRUISE"
  )) %>%
  dplyr::left_join(region_lu, by = c("STRATUM")) %>%
  dplyr::group_by(SPECIES_CODE, `Depth range`) %>% # ,
  dplyr::summarize(
    "N" = sum(FREQUENCY, na.rm = TRUE),
    "Mean length" = weighted.mean(LENGTH, w = FREQUENCY, na.rm = TRUE)
  ) %>%
  ungroup()

total_otos <- sum(otos_collected$`Pairs of otoliths collected`) %>%
  format(big.mark = ",")



# Create 'pseudolengths' table used for length comp figures ----------------
# Janky but I am in a rush so will have to deal. See notes below. This table is needed for joy division figs.
report_pseudolengths <- data.frame()

for (i in 1:nrow(report_species)) {
  sp_code <- report_species$species_code[i]

  # Is the species code for a complex?
  if (sp_code %in% c("OROX", "REBS", "OFLATS")) {
    sizecomp1 <- sizecomp_complexes
  } else {
    sizecomp1 <- sizecomp
  }

  males <- sizecomp1 |>
    filter(SPECIES_CODE == sp_code) |>
    dplyr::group_by(YEAR) |>
    dplyr::mutate(prop_10k = (MALES / sum(MALES)) * 10000) |>
    dplyr::mutate(prop_10k = round(prop_10k)) |>
    arrange(-YEAR, LENGTH) |>
    dplyr::mutate(prop_10k = ifelse(MALES == 0, 0, prop_10k)) |>
    tidyr::uncount(prop_10k, .id = "id") |>
    dplyr::select(SURVEY, YEAR, SPECIES_CODE, LENGTH, id) |>
    mutate(Sex = "Male")

  females <- sizecomp1 |>
    filter(SPECIES_CODE == sp_code) |>
    dplyr::group_by(YEAR) |>
    dplyr::mutate(prop_10k = (FEMALES / sum(FEMALES)) * 10000) |> # this is just a way to recreate the proportions in each length category with a smaller total number for figs and stuff.
    dplyr::mutate(prop_10k = round(prop_10k)) |>
    arrange(-YEAR, LENGTH) |>
    dplyr::mutate(prop_10k = ifelse(FEMALES == 0, 0, prop_10k)) |>
    uncount(prop_10k, .id = "id") |>
    dplyr::select(SURVEY, YEAR, SPECIES_CODE, LENGTH, id) |>
    mutate(Sex = "Female")

  unsexed <- sizecomp1 |>
    filter(SPECIES_CODE == sp_code) |>
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


write.csv(report_pseudolengths, paste0("data/", maxyr, "_", SRVY, "_", "report_pseudolengths.csv"), row.names = FALSE)

# Taxonomic diversity -----------------------------------------------------
# get number of fish and invert spps
catch <- read.csv("data/local_racebase/catch.csv", header = TRUE)


# Species with highest est'd biomass --------------------------------------
biomass_maxyr <- biomass_total %>%
  filter(YEAR == maxyr & SURVEY_DEFINITION_ID == ifelse(SRVY == "GOA", 47, 52))

highest_biomass <- biomass_maxyr %>%
  dplyr::slice_max(n = 50, order_by = BIOMASS_MT, with_ties = FALSE) %>%
  janitor::clean_names() %>%
  dplyr::left_join(species_names)

highest_biomass_flatfish <- highest_biomass %>%
  filter(major_group == "Flatfish")

highest_elasmos <- biomass_total %>%
  filter(YEAR == maxyr & SURVEY_DEFINITION_ID == ifelse(SRVY == "GOA", 47, 52)) %>%
  janitor::clean_names() %>%
  dplyr::left_join(species_names) %>%
  filter(major_group == "Chondrichthyans") %>%
  dplyr::slice_max(n = 3, order_by = biomass_mt, with_ties = FALSE)

highest_biomass_overall <- stringr::str_to_sentence(highest_biomass$common_name[1])
second_highest_biomass_overall <- highest_biomass$common_name[2]
third_highest_biomass_overall <- highest_biomass$common_name[3]
fourth_highest_biomass_overall <- highest_biomass$common_name[4]


# Random vessel info, not sure where to put this: 1,100 kg (Alaska Provider) or 800 kg (Ocean Explorer) - average catch weight per tow on each boat? Based on 2022 values.
