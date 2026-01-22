# 06_prep_data
# Read in local copies of all the "stuff" needed for making tables and figs.
# This takes a while because a lot of these are big tables/files/etc.
# Note: if you've downloaded all the local versions of the tables, you should be able to run this script without internet access or anything. If it fails, make an issue!

# RACEBASE tables ----------------------------------------------------
# haul info (source: RACEBASE)
haul <- read.csv(here::here("data", "local_racebase", "haul.csv"))


# species ID info (source: RACEBASE) --------------------------------------
species_names0 <- read.csv(here::here("data", "local_racebase", "species.csv"), header = TRUE)
species_names <- species_names0 |>
  janitor::clean_names() |>
  dplyr::rename(scientific_name = species_name) |>
  dplyr::select(-year_added) |>
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
  tibble::add_row(
    species_code = c(
      "OROX", "OFLATS", "REBS",
      "DEEPFLATS", "DSROX", "NRSSRS",
      "SWFLATS", "SHARKS", "SKATES",
      "THORNYHEADS"
    ),
    scientific_name = c(
      "Several species", "Several species", "Sebastes aleutianus and Sebastes melanosticus",
      "Several species", "Several species", "Lepidopsetta polyxystra and L. bilineata",
      "Several species", "Several species", "Several species",
      "Sebastolobus sp."
    ),
    common_name = c(
      "Other rockfish complex", "Other flatfish complex", "Rougheye/blackspotted complex",
      "Deepwater flatfish complex", "Demersal shelf rockfish", "Northern and southern rock sole complex",
      "Shallow-water flatfish complex", "Sharks", "Skates",
      "Thornyheads"
    ),
    major_group = c(
      "Rockfish", "Flatfish", "Rockfish",
      "Flatfish", "Rockfish", "Flatfish",
      "Flatfish", "Chondrichthyans", "Chondrichthyans",
      "Thornyheads"
    )
  )


# Haul data ---------------------------------------------------------------
haul_maxyr <- haul |>
  mutate(YEAR = as.numeric(gsub("(^\\d{4}).*", "\\1", CRUISE))) |> # extract year
  filter(REGION == SRVY & YEAR != 1997) |> # YEAR >= 1994 &
  filter(YEAR == maxyr & ABUNDANCE_HAUL == "Y")

# This year's survey number
cruises <- read.csv(here::here("data", "local_race_data", "cruises.csv"))

survnumber <- cruises |>
  filter(SURVEY_NAME == ifelse(SRVY == "AI",
    "Aleutian Islands Bottom Trawl Survey",
    "Gulf of Alaska Bottom Trawl Survey"
  )) |>
  filter(YEAR >= ifelse(SRVY == "AI", 1991, 1990)) |> # Per Ned, "ABUNDANCE_HAUL = 'Y' should return the standardized survey stanza (1990-present for Gulf...after Chris Anderson runs the update I've proposed) and 1991 to present for AI"
  dplyr::filter(CRUISE != 202001 & YEAR <= maxyr) |>
  distinct(CRUISE) |>
  arrange(CRUISE) |>
  nrow() |>
  scales::ordinal()

# Temp data --------------------------------------------------------

haul_maxyr |>
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

nhauls_no_stemp <- haul_maxyr |>
  filter(is.na(SURFACE_TEMPERATURE)) |>
  nrow()

nhauls_no_btemp <- haul_maxyr |>
  filter(is.na(GEAR_TEMPERATURE)) |>
  nrow()

# Special sentence about temperature data
if (nhauls_no_btemp == 0 & nhauls_no_stemp == 0) {
  temp_record_sentence <- "Bottom and surface temperature data were recorded for all hauls."
}
if (nhauls_no_btemp == 0 & nhauls_no_stemp > 0) {
  temp_record_sentence <- paste("Temperatures at depth were recorded for all hauls and surface temperature data were recorded for all but", nhauls_no_stemp, "hauls.")
}
if (nhauls_no_btemp > 0 & nhauls_no_stemp == 0) {
  temp_record_sentence <- paste("Surface temperature data were recorded for all hauls and temperatures at depth were recorded for all but", nhauls_no_btemp, "hauls.")
}
if (nhauls_no_btemp > 0 & nhauls_no_stemp > 0) {
  temp_record_sentence <- paste("Bottom temperatures were recorded for all but", nhauls_no_btemp, "hauls and surface temperature data were recorded for all but", nhauls_no_stemp, "hauls.")
}


# write.csv(file = "hauls_no_stemp.csv",x = hauls_no_stemp)
# write.csv(file = "hauls_no_btemp.csv",x = hauls_no_btemp)

# Econ info ---------------------------------------------------------------
# dat <- read.csv("data/GOA_planning_species_2021.csv") #for 2021 report
dat <- read.csv("data/GOA_planning_species_2023.csv")
sp_prices <- dat |>
  dplyr::filter(!is.na(ex.vessel.price)) |>
  dplyr::select(-species.code, common.name, species.name, include, ex.vessel.price, source) |>
  dplyr::rename(
    `Scientific name` = species.name,
    `Common name` = common.name,
    `Included in design` = include,
    `Ex-vessel price` = ex.vessel.price,
    `Source` = source
  )

pricespeciescount <- nrow(sp_prices[which(!is.na(sp_prices$`Ex-vessel price`)), ])

# Station allocation, counts, etc. ----------------------------------------
# Station allocation table (source: AI or GOA schema)
if (SRVY == "GOA") {
  all_allocation <- readxl::read_xlsx(path = "G:/GOA/GOA 2025/Station Allocation/goa_2025_station_allocation_450.xlsx", sheet = "Station Allocation") # customize from year to year
} else {
  all_allocation <- read.csv(here::here("data", "local_ai", "ai_station_allocation.csv"))
}

if (maxyr == 2024) { # get allocation from special sheet with reduced stations
  a0 <- read.csv("data/AI2024_allocation.csv", na.strings = "NA")
  table(a0$STATION_TYPE)
  all_allocation <- a0 |>
    mutate(YEAR = 2024, SURVEY = SRVY)
}


# We didn't do bonus stations until 2022:
if (maxyr <= 2021) {
  all_allocation <- all_allocation |>
    mutate(STATION_TYPE = "before_2022")
}

# Get a table of the strata and depths / regions (source: GAP_PRODUCTS)
# This like a lookup table for allocating strata to the correct area and depth
dat <- read.csv("data/local_gap_products/area.csv")

stratum_lu <- dat |>
  dplyr::mutate(SURVEY = ifelse(SURVEY_DEFINITION_ID == 47, "GOA", "AI")) |>
  dplyr::filter(SURVEY_DEFINITION_ID == 47 & DESIGN_YEAR == design_year & AREA_TYPE == "STRATUM") |>
  dplyr::rename(
    STRATUM = "AREA_ID",
    MIN_DEPTH = "DEPTH_MIN_M",
    MAX_DEPTH = "DEPTH_MAX_M",
    REGULATORY_AREA_NAME = "AREA_NAME",
    AREA = "AREA_KM2"
  ) |>
  dplyr::select(
    SURVEY, STRATUM, MIN_DEPTH, MAX_DEPTH,
    REGULATORY_AREA_NAME, AREA, DESCRIPTION
  ) |>
  # dplyr::filter(STRATUM <= 794) |>
  tidyr::unite("Depth range", MIN_DEPTH:MAX_DEPTH, sep = " - ", remove = FALSE) |>
  dplyr::mutate(`Depth range` = paste0(`Depth range`, " m")) |>
  dplyr::mutate(REGULATORY_AREA_NAME = str_trim(REGULATORY_AREA_NAME))

if (SRVY == "GOA" & maxyr >= 2025) {
  region_lu <- dat |>
    dplyr::mutate(SURVEY = ifelse(SURVEY_DEFINITION_ID == 47, "GOA", "AI")) |>
    dplyr::filter(SURVEY_DEFINITION_ID == 47, AREA_TYPE == "NMFS STATISTICAL AREA", DESIGN_YEAR == design_year) |>
    dplyr::rename(
      STRATUM = "AREA_ID",
      MIN_DEPTH = "DEPTH_MIN_M",
      MAX_DEPTH = "DEPTH_MAX_M",
      REGULATORY_AREA_NAME = "AREA_NAME",
      AREA = "AREA_KM2"
    ) |>
    dplyr::select(
      SURVEY, STRATUM, MIN_DEPTH, MAX_DEPTH,
      REGULATORY_AREA_NAME, AREA, DESCRIPTION
    ) |>
    # dplyr::filter(STRATUM <= 794) |>
    tidyr::unite("Depth range", MIN_DEPTH:MAX_DEPTH, sep = " - ", remove = FALSE) |>
    dplyr::mutate(`Depth range` = paste0(`Depth range`, " m")) |>
    dplyr::mutate(REGULATORY_AREA_NAME = str_trim(REGULATORY_AREA_NAME))
} else { # AI survey and old GOA
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
} # /end if() for old GOA and AI designs


nyears <- length(unique(filter(haul, REGION == SRVY)$CRUISE))

haul2 <- haul |>
  mutate(YEAR = as.numeric(stringr::str_extract(CRUISE, "^\\d{4}"))) |>
  filter(YEAR == maxyr & REGION == SRVY)

avg_net_height <- haul |>
  filter(REGION == SRVY, CRUISE >= 199101 & ABUNDANCE_HAUL == "Y") |>
  summarize(mean(NET_HEIGHT, na.rm = T)) |>
  round(digits = 1) |>
  as.numeric()

avg_net_width <- haul |>
  filter(REGION == SRVY, CRUISE >= 199101 & ABUNDANCE_HAUL == "Y") |>
  summarize(mean(NET_WIDTH, na.rm = T)) |>
  round(digits = 1) |>
  as.numeric()

if (maxyr > 2023) {
  nstationsassigned <- all_allocation |>
    filter(YEAR == maxyr & STATION_TYPE != "bonus_stn") |>
    nrow()
} else {
  nstationsassigned <- all_allocation |>
    filter(YEAR == maxyr) |>
    nrow()
}

nnewstations <- all_allocation |>
  filter(YEAR == maxyr & STATION_TYPE == "new_stn") |>
  nrow()

if (nnewstations == 0) {
  print("Code says no new stations were sampled this year. If this is not correct, check allocation table.")
}

# Of the new stations allocated to the different vessels, which ones were successfully sampled?
hist_stations <- haul |>
  mutate(YEAR = as.numeric(gsub("(^\\d{4}).*", "\\1", CRUISE))) |>
  filter(REGION == SRVY) |>
  filter(YEAR != maxyr) |>
  distinct(STATIONID) |>
  as.vector()
hist_stations <- hist_stations$STATIONID

test <- haul_maxyr |>
  mutate(newstation = ifelse(STATIONID %in% hist_stations, "no", "yes")) |>
  filter(newstation == "yes")

new_successfully_sampled <- test |>
  filter(ABUNDANCE_HAUL == "Y") |>
  nrow()

# In the AI, we assign boat to investigate new stations that haven't been trawled before. In the GOA survey, that doesn't happen.
if (SRVY == "AI") {
  newstationsentence <- paste(nnewstations / 2, "previously untrawled locations were assigned to each vessel in", maxyr, ". Among the", nnewstations, "total new stations assigned in the survey,", new_successfully_sampled, "were found and successfully trawled.")
} else {
  newstationsentence <- ""
}

# Number of stations "successfully sampled"
# Subset maxyr HAUL table to abundance_haul=="Y", count the number of unique stations.
nstations <- haul2 |>
  filter(ABUNDANCE_HAUL == "Y") |>
  distinct(STATIONID, STRATUM) |>
  nrow()

# Number of "successful hauls":
#   Subset maxyr HAUL table to abundance_haul=="Y", count number of rows (i.e. the unique number of hauls).
nsuccessfulhauls <- haul2 |>
  filter(ABUNDANCE_HAUL == "Y") |>
  nrow()

# Number of attempted tows:
nattemptedhauls <- haul2 |>
  filter(HAUL_TYPE == 3) |>
  nrow()

# Number of stations attempted:
nattemptedstations <- haul2 |>
  distinct(STATIONID, STRATUM) |>
  nrow()

# Number of stations for which Marport net spread was successfully recorded:
nstations_w_marport_data <- haul2 |>
  filter(ABUNDANCE_HAUL == "Y" & NET_MEASURED == "Y") |>
  distinct(STATIONID, STRATUM) |>
  nrow()

nestimatedspreads <- haul2 |>
  filter(ABUNDANCE_HAUL == "Y" & NET_MEASURED == "N") |>
  distinct(STATIONID, STRATUM) |>
  nrow()


# Number of "failed tows":
nfailedtows <- haul2 |>
  filter(HAUL_TYPE == 3 & PERFORMANCE < 0) |>
  nrow()

pct_reduction_from_550 <- round((nstationsassigned/550) * 100)
# SQL version for nfailedtows (from Ned):
# select count(*) from racebase.haul
# where region = 'GOA' and cruise = 202101
# and abundance_haul = 'N'
# and haul_type = 3)

# check the number of stations successfully sampled
if ((nestimatedspreads + nstations_w_marport_data) != nstations) {
  print("Check nstations and nstations_w_marport_data! They are not adding up properly.")
}

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


# Depths and areas with highest sampling densities ------------------------
load(paste0(dir_out_tables, "list_samplingdensities.rdata"))
depthrange_hisamplingdensity <- list_samplingdensities$depthrange_hisamplingdensity
stationdensity_hisamplingdensity <- list_samplingdensities$stationdensity_hisamplingdensity
surveywide_samplingdensity <- list_samplingdensities$surveywide_samplingdensity

# Lengths and otos sampled -------------------------------------------
L0 <- read.csv(here::here("data/local_racebase/length.csv"))
L <- L0 |>
  mutate(YEAR = as.numeric(gsub("(^\\d{4}).*", "\\1", CRUISE)))

length_maxyr_species <- filter(L, YEAR == maxyr & REGION == SRVY) |>
  dplyr::mutate_at(.vars = "SPECIES_CODE", as.character)

length_maxyr_complexes <- length_maxyr_species |>
  dplyr::filter(SPECIES_CODE %in% complex_lookup$species_code) |>
  dplyr::mutate(SPECIES_CODE = case_when(
    SPECIES_CODE %in% complex_lookup$species_code[which(complex_lookup$complex == "OROX")] ~ "OROX",
    SPECIES_CODE %in% complex_lookup$species_code[which(complex_lookup$complex == "REBS")] ~ "REBS",
    SPECIES_CODE %in% complex_lookup$species_code[which(complex_lookup$complex == "OFLATS")] ~ "OFLATS",
    SPECIES_CODE %in% complex_lookup$species_code[which(complex_lookup$complex == "DEEPFLATS")] ~ "DEEPFLATS",
    SPECIES_CODE %in% complex_lookup$species_code[which(complex_lookup$complex == "DSROX")] ~ "DSROX",
    SPECIES_CODE %in% complex_lookup$species_code[which(complex_lookup$complex == "NRSSRS")] ~ "NRSSRS",
    SPECIES_CODE %in% complex_lookup$species_code[which(complex_lookup$complex == "SWFLATS")] ~ "SWFLATS",
    SPECIES_CODE %in% complex_lookup$species_code[which(complex_lookup$complex == "SHARKS")] ~ "SHARKS",
    SPECIES_CODE %in% complex_lookup$species_code[which(complex_lookup$complex == "SKATES")] ~ "SKATES",
    SPECIES_CODE %in% complex_lookup$species_code[which(complex_lookup$complex == "THORNYHEADS")] ~ "THORNYHEADS",
    .default = as.character(SPECIES_CODE)
  ))
unique(length_maxyr_complexes$SPECIES_CODE)

length_maxyr <- bind_rows(length_maxyr_species, length_maxyr_complexes)

# Number of lengths collected per area
lengths_collected <- sum(length_maxyr_species$FREQUENCY) |>
  format(big.mark = ",")

nfishlengths <- sum(length_maxyr_species |>
  filter(LENGTH_TYPE %in% c(1, 5, 11)) |> dplyr::select(FREQUENCY)) |>
  format(big.mark = ",")

nsquidlengths <- sum(length_maxyr_species |>
  filter(LENGTH_TYPE == 12) |> dplyr::select(FREQUENCY)) |>
  format(big.mark = ",")

# Number of otoliths sampled per area
S <- read.csv(here::here("data", "local_racebase", "specimen.csv"))

specimen_maxyr <- S |>
  mutate(YEAR = as.numeric(gsub("(^\\d{4}).*", "\\1", CRUISE))) |>
  filter(YEAR == maxyr & REGION == SRVY)

otos_collected <- specimen_maxyr |>
  filter(SPECIMEN_SAMPLE_TYPE == 1) |> # this means it's an oto collection
  dplyr::left_join(haul_maxyr, by = c(
    "CRUISEJOIN", "HAULJOIN", "CRUISE", "HAUL",
    "REGION", "VESSEL", "YEAR"
  )) |>
  dplyr::left_join(region_lu, by = c("STRATUM")) |>
  group_by(REGULATORY_AREA_NAME, `Depth range`) |>
  dplyr::summarize("Pairs of otoliths collected" = n()) |>
  ungroup() |>
  arrange(factor(REGULATORY_AREA_NAME, levels = district_order))

otos_by_species <- specimen_maxyr |>
  filter(SPECIMEN_SAMPLE_TYPE == 1) |> # this means it's an oto collection
  dplyr::left_join(haul_maxyr, by = c(
    "CRUISEJOIN", "CRUISE", "HAULJOIN", "HAUL",
    "REGION", "VESSEL", "YEAR"
  )) |>
  dplyr::group_by(SPECIES_CODE) |>
  dplyr::summarize("Pairs of otoliths collected" = n()) |>
  ungroup()

# This is used for the presentation only
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
  ) |>
  dplyr::arrange(-`Lengths collected`)

# Hereafter, we want to work with only abundance hauls!
# haul_maxyr <- subset(haul_maxyr,ABUNDANCE_HAUL=="Y")

meanlengths_area <- length_maxyr |>
  dplyr::left_join(haul_maxyr, by = c(
    "CRUISEJOIN", "HAULJOIN",
    "REGION", "VESSEL", "CRUISE", "YEAR", "HAUL"
  )) |>
  dplyr::filter(ABUNDANCE_HAUL == "Y") |>
  dplyr::left_join(stratum_lu, by = c("STRATUM")) |>
  group_by(SPECIES_CODE, REGULATORY_AREA_NAME) |> # , `Depth range`
  dplyr::summarize(
    "N" = sum(FREQUENCY, na.rm = TRUE),
    "Mean length" = weighted.mean(LENGTH, w = FREQUENCY, na.rm = TRUE)
  ) |>
  ungroup()

meanlengths_depth <- length_maxyr |>
  dplyr::left_join(haul_maxyr, by = c(
    "CRUISEJOIN", "HAULJOIN",
    "REGION", "VESSEL", "CRUISE"
  )) |>
  dplyr::filter(ABUNDANCE_HAUL == "Y") |>
  dplyr::left_join(stratum_lu, by = c("STRATUM")) |>
  dplyr::group_by(SPECIES_CODE, `Depth range`) |> # ,
  dplyr::summarize(
    "N" = sum(FREQUENCY, na.rm = TRUE),
    "Mean length" = weighted.mean(LENGTH, w = FREQUENCY, na.rm = TRUE)
  ) |>
  ungroup()

total_otos <- sum(otos_collected$`Pairs of otoliths collected`) |>
  format(big.mark = ",")


# Load pseudolengths file -------------------------------------------------
if (!exists("report_pseudolengths")) {
  report_pseudolengths <- read.csv(file = paste0(dir_out_srvy_yr, "tables/report_pseudolengths.csv"))
}

# Taxonomic diversity -----------------------------------------------------
# get number of fish and invert spps
catch <- read.csv("data/local_racebase/catch.csv", header = TRUE)


# Species with highest est'd biomass --------------------------------------
# Load total biomass table with all species and complexes
if (!exists("biomass_total")) {
  biomass_total <- read.csv(file = paste0(dir_out_srvy_yr, "tables/biomass_total_all.csv"))
}

biomass_maxyr <- biomass_total |>
  filter(YEAR == maxyr & SURVEY_DEFINITION_ID == ifelse(SRVY == "GOA", 47, 52))

highest_biomass <- biomass_maxyr |>
  dplyr::slice_max(n = 50, order_by = BIOMASS_MT, with_ties = FALSE) |>
  janitor::clean_names() |>
  dplyr::left_join(species_names)

highest_biomass_flatfish <- highest_biomass |>
  filter(major_group == "Flatfish")

highest_chonds <- biomass_total |>
  filter(YEAR == maxyr & SURVEY_DEFINITION_ID == ifelse(SRVY == "GOA", 47, 52)) |>
  janitor::clean_names() |>
  dplyr::left_join(species_names) |>
  filter(major_group == "Chondrichthyans") |>
  dplyr::slice_max(n = 3, order_by = biomass_mt, with_ties = FALSE)

highest_biomass_overall <- stringr::str_to_sentence(highest_biomass$common_name[1])
second_highest_biomass_overall <- highest_biomass$common_name[2]
third_highest_biomass_overall <- highest_biomass$common_name[3]
fourth_highest_biomass_overall <- highest_biomass$common_name[4]


# Load tables for CPUE, biomass, etc for ALL species, complexes -----------
# Need:
# - cpue_raw
# - biomass_total
if (!exists("cpue_processed")) {
  cpue_processed <- read.csv(paste0(dir_out_srvy_yr, "tables/cpue_processed.csv")) # Load cpue table with all species and complexes
}

# Species in complexes (presentation) -------------------------------------
if (pres_or_report == "pres") {
  biomass_all <- read.csv("data/local_gap_products/biomass.csv")
  biomass_complexes <- biomass_all |>
    dplyr::filter(YEAR == maxyr &
      SPECIES_CODE %in% complex_lookup$species_code &
      AREA_ID == ifelse(SRVY == "GOA", 99903, 99904)) |>
    left_join(complex_lookup, by = c("SPECIES_CODE" = "species_code"))

  complex_name_text <- biomass_complexes |>
    group_by(complex) |>
    arrange(-BIOMASS_MT) |>
    mutate(common_name = case_when(BIOMASS_MT == 0 ~ paste0(common_name, "*"), TRUE ~ common_name)) |>
    summarise(species = toString(unique(common_name), .groups = "drop"))
}

# Notes and tidbits -------------------------------------------------------

# Random vessel info, not sure where to put this: 1,100 kg (Alaska Provider) or 800 kg (Ocean Explorer) - average catch weight per tow on each boat? Based on 2022 values.
