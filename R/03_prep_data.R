# 03_prep_data
# Read in local copies of all the "stuff" needed for making tables and figs.
# This takes a while because a lot of these are big tables.
# Note: if you've downloaded all the local versions of the tables, you should be able to run this script without internet access or anything. If it fails, make an issue!

# Tables from RACEBASE ----------------------------------------------------
# Get species table
if (SRVY == "AI") report_species <- read.csv("data/ai_report_specieslist.csv")
report_species <- report_species %>%
  arrange(-species_code) %>%
  filter(report==1)

pres_species <- report_species %>%
  arrange(-species_code) %>%
  filter(presentation==1)

# haul info (source: RACEBASE)
haul <- read.csv(here::here("data/local_racebase/haul.csv"))

# species ID info (source: RACEBASE)
common_names <- read.csv("data/local_racebase/species.csv", header = TRUE)
species_names <- common_names %>%
  janitor::clean_names() %>%
  dplyr::rename(scientific_name = species_name) %>%
  dplyr::select(-year_added) %>%
  mutate(major_group = case_when(
    species_code >= 10000 & species_code <= 19999 ~ "Flatfish",
    species_code >= 20000 & species_code <= 39999 ~ "Roundfish",
    species_code >= 30000 & species_code <= 36999 ~ "Rockfish",
    species_code >= 40000 & species_code <= 99990 ~ "Invertebrates",
    species_code >= 00150 & species_code <= 00799 ~ "Chondrichthyans"
  )) # add column for species category

# This year's haul data
haul_maxyr <- haul %>%
  mutate(YEAR = as.numeric(gsub("(^\\d{4}).*", "\\1", CRUISE))) %>% # extract year
  filter(REGION == SRVY & YEAR == maxyr)

# Temperature data --------------------------------------------------------
minbottomtemp <- min(haul_maxyr$GEAR_TEMPERATURE, na.rm = T)
maxbottomtemp <- max(haul_maxyr$GEAR_TEMPERATURE, na.rm = T)

minsurfacetemp <- min(haul_maxyr$SURFACE_TEMPERATURE, na.rm = T)
maxsurfacetemp <- max(haul_maxyr$SURFACE_TEMPERATURE, na.rm = T)


# Tables from AI/GOA schemas ----------------------------------------------
# cpue (source: AI or GOA schema)
# NOTE: THis does not contain inverts and weird stuff! There are only 76 spps in here.
x <- read.csv(file = here::here("data/local_ai/cpue.csv"), header = TRUE) # This is already 0-filled
cpue_raw <- x %>%
  left_join(common_names) %>%
  dplyr::select(-YEAR_ADDED) %>%
  dplyr::left_join(haul) %>%
  janitor::clean_names() %>% # need to add common name lookup
  dplyr::rename(cpue_kgkm2 = wgtcpue) %>%
  janitor::clean_names()
 
# Biomass by stratum (source: AI or GOA schema)
biomass_stratum <- read.csv(here::here("data", "local_ai", "biomass_stratum.csv"))
# where biomass_stratum.csv is GOA.BIOMASS_STRATUM or AI.BIOMASS_STRATUM downloaded from Oracle as csv - janky but will have to work for now

# Total biomass across survey area - currently reads from local copy; download/update to new one by running the setup script again and downloading fresh tables from oracle)
biomass_total <- read.csv(here::here("data", "local_ai", "biomass_total.csv"))

# Station allocation table (source: AI or GOA schema)
all_allocation <- read.csv("data/local_ai/ai_station_allocation.csv")

# Get a table of the strata and depths / regions (source: AI or GOA schema)
dat <- read.csv("data/goa_strata.csv", header = TRUE)
region_lu <- dat %>%
  filter(SURVEY == SRVY) %>%
  dplyr::select(SURVEY, STRATUM, INPFC_AREA, MIN_DEPTH, MAX_DEPTH, REGULATORY_AREA_NAME, AREA) %>%
  filter(STRATUM <= 794) %>% # STRATUM >=211 &
  tidyr::unite("Depth range", MIN_DEPTH:MAX_DEPTH, sep = " - ", remove = FALSE) %>%
  mutate(`Depth range` = paste0(`Depth range`, " m")) %>%
  mutate(INPFC_AREA = str_trim(INPFC_AREA))

region_lu2 <- region_lu %>%
  dplyr::group_by(INPFC_AREA) %>%
  dplyr::summarize(INPFC_AREA_AREA_km2 = sum(AREA, na.rm = T)) %>%
  dplyr::ungroup()

# Add Aleutian areas
INPFC_areas <- region_lu2 %>%
  tibble::add_row(
    INPFC_AREA = "All Aleutian Districts",
    INPFC_AREA_AREA_km2 = sum(filter(region_lu2, INPFC_AREA != "Southern Bering Sea")$INPFC_AREA_AREA_km2)
  ) %>%
  tibble::add_row(
    INPFC_AREA = "All Districts",
    INPFC_AREA_AREA_km2 = sum(filter(region_lu2)$INPFC_AREA_AREA_km2)
  )

# individual values needed for report (e.g., most abundant species) -------
nyears <- length(unique(filter(haul, REGION == SRVY)$CRUISE))

haul2 <- haul %>%
  mutate(YEAR = stringr::str_extract(CRUISE, "^\\d{4}")) %>%
  filter(YEAR == maxyr & REGION == SRVY)

nstations <- haul2 %>%
  filter(ABUNDANCE_HAUL == "Y") %>%
  distinct(STATIONID) %>%
  nrow()

nsuccessfulhauls <- haul2 %>%
  filter(ABUNDANCE_HAUL == "Y") %>%
  nrow() # 420 in 2018


# N lengths and otoliths sampled -------------------------------------------
L <- read.csv(here::here("data/local_racebase/length.csv"))
L <- L %>%
  mutate(YEAR = as.numeric(gsub("(^\\d{4}).*", "\\1", CRUISE)))
length_maxyr <- filter(L, YEAR == maxyr & REGION == SRVY)

# Number of lengths collected per area
lengths_collected <- nrow(length_maxyr)

# Number of otoliths sampled per area
S <- read.csv(here::here("data/local_racebase/specimen.csv"))
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
  dplyr::summarize("Number of otoliths collected" = n()) %>%
  ungroup()

total_otos <- sum(otos_collected$`Number of otoliths collected`)

# get number of fish and invert spps
catch <- read.csv("data/local_racebase/catch.csv", header = TRUE)


# Species with highest est'd biomass --------------------------------------
highest_biomass <- biomass_total %>%
  filter(YEAR == maxyr & SURVEY == SRVY) %>%
  dplyr::slice_max(n = 50, order_by = TOTAL_BIOMASS, with_ties = FALSE) %>%
  janitor::clean_names() %>%
  dplyr::left_join(species_names)

highest_biomass_flatfish <- highest_biomass %>%
  filter(major_group == "Flatfish")

highest_skates <- biomass_total %>%
  filter(YEAR == maxyr & SURVEY == SRVY) %>%
  janitor::clean_names() %>%
  dplyr::left_join(species_names) %>%
  filter(major_group == "Chondrichthyans") %>%
  dplyr::slice_max(n = 3, order_by = total_biomass, with_ties = FALSE)

# Stats for the species in report_spps ------------------------------------
head(report_species)
biomass_total %>%
  filter(YEAR == maxyr) %>%
  janitor::clean_names() %>%
  right_join(report_species)
