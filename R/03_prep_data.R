# 03_prep_data
# Read in local copies of all the "stuff" needed for making tables and figs
# This takes a while because a lot of these are big tables.
# Note: if you've downloaded all the local versions of the tables, you should be able to run this script without internet access or anything. If it fails, make an issue!

# Tables from RACEBASE ----------------------------------------------------
# Get species table
if (SRVY == "AI") report_species <- read.csv("data/ai_report_specieslist.csv")
report_species <- report_species %>% 
  arrange(-species_code)

# haul info (source: RACEBASE)
haul <- read.csv(here::here("data/local_racebase/haul.csv"))

# species ID info (source: RACEBASE)
common_names <- read.csv("data/local_racebase/species.csv", header = TRUE)
species_names <- common_names %>%
  janitor::clean_names() %>%
  dplyr::rename(scientific_name = species_name) %>%
  dplyr::select(-year_added)


# Temperature data --------------------------------------------------------
minbottomtemp <- min(haul_maxyr$GEAR_TEMPERATURE, na.rm = T)
maxbottomtemp <- max(haul_maxyr$GEAR_TEMPERATURE, na.rm = T)

minsurfacetemp <- min(haul_maxyr$SURFACE_TEMPERATURE, na.rm = T)
maxsurfacetemp <- max(haul_maxyr$SURFACE_TEMPERATURE, na.rm = T)


# Tables from AI/GOA schemas ----------------------------------------------
# cpue (source: AI or GOA schema)
# NOTE: THis does not contain inverts and weird stuff! There are only 76 spps in here.
x <- read.csv(file = here::here("data/local_ai/cpue.csv"), header = TRUE)
cpue_raw <- x %>%
  left_join(common_names) %>%
  dplyr::select(-YEAR_ADDED) %>%
  dplyr::left_join(haul) %>%
  janitor::clean_names() %>% # need to add common name lookup
  dplyr::rename(cpue_kgkm2 = wgtcpue) %>%
  janitor::clean_names()

# Biomass by stratum (source: AI or GOA schema)
biomass_stratum <- read.csv(here::here("data","local_ai","biomass_stratum.csv")) 
# where biomass_stratum.csv is GOA.BIOMASS_STRATUM or AI.BIOMASS_STRATUM downloaded from Oracle as csv - janky but will have to work for now

# Total biomass across survey area - currently reads from local copy; download/update to new one by running the setup script again and downloading fresh tables from oracle)
biomass_total <- read.csv(here::here("data","local_ai","biomass_total.csv"))

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


# individual values needed for report (e.g., most abundant species) -------
nyears <- length(unique(filter(haul, REGION==SRVY)$CRUISE))

haul2 <- haul %>%
  mutate(YEAR = stringr::str_extract(CRUISE, "^\\d{4}")) %>%
  filter(YEAR == maxyr & REGION == SRVY)

nstations <- haul2 %>%
  filter(ABUNDANCE_HAUL == "Y") %>%
  distinct(STATIONID) %>%
  nrow()

nsuccessfulhauls <- haul2 %>%
  filter(ABUNDANCE_HAUL == "Y") %>%
  nrow() #420 in 2018

# get number of fish and invert spps
catch <- read.csv("data/local_racebase/catch.csv",header = TRUE)


# Species with highest est'd biomass --------------------------------------
highest_biomass <- biomass_total %>%
  filter(YEAR==maxyr & SURVEY==SRVY) %>%
  dplyr::slice_max(n = 5, order_by = TOTAL_BIOMASS, with_ties = FALSE) %>%
  janitor::clean_names() %>%
  dplyr::left_join(species_names)


# Stats for the species in report_spps ------------------------------------
head(report_species)
biomass_total %>% 
  filter(YEAR==maxyr) %>% 
  janitor::clean_names() %>% 
  right_join(report_species)
