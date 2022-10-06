# 08_prep_data
# Get all the "stuff" needed for making tables and figs


# Tables from RACEBASE ----------------------------------------------------
# Get species table
if (SRVY == "AI") report_species <- read.csv("data/ai_report_specieslist.csv")

# haul info (source: RACEBASE)
hauls <- read.csv(here::here("data/local_racebase/haul.csv"))

# species ID info (source: RACEBASE)
common_names <- read.csv("data/local_racebase/species.csv", header = TRUE)
species_names <- common_names %>%
  janitor::clean_names() %>%
  dplyr::rename(scientific_name = species_name) %>%
  dplyr::select(-year_added)


# Tables from AI/GOA schemas ----------------------------------------------

# cpue (source: AI or GOA schema)
x <- read.csv(file = here::here("data/local_ai/cpue.csv"), header = TRUE)
cpue_raw <- x %>%
  left_join(common_names) %>%
  dplyr::select(-YEAR_ADDED) %>%
  dplyr::left_join(hauls) %>%
  janitor::clean_names() %>% # need to add common name lookup
  dplyr::rename(cpue_kgkm2 = wgtcpue) %>%
  janitor::clean_names()

# Biomass by stratum (source: AI or GOA schema)
biomass_stratum <- read.csv("data/local_ai/biomass_stratum.csv") 
# where biomass_stratum.csv is GOA.BIOMASS_STRATUM downloaded from Oracle as csv - janky but will have to work for now

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

# if(SRVY=="GOA"){
# region_lu <- region_lu %>%
#   dplyr::mutate(MGMT_AREA = case_when(INPFC_AREA %in% c("Chirikof","Kodiak") ~ "Central",
#                                INPFC_AREA %in% c("Yakutat","Southeastern") ~ "Eastern",
#                                INPFC_AREA == "Shumagin" ~ "Western"))
# }