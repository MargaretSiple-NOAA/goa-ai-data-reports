#08_values

# Get all the "stuff" needed for making tables and figs -------------------
# Get species table
if(SRVY=="AI") report_species <- read.csv("data/ai_report_specieslist.csv")

# haul info (source: RACEBASE)
hauls <- read.csv(here::here("data/local_racebase/haul.csv"))

# species ID info (source: RACEBASE)
common_names <- read.csv("data/local_racebase/species.csv",header = TRUE)
species_names <- common_names %>% 
  janitor::clean_names() %>%
  dplyr::rename(scientific_name = species_name) %>%
  dplyr::select(-year_added)

# cpue (source: AI or GOA schema)
x <- read.csv(file = here::here("data/local_ai/cpue.csv"),header = TRUE)
cpue_raw <- x %>%
  left_join(common_names) %>%
  dplyr::select(-YEAR_ADDED) %>%
  janitor::clean_names() %>% # need to add common name lookup
  dplyr::rename(cpue_kgkm2 = wgtcpue) %>%
  left_join(hauls)

# Biomass by stratum (source: AI or GOA schema)
biomass_stratum <- read.csv("data/local_ai/biomass_stratum.csv") # where biomass_stratum.csv is GOA.BIOMASS_STRATUM downloaded from Oracle as csv - janky but will have to work for now

# Station allocation table (source: AI or GOA schema)
all_allocation <- read.csv("data/local_ai/ai_station_allocation.csv")

