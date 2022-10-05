# This script should load the necessary data and build PNGs of all the figures for the paper, so they can be accessed in the Markdown part.


# Tables ------------------------------------------------------------------
# Table 1 is the target sample sizes for different species categories
# Table 2 is the number of stations allocated, attempted, and successfully completed.
# Table 3 onwards are specific to each species.
# targetn <- read.csv("data/TargetN.csv",header = TRUE) # csv was corrupted
targetn <- data.frame(
          stringsAsFactors = FALSE,
             `Species or species group` = c("Walleye pollock",
                                          "Pacific cod",
                                          "Arrowtooth flounder (ATF)",
                                          "All rockfish species",
                                          "Sablefish",
                                          "Atka mackerel",
                                          "All species of flatfish (except ATF)",
                                          "Skates and Sharks (total length)",
                                          "Grenadiers (tip of snout to insertion of first anal ray)",
                                          "Prowfish",
                                          "Lingcod",
                                          "Salmon",
                                          "Yellow Irish lord (*Hemilepidotus jordani*)",
                                          "Bigmouth sculpin (*Hemilepidotus bolini*)",
                                          "Great sculpin (*Myoxocephalus polyacnthocephalus*)",
                                          "Plain sculpin (*Myoxocephalus jaok*)",
                                          "Warty sculpin (*Myoxocephalus verrucosus*)",
                                          "Forage fish (herring, eulachon, capelin, sand lance)",
                                          "Commander squid (*Berryteuthis magister*)"),
                   `Target sample size` = c("100","100","150","100",
                                          "100","100","100","50","50","All",
                                          "All","All","All","All","All",
                                          "All","All","All","All")
           )


common_names <- read.csv("data/local_racebase/species.csv",header = TRUE)
species_names <- common_names %>% 
  janitor::clean_names() %>%
  dplyr::rename(scientific_name = species_name) %>%
  dplyr::select(-year_added)
  

x <- read.csv(file = here::here("data/local_ai/cpue.csv"),header = TRUE)
cpue_raw <- x %>%
  left_join(common_names) %>%
  dplyr::select(-YEAR_ADDED) %>%
  janitor::clean_names() %>% # need to add common name lookup
  dplyr::rename(cpue_kgkm2 = wgtcpue)
  
top_CPUE <- make_top_cpue(YEAR = YEAR, 
                          SRVY = SRVY,
                          cpue_raw = cpue_raw)


# Biomass estimates by area and depth range -------------------------------
biomass_stratum <- read.csv("data/local_goa/biomass_stratum.csv") # where biomass_stratum.csv is GOA.BIOMASS_STRATUM downloaded from Oracle as csv - janky but will have to work for now

# **** TURN INTO FUNCTION
depth_mgmtarea_summary <- biomass_stratum %>%
  filter(SPECIES_CODE == 10130) %>% # FHS
  left_join(region_lu, by = c("SURVEY", "STRATUM")) %>%
  dplyr::select(YEAR, REGULATORY_AREA_NAME, `Depth range`, STRATUM_BIOMASS) %>%
  dplyr::group_by(YEAR, REGULATORY_AREA_NAME, `Depth range`) %>% #
  dplyr::summarize(total_biomass = sum(STRATUM_BIOMASS, na.rm = TRUE)) %>%
  dplyr::ungroup()

# Check
# dat2 %>%
#   group_by(YEAR) %>%
#   dplyr::summarize(total_biomass=sum(total_biomass))


# Stations allocated, attempted, succeeded --------------------------------
# This year's hauls
hauls <- read.csv(here::here("data/local_racebase/haul.csv"))

hauls_maxyr <- hauls %>%
  mutate(YEAR = as.numeric(gsub("(^\\d{4}).*", "\\1", CRUISE))) %>% # extract year
  filter(REGION == SRVY & YEAR == maxyr)

attempted <- hauls_maxyr %>%
  group_by(STRATUM) %>%
  distinct(STATIONID) %>% # how many stations were sampled?
  ungroup() %>%
  left_join(region_lu) %>%
  group_by(INPFC_AREA,`Depth range`) %>%
  dplyr::count(name = "attempted") %>%
  ungroup()

succeeded <-  hauls_maxyr %>%
  group_by(STRATUM) %>%
  filter(ABUNDANCE_HAUL=="Y") %>% # filter to successful hauls
  distinct(STATIONID) %>% # how many stations were sampled?
  ungroup() %>%
  left_join(region_lu) %>%
  group_by(INPFC_AREA,`Depth range`) %>%
  dplyr::count(name = "succeeded") %>%
  ungroup()

region_areas <- region_lu %>%
  distinct(INPFC_AREA, STRATUM, AREA, `Depth range`) %>%
  group_by(INPFC_AREA,`Depth range`) %>%
  #summarize(INPFC_AREA_area = sum(AREA)) %>%
  ungroup()

all_allocation <- read.csv("data/local_ai/ai_station_allocation.csv")

allocated_sampled <- all_allocation %>%
  filter(YEAR == maxyr & SURVEY == SRVY) %>%
  left_join(region_lu) %>%
  group_by(INPFC_AREA, `Depth range`) %>%
  dplyr::count(name = "allocated") %>%
  ungroup() %>%
  left_join(attempted) %>%
  left_join(succeeded) %>%
  left_join(region_areas) %>%
  mutate(stations_per_1000km2 = (succeeded/AREA) * 1000) 
# %>%
#   knitr::kable(caption = paste("Stations allocated and successfully sampled in",maxyr)) 


list_tables <- list()
list_tables[[1]] <- allocated_sampled # Stations allocated and successfully sampled
list_tables[[2]] <- targetn  # Target sample size for species/species groups
list_tables[[3]] <- top_CPUE

save(list_tables,
     file = paste0(dir_out_tables,"report_tables.rdata")
)



