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
           ) %>%
  knitr::kable() 


sampled_stations <- data.frame(
  INPFC_area = c("Shumagin","Chirikof", "Kodiak","Yakutat","Southeastern", "All areas"), 
                               Stations_allocated = NA) # there is sql code for this in sql/

common_names <- read.csv("data/local_racebase/species.csv",header = TRUE)
species_names <- common_names %>% 
  janitor::clean_names() %>%
  dplyr::rename(scientific_name = species_name) %>%
  dplyr::select(-year_added)
  

top_CPUE <- make_top_cpue(YEAR = YEAR, SRVY = SRVY,cpue_raw = cpue_raw)


# Biomass estimates by area and depth range -------------------------------
biomass_stratum <- read.csv("data/local_goa/biomass_stratum.csv") # where biomass_total.csv is GOA.BIOMASS_TOTAL downloaded from Oracle as csv - janky but will have to work for now

depth_mgmtarea_summary <- biomass_stratum %>%
  filter(SPECIES_CODE == 10130) %>% # FHS
  left_join(region_lu, by = c("SURVEY", "STRATUM")) %>%
  dplyr::select(YEAR, REGULATORY_AREA_NAME, `Depth range`, STRATUM_BIOMASS) %>%
  dplyr::group_by(YEAR, REGULATORY_AREA_NAME, `Depth range`) %>% #
  dplyr::summarize(total_biomass = sum(STRATUM_BIOMASS, na.rm = TRUE)) %>%
  dplyr::ungroup()

# write.csv(dat2,file = "FHS_area_depth.csv",row.names = FALSE)

# Check
# dat2 %>%
#   group_by(YEAR) %>%
#   dplyr::summarize(total_biomass=sum(total_biomass))


list_tables <- list()
list_tables[[1]] <- targetn # Target sample size for species/species groups
list_tables[[2]] <- sampled_stations
list_tables[[3]] <- top_CPUE

save(list_tables,
     file = paste0(dir_out_tables,"report_tables.rdata")
)



