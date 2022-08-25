# This script should load the necessary data and build PNGs of all the figures for the paper, so they can be accessed in the Markdown part.


# Tables ------------------------------------------------------------------
# Table 1 is the target sample sizes for different species categories
# Table 2 is the number of stations allocated, attempted, and successfully completed.
# Table 3 onwards are specific to each species.
# targetn <- read.csv("data/TargetN.csv",header = TRUE) # csv was corrupted
targetn <- data.frame(
          stringsAsFactors = FALSE,
             Species.or.species.group = c("Walleye pollock",
                                          "Pacific cod","Arrowtooth flounder (ATF)",
                                          "All rockfish species","Sablefish",
                                          "Atka mackerel",
                                          "All species of flatfish (except ATF)",
                                          "Skates and Sharks (total length)",
                                          "Grenadiers (tip of snout to insertion of first anal ray)",
                                          "Prowfish","Lingcod","Salmon",
                                          "Yellow Irish lord (Hemilepidotus jordani)",
                                          "Bigmouth sculpin (Hemilepidotus bolini)",
                                          "Great sculpin (Myoxocephalus polyacnthocephalus)",
                                          "Plain sculpin (Myoxocephalus jaok)",
                                          "Warty sculpin (Myoxocephalus verrucosus)",
                                          "Forage fish (herring, eulachon, capelin, sand lance)",
                                          "Commander squid (Berryteuthis magister)"),
                   Target.sample.size = c("100","100","150","100",
                                          "100","100","100","50","50","All",
                                          "All","All","All","All","All",
                                          "All","All","All","All")
           )
sampled_stations <- data.frame(INPFC_area = c("Shumagin","Chirikof", "Kodiak","Yakutat","Southeastern", "All areas"), 
                               Stations_allocated = NA) # there is sql code for this in sql/


dat <- read.csv("data/goa_strata.csv",header= TRUE)
region_lu <- dat %>% 
  filter(SURVEY==SRVY) %>%
  dplyr::select(SURVEY, STRATUM, INPFC_AREA) 

common_names <- read.csv("data/local_racebase/species.csv",header = TRUE)
species_names <- common_names %>% 
  janitor::clean_names() %>%
  dplyr::rename(scientific_name = species_name) %>%
  dplyr::select(-year_added)
  
make_top_cpue <- function(YEAR, SRVY){ # Gives top 20 spps for each region
  x <- cpue_raw %>% 
    filter(year==YEAR & srvy==SRVY) %>%
    dplyr::mutate(taxon = dplyr::case_when(
      species_code <= 31550 ~ "fish", 
      species_code >= 40001 ~ "invert")) %>%
    dplyr::filter(taxon == "fish") %>%
    dplyr::mutate(common_name = case_when(species_code >= 30050 & species_code<=30052 ~ "Rougheye / blackspotted rockfish complex",
                 TRUE ~ common_name)) %>%
    # Old skate check
    #dplyr::filter(species_code >=400 & species_code<=495) %>%
    left_join(region_lu, by=c('stratum'='STRATUM')) %>%
    dplyr::group_by(INPFC_AREA, common_name) %>%
    dplyr::summarize(mean_cpue = mean(cpue_kgkm2)) %>%
    dplyr::slice_max(n = 20, order_by = mean_cpue, with_ties = FALSE) %>%
    dplyr::ungroup() %>%
    dplyr::left_join(species_names)
  return(x)
}

top_CPUE <- make_top_cpue(YEAR = YEAR, SRVY = SRVY)

list_tables <- list()
list_tables[[1]] <- targetn
list_tables[[2]] <- sampled_stations
list_tables[[3]] <- top_CPUE

save(list_tables,
     file = paste0(dir_out_tables,"report_tables.rdata")
)

