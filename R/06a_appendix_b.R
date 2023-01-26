# RACEBASE tables ----------------------------------------------------
# get catch and taxonomy info
catch <- read_csv("data/local_racebase/catch.csv") %>%
  mutate(YEAR = stringr::str_extract(CRUISE, "^\\d{4}")) 

taxonomy <- read_csv("data/local_racebase/species_classification.csv")

species_codes <- read_csv("data/local_race_data/race_species_codes.csv") %>%
  dplyr::select("SPECIES_CODE", "SPECIES_NAME", "COMMON_NAME")



# Getting species from year ----------------------------------------------------

# filtering to just species caught this survey year and getting subregion info
catch_maxyr <- catch %>%
  filter(YEAR == maxyr & REGION == SRVY) %>%
  left_join(haul, by = c("CRUISEJOIN", "HAULJOIN")) %>% # need to merge with haul df to get stratum
  left_join(region_lu, by = "STRATUM") %>%
  dplyr::select("SPECIES_CODE", "INPFC_AREA", "START_LONGITUDE", "START_LATITUDE", "BOTTOM_DEPTH") %>%
  unique()


# non species indicator strings
rm_bits <- paste0(
  c(" egg", "egg case", "larva", "larvae", " tubes", "sp\\.$"), 
  collapse = "|"
  )


# limiting to only taxa identified to species level (i.e. no genus, etc. level IDs)
species_maxyr <- catch_maxyr %>%
  left_join(species_codes) %>%
  mutate(SPECIES_NAME = trimws(SPECIES_NAME),
         level = case_when(
           str_detect(SPECIES_NAME, rm_bits) | !str_detect(SPECIES_NAME, " ") | is.na(SPECIES_NAME) ~ "",
           TRUE ~ "species"
         )) %>% 
  filter(level == "species") %>%
  dplyr::select(-level)





# Finding outliers ----------------------------------------------------

# checking for species that were caught this year that are suspicious/need manual checking using DBSCAN/past confirmed records



#all catch/haul data to check against
catch_haul <- catch %>%
  filter(SPECIES_CODE %in% species_maxyr$SPECIES_CODE) %>%
  left_join(haul, by = c("CRUISEJOIN", "HAULJOIN")) %>%
  left_join(species_codes, by ="SPECIES_CODE") %>%
  # mutate(START_LONGITUDE = ifelse(START_LONGITUDE < 0, 
  #                                 START_LONGITUDE, START_LONGITUDE*-1)) %>%
  dplyr::select(SPECIES_CODE, SPECIES_NAME, START_LONGITUDE,
                START_LATITUDE, GEAR_DEPTH, YEAR)


# outlier species from this year
outlier_spp <- species_maxyr %>%
  dplyr::select(SPECIES_CODE) %>%
  unique() %>%
  mutate(outlier = purrr::map(SPECIES_CODE, ~check_outlier(.x, maxyr, catch_haul))) %>%
  unnest(cols = outlier) %>%
  left_join(species_codes) %>%
  dplyr::select(SPECIES_CODE, SPECIES_NAME) %>%
  unique() 
  


# # plots outliers to pdf document -- FOR SARAH
# pdf(paste0("output/outliers_", maxyr, ".pdf"))
# outlier_spp %>%
#   mutate(g = purrr::map(SPECIES_CODE, ~check_outlier(.x, maxyr, catch_haul, plot = T)))
# dev.off()





# Generate tables/stats ----------------------------------------------------

# Table for Appendix B
appB <- species_maxyr %>%
  left_join(taxonomy, by = "SPECIES_CODE") %>%
  left_join(species_codes) %>%
  janitor::clean_names() %>%
  mutate(major_group = case_when(
    species_code >= 10000 & species_code <= 19999 ~ "Flatfish",
    species_code >= 20000 & species_code <= 39999 ~ "Roundfish",
    species_code >= 30000 & species_code <= 36999 ~ "Rockfish",
    species_code >= 40000 & species_code <= 99990 ~ "Invertebrates",
    species_code >= 00150 & species_code <= 00799 ~ "Chondrichthyans"
  )) %>%
  dplyr::mutate(tax_group = dplyr::case_when(
    species_code <= 31550 ~ "fish",
    species_code >= 40001 ~ "invert"
  )) %>%
  dplyr::select(inpfc_area, species_name, common_name, family = family_taxon, phylum = phylum_taxon, major_group, tax_group) %>%
  unique() %>%
  arrange(inpfc_area, tax_group, major_group, species_name)




# diversity by subregion
subregion_diversity <- appB %>%
  group_by(inpfc_area, tax_group) %>%
  tally(name = "nsp") %>%
  pivot_wider(names_from = tax_group, values_from = nsp)
subregion_diversity



# statement for text
total_diversity <- appB %>%
  dplyr::select(-inpfc_area) %>%
  unique()

n_fish <- total_diversity %>% filter(tax_group == "fish")
n_fam <- length(unique(n_fish$family))
n_inverts <- total_diversity %>% filter(tax_group == "invert")
n_phyla <- length(unique(n_inverts$phylum))

tax_summary_sentence <- paste(
  "Total catches across the survey area were",
  nrow(n_fish), "fish species from", n_fam, "families and",
  nrow(n_inverts), "invertebrate species or taxa from", n_phyla, "phyla"
)

cat(tax_summary_sentence)

