# This script should load the necessary data and build PNGs of all the figures for the paper, so they can be accessed in the Markdown part.
# Tables ------------------------------------------------------------------
# Tables are labeled / ordered based on the historical data reports order of tables (currently based on the 2018 AI report, because that's the most recent one I have access to)
# Table 1 is the target sample sizes for different species categories
# Table 2 is the number of stations allocated, attempted, and successfully completed.
# Table 3 onwards are specific to each species.

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




# Mean CPUE for 20 most abundant groundfish spps --------------------------

top_CPUE <- make_top_cpue(YEAR = YEAR, 
                          SRVY = SRVY,
                          cpue_raw = cpue_raw)

write.csv(x = top_CPUE,file = paste0(dir_out_tables,"top_CPUE","_",maxyr,".csv"),row.names = FALSE)

# Biomass estimates by area and depth range -------------------------------
# Not needed I don't think yet but I did produce this for Maia for 2022 FHS.
#make_depth_mgmt_area_summary(species_code = 10130)

# Percent changes in biomass since last survey ----------------------------

head(biomass_total)

compare_tab <- biomass_total %>% 
  filter(YEAR %in% c(maxyr, compareyr) & 
           SPECIES_CODE %in% report_species$species_code) %>%
  dplyr::select(YEAR, SPECIES_CODE, TOTAL_BIOMASS) %>%
  dplyr::arrange(YEAR) %>%
  tidyr::pivot_wider(names_from = YEAR,
                     values_from = TOTAL_BIOMASS,
                     names_prefix = "yr_") %>%
  as.data.frame()

compare_tab$percent_change <- round((compare_tab[,3] - compare_tab[,2]) / compare_tab[,2] * 100, digits = 1)
names(compare_tab)

compare_tab2 <- compare_tab %>%
  left_join(report_species, by = c("SPECIES_CODE"="species_code")) %>%
  arrange(-SPECIES_CODE) 

write.csv(x = compare_tab2,
          file = paste0(dir_out_chapters,"comparison_w_previous_year.csv"))

# Stations allocated, attempted, succeeded --------------------------------

attempted <- haul_maxyr %>%
  group_by(STRATUM) %>%
  distinct(STATIONID) %>% # how many stations were attempted sampled?
  ungroup() %>%
  left_join(region_lu) %>%
  group_by(INPFC_AREA,`Depth range`) %>%
  dplyr::count(name = "attempted") %>%
  ungroup()

succeeded <-  haul_maxyr %>%
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
  ungroup()


allocated_sampled <- all_allocation %>%
  filter(YEAR == maxyr & SURVEY == SRVY) %>%
  left_join(region_lu) %>%
  group_by(INPFC_AREA, `Depth range`) %>%
  dplyr::count(name = "allocated") %>%
  ungroup() %>%
  left_join(attempted) %>%
  left_join(succeeded) %>%
  left_join(region_areas) %>%
  mutate(stations_per_1000km2 = (succeeded/AREA) * 1000) %>%
  mutate(AREA = round(AREA, digits = 1),
         stations_per_1000km2 = round(stations_per_1000km2, digits = 2)) %>%
  dplyr::select(-STRATUM)
colnames(allocated_sampled) <- c("INFPC area","Depth range","Allocated","Attempted","Succeeded","Total area","Stations per 1,000 km^2")


# district_depth_effort_sp_list - list of tables that Paul made -----------
district_depth_effort_sp_list <- list()
names(district_depth_effort_sp_list) <- 
# Get all the xls files in the folder where they're stored. 



# district_depth_cpue_sp_list - list of tables that Paul made -------------
district_depth_cpue_sp_list <- list()



list_tables <- list()
list_tables[[1]] <- allocated_sampled # Stations allocated and successfully sampled
list_tables[[2]] <- targetn  # Target sample size for species/species groups
list_tables[[3]] <- top_CPUE

save(list_tables,
     file = paste0(dir_out_tables,"report_tables.rdata")
)



