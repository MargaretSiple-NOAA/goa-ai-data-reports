# This script builds all the tables needed for the report. It is intended to be run after all the preceding files (00 thru 06) so don't try to run it on its own.
if(!maxyr){print("This script requires objects that aren't in the environment yet - make sure you run all the preceding files (00_report-settings.R thru 06_prep_data.R).")}

# Tables are labeled / ordered based on the historical data reports order of tables (currently based on the 2018 AI report, because that's the most recent one I have access to)
# Table 1 is the target sample sizes for different species categories
# Table 2 is the number of stations allocated, attempted, and successfully completed.
# Table 3 onwards are specific to each species.

# Aesthetics --------------------------------------------------------------
# Color of thick border between subdistrict areas
cl <- fp_border(color = "#5A5A5A", width = 3)

# Otolith targets ---------------------------------------------------------
targetn <- read.csv(here::here("data","target_n.csv"))

# Species richness by subregion and family --------------------------------
subregion_fam_div <- appB %>%
  group_by(inpfc_area, family) %>%
  tally(name = "nsp") %>%
  pivot_wider(names_from = inpfc_area, values_from = nsp) %>%
  dplyr::rename(Family = family) %>%
  ungroup() %>%
  mutate_at(2:5, ~ replace_na(., 0)) %>%
  relocate(any_of(c("Family", district_order)))

# Mean CPUE 20 most abundant groundfish spps ------------------------------
#NOTE: This table is different if you produce it using the standard SQL script vs if you produce it by hand or in R. We don't know exactly why these values are very slightly different, but they are! So if we want to reproduce the report in the exact same way, at least for the Aleutians, we have to use a SQL script to produce the table of the top CPUEs by region. 
if (use_sql_cpue) {
  # colnames should be: c("INPFC_AREA", "species_code", "wgted_mean_cpue_kgkm2", "wgted_mean_cpue_kgha", "scientific_name", "common_name", "major_group")
  top_CPUE <- prep_tab2()
} else {
  top_CPUE <- make_top_cpue(
    YEAR = YEAR,
    SRVY = SRVY,
    cpue_raw = cpue_raw
  )
}
if (SRVY == "AI") {
  top_CPUE <- top_CPUE %>%
    arrange(factor(INPFC_AREA, levels = c(
      district_order,
      "All Aleutian Districts",
      "All Areas Combined"
    )))
}
if (SRVY == "GOA") {
  top_CPUE <- top_CPUE %>%
    arrange(factor(INPFC_AREA, levels = district_order))
}


write.csv(
  x = top_CPUE, file = paste0(dir_out_tables, "top_CPUE", "_", maxyr, ".csv"),
  row.names = FALSE
)

# Biomass estimates by area and depth range -------------------------------
# Not needed I don't think yet but I did produce this for Maia for 2022 FHS.
#make_depth_mgmt_area_summary(species_code = 10130)

# % changes in biomass since last survey ----------------------------

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
  group_by(INPFC_AREA, `Depth range`) %>%
  dplyr::count(name = "attempted") %>%
  ungroup()

succeeded <- haul_maxyr %>%
  group_by(STRATUM) %>%
  filter(ABUNDANCE_HAUL == "Y") %>% # filter to successful hauls
  distinct(STATIONID) %>% # how many stations were sampled?
  ungroup() %>%
  left_join(region_lu) %>%
  group_by(INPFC_AREA, `Depth range`) %>%
  dplyr::count(name = "succeeded") %>%
  ungroup()

inpfc_depth_areas <- region_lu %>%
  distinct(INPFC_AREA, STRATUM, AREA, `Depth range`) %>%
  group_by(INPFC_AREA, `Depth range`) %>%
  dplyr::summarize(AREA = sum(AREA)) %>%
  ungroup()


piece1 <- all_allocation %>%
  filter(YEAR == maxyr & SURVEY == SRVY) %>%
  left_join(region_lu) %>%
  group_by(INPFC_AREA, `Depth range`) %>%
  dplyr::count(name = "allocated") %>%
  ungroup() %>%
  left_join(attempted) %>%
  left_join(succeeded) %>%
  left_join(inpfc_depth_areas)

depth_areas <- piece1 %>%
  group_by(INPFC_AREA) %>%
  dplyr::summarize(
    AREA = sum(AREA),
    allocated = sum(allocated),
    attempted = sum(attempted),
    succeeded = sum(succeeded)
  ) %>%
  ungroup() %>%
  mutate(`Depth range` = "All depths")

allocated_prep <- piece1 %>%
  bind_rows(depth_areas) %>%
  arrange(INPFC_AREA, `Depth range`) %>%
  dplyr::arrange(factor(INPFC_AREA,levels = district_order)) %>%
  mutate(stations_per_1000km2 = (succeeded / AREA) * 1000) %>%
  mutate(
    AREA = round(AREA, digits = 1),
    stations_per_1000km2 = round(stations_per_1000km2, digits = 2)
  ) 


all_areas <- allocated_prep %>%
  filter(`Depth range` != "All depths") %>%
  group_by(`Depth range`) %>%
  dplyr::summarize(
    allocated = sum(allocated),
    attempted = sum(attempted),
    succeeded = sum(succeeded),
    AREA = sum(AREA)
  ) %>%
  dplyr::mutate(stations_per_1000km2 = round((succeeded / AREA) * 1000, digits = 2)) %>%
  ungroup() %>%
  tibble::add_column(INPFC_AREA = "All areas", .before = "Depth range")

all_areas_depths <- all_areas %>%
  dplyr::summarize(across(allocated:AREA, sum)) %>%
  tibble::add_column(`Depth range` = "All depths", .before = "allocated") %>%
  ungroup() %>%
  mutate(stations_per_1000km2 = succeeded / AREA) %>%
  tibble::add_column(INPFC_AREA = "All areas", .before = "Depth range") 

allocated_sampled <- bind_rows(allocated_prep, all_areas, all_areas_depths) %>%
  dplyr::arrange(factor(INPFC_AREA,levels = c(district_order,"All areas")))

colnames(allocated_sampled) <- c("Survey district", "Depth range", 
                                 "Stations allocated", "Stations attempted", "Stations completed", 
                                 "Total area", "Stations per 1,000 km^2")

# CPUE table by district - formatted by Paul ------------------------------
#CPUE_table_formatted <- read_xlsx(path = paste0(dir_in_premadetabs,"Table 2/Table 2 AI2022.xlsx"))

# district_depth_cpue_sp_list - list of tables that Paul made -------------
# Check to see if all the species in the list are in the folder
toMatch <- report_species$species_code
matches <- unique(grep(paste(toMatch, collapse = "|"),
  list.files(paste0(dir_in_premadetabs, "Table 3/")),
  value = TRUE
))

print("Checking for tables missing from the G Drive...")
# which species are there tables for?
x <- list.files(paste0(dir_in_premadetabs, "Table 3/"))
y <- sub(pattern = "*_2022.csv", replacement = "", x = x)

lookforme <- as.character(toMatch)[which(!as.character(toMatch) %in% y)]

if (length(lookforme) > 0) {
  print(paste("Check for species", lookforme))
}

table3s_list <- lapply(X = report_species$species_code, FUN = prep_tab3)
names(table3s_list) <- report_species$species_code

table4s_list <- lapply(X = report_species$species_code, FUN = prep_tab4)
names(table4s_list) <- report_species$species_code


# for(i in 1:nrow(report_species)){
#   tab4_sp <- table4s_list[[as.character(report_species$species_code[i])]]
#   tab4_sp <- tab4_sp %>% arrange(desc(`Number of hauls`))
#   write.csv(tab4_sp,file = paste0("C:/Users/margaret.siple/Work/Data reports/goa-ai-data-reports/output/2023-05-09/tables/",maxyr,"_",report_species$spp_name_informal[i],"_Table4.csv"))
#   
#   tab3_sp <- table3s_list[[as.character(report_species$species_code[i])]]
#   write.csv(tab3_sp,file = paste0("C:/Users/margaret.siple/Work/Data reports/goa-ai-data-reports/output/2023-05-09/tables/",maxyr,"_",report_species$spp_name_informal[i],"_Table3.csv"))
# }

# Put together big list...will be edited later ----------------------------

list_tables <- list()
list_tables[["allocated_sampled"]] <- allocated_sampled # Stations allocated and successfully sampled
list_tables[["length-sample-sizes"]] <- targetn  # Target sample size for species/species groups
list_tables[["top_CPUE"]] <- top_CPUE #
#names(list_tables) <- c("allocated_sampled","targetn","top_CPUE")


save(list_tables,
     file = paste0(dir_out_tables,"report_tables.rdata")
)

save(table3s_list,
     file = paste0(dir_out_tables,"table3s_list.rdata")
)

save(table4s_list,
     file = paste0(dir_out_tables,"table4s_list.rdata")
)

