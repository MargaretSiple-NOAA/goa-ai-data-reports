# This script builds all the tables needed for the report. It is intended to be run after all the preceding files (00 thru 06) so don't try to run it on its own.
if (!maxyr) {
  print("This script requires objects that aren't in the environment yet - make sure you run all the preceding files (00_report_settings.R thru 06_prep_data.R).")
}

# Tables are labeled / ordered based on the historical data reports order of tables (currently based on the 2018 AI report, because that's the most recent one I have access to)
# "Table 1" is the target sample sizes for different species categories
# "Table 2" is the number of stations allocated, attempted, and successfully completed.
# "Table 3" onwards are specific to each species.

# Aesthetics --------------------------------------------------------------
# Color of thick border between subdistrict areas
cl <- fp_border(color = "#5A5A5A", width = 3)


# Data tables needed ------------------------------------------------------
if (!exists("biomass_stratum_complexes")) {
  biomass_stratum_complexes <- read.csv(paste0(dir_out_srvy_yr, "tables/biomass_stratum_complexes.csv"))
}
# Length targets ----------------------------------------------------------
targetn <- read.csv(here::here("data", "target_n.csv"))

# Otolith targets ---------------------------------------------------------

df <- read.csv(here::here("data", paste0(SRVY, maxyr, "_otolith_targets.csv")))

otos_target_sampled <- df |>
  dplyr::mutate(percent.diff = round((collection - target) / target * 100)) |>
  dplyr::select(species, collection, target, percent.diff)

# to check in SQL (from Ned)
# (select common_name, count(*) otolith_count
#   from racebase.specimen a, racebase.species b
#   where region = 'GOA' and cruise = 202101
#   and specimen_sample_type = 1
#   and a.species_code = b.species_code
#   group by common_name
#   order by otolith_count)

# Sp richness by subregion and family ------------------------------------------
subregion_fam_div <- appB |>
  group_by(regulatory_area_name, family) |>
  tally(name = "nsp") |>
  pivot_wider(names_from = regulatory_area_name, values_from = nsp) |>
  dplyr::rename(Family = family) |>
  ungroup() |>
  mutate_at(2:6, ~ replace_na(., 0)) |>
  relocate(any_of(c("Family", district_order)))


# "Table 2": Mean CPUE 20 most abundant groundfish spps ------------------------
# Can convert this into a function later
biomass_gp <- read.csv("data/local_gap_products/biomass.csv")

area_gp <- read.csv("data/local_gap_products/area.csv")

if(SRVY == "GOA" & maxyr >= 2025){
  area_gp_reg_area <- area_gp |>
    dplyr::filter(AREA_TYPE %in% c("NMFS STATISTICAL AREA", "REGION") &
                    DESIGN_YEAR == 2025)
      #DESIGN_YEAR == ifelse(maxyr >= 2025, 2025, 1984)) #DESIGN_YEAR == 2025
  area_gp_reg_area$AREA_NAME[which(area_gp_reg_area$AREA_NAME=="Western Regulatory Area")] = "Shumagin"
} else {
  area_gp_reg_area <- area_gp |>
    dplyr::filter(AREA_TYPE %in% c("INPFC", "REGION") &
      DESIGN_YEAR == ifelse(SRVY=="GOA", 1984, 1980)) # get proper design year
}

topn <- 20

# biomass_subarea is created in prep_data and includes complexes
if (!exists("biomass_subarea")) {
  biomass_subarea <- read.csv(file = paste0(dir_out_srvy_yr, "tables/biomass_subarea_all.csv"))
}

# Make table of top CPUE
top_CPUE <- biomass_subarea |>
  dplyr::filter(YEAR == maxyr & grepl(pattern = "[0-9]", x = SPECIES_CODE)) |>
  dplyr::filter(SPECIES_CODE < 40001) |> # take out inverts
  dplyr::right_join(area_gp_reg_area, by = c("SURVEY_DEFINITION_ID", "AREA_ID")) |>
  dplyr::select(AREA_NAME, N_HAUL, SPECIES_CODE, CPUE_KGKM2_MEAN) |>
  #dplyr::rename("NMFS_STATISTICAL_AREA" = AREA_NAME) |> # will change if needed
  dplyr::mutate(wgted_mean_cpue_kgha = CPUE_KGKM2_MEAN / 100) |> # convert to hectares
  #group_by(NMFS_STATISTICAL_AREA) |>
  dplyr::group_by(AREA_NAME) |>
  dplyr::slice_max(n = topn, order_by = wgted_mean_cpue_kgha, with_ties = FALSE) |>
  left_join(species_names, by = c("SPECIES_CODE" = "species_code")) |>
  dplyr::rename(
    "species_code" = SPECIES_CODE
  ) |>
  dplyr::ungroup()

top_CPUE <- top_CPUE |>
  arrange(factor(AREA_NAME, levels = c(district_order, "All")))
  #arrange(factor(NMFS_STATISTICAL_AREA, levels = c(district_order, "All")))


write.csv(
  x = top_CPUE, file = paste0(dir_out_tables, "top_CPUE", "_", maxyr, ".csv"),
  row.names = FALSE
)


# Stations allocated, attempted, succeeded --------------------------------

allocated_sampled <- make_allocated_sampled(haul_maxyr = haul_maxyr, 
                                            all_allocation = all_allocation,
                                            maxyr = maxyr, 
                                            district_order = district_order,
                                            area_lookup_table = region_lu)


# Statement about assigned sampling densities - numeric vector of two
depthrange_hisamplingdensity <- allocated_sampled |>
  dplyr::filter(`Depth range (m)` != "All depths" & `Management area` == "All areas") |>
  slice_max(n = 2, order_by = `Stations per 1,000 km^2`) |>
  dplyr::select(`Depth range (m)`) |>
  unlist()

stationdensity_hisamplingdensity <- allocated_sampled |>
  filter(`Depth range (m)` != "All depths" & `Management area` == "All areas") |>
  slice_max(n = 2, order_by = `Stations per 1,000 km^2`) |>
  dplyr::select(`Stations per 1,000 km^2`) |>
  unlist()

surveywide_samplingdensity <- allocated_sampled |>
  filter(`Depth range (m)` == "All depths" & `Management area` == "All areas") |>
  dplyr::select(`Stations per 1,000 km^2`) |>
  as.numeric() |>
  round(digits = 5)

list_samplingdensities <- list(
  depthrange_hisamplingdensity = depthrange_hisamplingdensity,
  stationdensity_hisamplingdensity = stationdensity_hisamplingdensity,
  surveywide_samplingdensity = surveywide_samplingdensity
)

save(list_samplingdensities, file = paste0(dir_out_tables, "list_samplingdensities.rdata"))

# load(paste0(dir_out_tables, "list_samplingdensities.rdata"))
# 
# depthrange_hisamplingdensity <- list_samplingdensities$depthrange_hisamplingdensity
# stationdensity_hisamplingdensity <- list_samplingdensities$stationdensity_hisamplingdensity
# surveywide_samplingdensity <- list_samplingdensities$surveywide_samplingdensity

# "Table 3" -----------------------------------------------------------
# biomass_gp already loaded above
# area_gp loaded above
table3s_list <- list()

for (i in 1:nrow(report_species)) {
  # if single species, use biomass from GAP_PRODUCTS, if complex use biomass from gapindex.
  if (grepl(pattern = "[0-9]", x = report_species$species_code[i])) {
    bt <- biomass_gp
  } else {
    bt <- biomass_subarea
  }

  tab3 <- make_tab3(
    species_code = report_species$species_code[i],
    year = maxyr,
    biomass_tbl = bt,
    area_tbl = area_gp
  )

  # remove avg individual weight if this is a complex
  if (grepl(pattern = "[A-Z]", x = report_species$species_code[i])) {
    tab3 <- tab3 |>
      dplyr::select(-`Average weight (kg)`)
  }

  write.csv(x = tab3, file = paste0(
    dir_out_srvy_yr, "tables/tab3_",
    report_species$species_code[i], "_", maxyr, ".csv"
  ))
  table3s_list[[i]] <- tab3
}

names(table3s_list) <- report_species$species_code

print("Done creating Table 3s")

# "Table 4"  --------------------------------------------------------------

table4s_list <- list()

for (i in 1:nrow(report_species)) {
  # if single species, use biomass from GAP_PRODUCTS, if complex use biomass from gapindex.
  if (grepl(pattern = "[0-9]", x = report_species$species_code[i])) {
    bt <- biomass_gp
  } else {
    bt <- biomass_stratum_complexes |>
      dplyr::rename("AREA_ID" = "STRATUM")
  }

  tab4 <- make_tab4(
    species_code = report_species$species_code[i],
    year = maxyr,
    biomass_tbl = bt,
    area_tbl = area_gp
  )

  write.csv(x = tab4, file = paste0(
    dir_out_srvy_yr, "tables/tab4_",
    report_species$species_code[i], "_", maxyr, ".csv"
  ))
  table4s_list[[i]] <- tab4
}

names(table4s_list) <- report_species$species_code

print("Done creating Table 4s")


# Comparison of biomass between maxyr and compareyr -----------------------
compare_tab <- biomass_total |>
  dplyr::filter(YEAR %in% c(maxyr, compareyr) &
    SPECIES_CODE %in% report_species$species_code) |>
  dplyr::select(YEAR, SPECIES_CODE, BIOMASS_MT) |>
  dplyr::arrange(YEAR) |>
  tidyr::pivot_wider(
    names_from = YEAR,
    values_from = BIOMASS_MT,
    names_prefix = "yr_"
  ) |>
  as.data.frame()

compare_tab$percent_change <- round((compare_tab[, 3] - compare_tab[, 2]) / compare_tab[, 2] * 100, digits = 1)
names(compare_tab)

compare_tab2 <- compare_tab |>
  left_join(report_species, by = c("SPECIES_CODE" = "species_code")) |>
  # arrange(-SPECIES_CODE) |>
  dplyr::select(-(presentation:reportorder))

write.csv(
  x = compare_tab2,
  file = paste0(dir_out_tables, maxyr, "_comparison_w_previous_survey.csv")
)


# Assemble and save tables -----------------------------------------------------

list_tables <- list()

list_tables[["allocated_sampled"]] <- allocated_sampled # Stations allocated and successfully sampled
list_tables[["length-sample-sizes"]] <- targetn # Target length size for species/species groups
list_tables[["top_CPUE"]] <- top_CPUE # Top 20 species by CPUE in all the INPFC regions
list_tables[["otos_target_sampled"]] <- otos_target_sampled # Otolith targets and whether they were met
# list_tables[["depthrange_hisamplingdensity"]] <- depthrange_hisamplingdensity
# list_tables[["stationdensity_hisamplingdensity"]] <- stationdensity_hisamplingdensity
# list_tables[["surveywide_samplingdensity"]] <- stationdensity_hisamplingdensity


save(list_tables,
  file = paste0(dir_out_tables, "report_tables.rdata")
)

save(list_samplingdensities,
  file = paste0(dir_out_tables, "list_samplingdensities.rdata")
)

save(table3s_list,
  file = paste0(dir_out_tables, "table3s_list.rdata")
)

save(table4s_list,
  file = paste0(dir_out_tables, "table4s_list.rdata")
)
