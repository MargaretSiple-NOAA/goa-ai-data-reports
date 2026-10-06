# RACEBASE tables ----------------------------------------------------
# get catch and taxonomy info
catch <- read_csv("data/local_racebase/catch.csv") |>
  mutate(YEAR = stringr::str_extract(CRUISE, "^\\d{4}"))

taxonomy <- read_csv("data/local_racebase/species_classification.csv")

species_codes <- read_csv("data/local_race_data/race_species_codes.csv") |>
  dplyr::select("SPECIES_CODE", "SPECIES_NAME", "COMMON_NAME")

haul <- read_csv("data/local_racebase/haul.csv")


# Alternative to above chunk: Zack SQL query ------------------------------
channel <- gapindex::get_connected(db = "AFSC")
appendix_b_tbl <- 
  gapindex::sql_query(channel = channel,
                      query = paste0("
/* 
   Appendix B: list of species observed in each INPFC area.
   Source replacement for RACEBASE.CATCH table in script 05_download_data_from_oracle.R.
*/
select 
    area.area_name,
    tax.family_taxon,
    tax.common_name,
    tax.species_name,
    tax.species_code
from 
    racebase.catch catch_ 
join /* attach taxonomic info */
    gap_products.taxonomic_classification tax on tax.species_code = catch_.species_code
join /* attach abundance_haul info */
    gap_products.haul haul on haul.hauljoin = catch_.hauljoin
join /* attach year and survey_definition_id */
    gap_products.cruise cruise on cruise.cruisejoin = haul.cruisejoin
join /* attach design_year info */
    gap_products.survey_design design on design.year = cruise.year and design.survey_definition_id = cruise.survey_definition_id
join /* attach area_id of the INPFC or NMFS area that the strata belong to*/
    gap_products.stratum_groups stratum_groups on stratum_groups.stratum = haul.stratum
join /* attach area name */
    gap_products.area area on 
        area.survey_definition_id = stratum_groups.survey_definition_id 
        and area.design_year = stratum_groups.design_year
        and area.area_id = stratum_groups.area_id
where 
   /* filter records from tax that the survey uses */
    tax.survey_species = 1
   /* by INPFC areas in the AI and NMFS areas in the GOA*/
    and area.area_type = '", unname(c("AI" = "INPFC", "GOA" = "NMFS")[SRVY]), "' 
   /* filter for the current year */
    and cruise.year = ", maxyr, " 
   /* filter for the current survey region*/
    and area.survey_definition_id = ", sdi, " 
    /* filter for only species-level SPECIES_CODES, remove egg cases, larva, tubes, etc. */
    and tax.id_rank = 'species' 
    and tax.species_name not like '% egg%'
    and tax.species_name not like '%egg case%'
    and tax.species_name not like '%larva%' 
    and tax.species_name not like '%larvae%'
    and tax.species_name not like '% tubes%'
group by 
    area.area_name,
    tax.family_taxon,
    tax.common_name,
    tax.species_name,
    tax.species_code
order by 
    area.area_name, 
    tax.species_code
")
  )



# Getting species from year ----------------------------------------------------

# filtering to just species caught this survey year and getting subregion info
catch_maxyr0 <- catch |>
  filter(YEAR == maxyr & REGION == SRVY) |>
  left_join(haul, by = join_by(CRUISEJOIN, HAULJOIN, REGION, VESSEL, CRUISE, HAUL))

if (SRVY == "GOA" & design_year >= 2025) {
  catch_maxyr <- catch_maxyr0 |> # need to merge with haul df to get stratum
    left_join(stratum_lu, by = "STRATUM") |>
    dplyr::select("SPECIES_CODE", "REGULATORY_AREA_NAME", "START_LONGITUDE", "START_LATITUDE", "BOTTOM_DEPTH") |>
    unique()
} else { # for AI and old GOA surveys
  catch_maxyr <- catch_maxyr0 |> # need to merge with haul df to get stratum
    left_join(region_lu, by = "STRATUM") |> # this and the following line are different
    dplyr::select("SPECIES_CODE", "INPFC_AREA", "START_LONGITUDE", "START_LATITUDE", "BOTTOM_DEPTH") |>
    unique()
}

# non species indicator strings
rm_bits <- paste0(
  c(" egg", "egg case", "larva", "larvae", " tubes", "sp\\.$"),
  collapse = "|"
)


# limiting to only taxa identified to species level (i.e. no genus, etc. level IDs)
species_maxyr <- catch_maxyr |>
  left_join(species_codes) |>
  mutate(
    SPECIES_NAME = trimws(SPECIES_NAME),
    level = case_when(
      str_detect(SPECIES_NAME, rm_bits) | !str_detect(SPECIES_NAME, " ") | is.na(SPECIES_NAME) ~ "",
      TRUE ~ "species"
    )
  ) |>
  filter(level == "species") |>
  dplyr::select(-level)


# Finding outliers ----------------------------------------------------
# checking for species that were caught this year that are suspicious/need manual checking using DBSCAN/past confirmed records

# all catch/haul data to check against
catch_haul <- catch |>
  filter(SPECIES_CODE %in% species_maxyr$SPECIES_CODE) |>
  left_join(haul, by = c("CRUISEJOIN", "HAULJOIN")) |>
  left_join(species_codes, by = "SPECIES_CODE") |>
  # mutate(START_LONGITUDE = ifelse(START_LONGITUDE < 0,
  #                                 START_LONGITUDE, START_LONGITUDE*-1)) %>%
  dplyr::select(
    SPECIES_CODE, SPECIES_NAME, START_LONGITUDE,
    START_LATITUDE, GEAR_DEPTH, YEAR
  )


# outlier species from this year
outlier_spp <- species_maxyr |>
  dplyr::select(SPECIES_CODE) |>
  unique() |>
  mutate(outlier = purrr::map(SPECIES_CODE, ~ check_outlier(.x, maxyr, catch_haul))) |>
  unnest(cols = outlier) |>
  left_join(species_codes) |>
  dplyr::select(SPECIES_CODE, SPECIES_NAME) |>
  unique()


# # plots outliers to pdf document -- FOR SARAH
# pdf(paste0("output/outliers_", maxyr, ".pdf"))
# outlier_spp %>%
#   mutate(g = purrr::map(SPECIES_CODE, ~check_outlier(.x, maxyr, catch_haul, plot = T)))
# dev.off()


# Generate tables/stats ----------------------------------------------------

# Table for Appendix B
appB0 <- species_maxyr |>
  left_join(taxonomy, by = "SPECIES_CODE") |>
  left_join(species_codes) |>
  janitor::clean_names() |>
  mutate(major_group = case_when(
    species_code >= 10000 & species_code <= 19999 ~ "Flatfish",
    species_code >= 20000 & species_code <= 39999 ~ "Roundfish",
    species_code >= 30000 & species_code <= 36999 ~ "Rockfish",
    species_code >= 40000 & species_code <= 99990 ~ "Invertebrates",
    species_code >= 00150 & species_code <= 00799 ~ "Chondrichthyans"
  )) |>
  dplyr::mutate(tax_group = dplyr::case_when(
    species_code <= 31550 ~ "fish",
    species_code >= 40001 ~ "invert"
  )) |>
  dplyr::mutate(family_taxon = case_when(
    species_code == 44086 ~ "Primnoidae", TRUE ~ family_taxon
  ))

if (SRVY == "GOA" & design_year >= 2025) {
  appB <- appB0 |>
    dplyr::select(regulatory_area_name, species_code, species_name, common_name,
      family = family_taxon, phylum = phylum_taxon,
      major_group, tax_group
    ) |>
    distinct() |>
    arrange(regulatory_area_name, tax_group, major_group, species_name)
} else {
  appB <- appB0 |>
    dplyr::select(inpfc_area, species_code, species_name, common_name,
      family = family_taxon, phylum = phylum_taxon,
      major_group, tax_group
    ) |>
    distinct() |>
    arrange(inpfc_area, tax_group, major_group, species_name)
}


# Compare appB with appendix_b_tbl from above:
appB$species_code[which(!appB$species_code %in% appendix_b_tbl$SPECIES_CODE)]



# diversity by subregion
if(SRVY == "AI" | maxyr < 2025){
subregion_diversity <- appB |>
  group_by(inpfc_area, tax_group) |>
  tally(name = "nsp") |>
  pivot_wider(names_from = tax_group, values_from = nsp)
}else{
  subregion_diversity <- appB |>
    group_by(regulatory_area_name, tax_group) |>
    tally(name = "nsp") |>
    pivot_wider(names_from = tax_group, values_from = nsp)
}

# standardize colnames to work for both areas, all years 
colnames(subregion_diversity)[which(colnames(subregion_diversity)=='inpfc_area')] <- 'regulatory_area_name'

head(subregion_diversity)


# statement for text
if (SRVY == "GOA" & design_year >= 2025) {
  total_diversity <- appB |>
    dplyr::select(-regulatory_area_name) |>
    unique()
} else {
  total_diversity <- appB |>
    dplyr::select(-inpfc_area) |>
    unique()
}

n_fish <- total_diversity |> filter(tax_group == "fish")
n_fam <- length(unique(n_fish$family))
n_inverts <- total_diversity |> filter(tax_group == "invert")
n_phyla <- length(unique(n_inverts$phylum))

tax_summary_sentence <- paste(
  "Total catches across the survey area included",
  nrow(n_fish), "fish species from", n_fam, "families, and",
  nrow(n_inverts), "invertebrate species or taxa from", n_phyla, "phyla"
)

cat(tax_summary_sentence)
