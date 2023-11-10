# Housekeeping -----------------------------------------------------------------

# Keep chapter content in a proper order
cnt_chapt <- "000"
# Automatically name objects with consecutive numbers
cnt_figures <- 0 #  e.g., Figure 1
cnt_tables <- 0 # e.g., Table 1
cnt_equations <- 0 # e.g., Equation 1
# Save object content
list_equations <- list()
list_tables <- list()
list_figures <- list()

# Conversions --------------------------------------------
find_units <- function(unit = "", unt = "", dat, divby = NULL) {
  # x <- ifelse(unit == "", "s", paste0(" ", unit))
  x <- unit # ifelse(unit != "", paste0(" ", unit), unit)
  x_ <- ifelse(unt == "", "", unt)

  # find appropriate units

  if (is.null(divby)) {
    min_val <- min(dat, na.rm = TRUE)
    min_val1 <- xunits(min_val, words = TRUE)
  } else {
    min_val <- divby
    min_val1 <- xunits(divby, words = TRUE)
  }


  if (min_val < 1e3) {
    divby <- 1
    unit_word <- ifelse(unit == "", "", paste0(" (", x, ")"))
    unit_wrd <- paste0("", x_)
  } else if (min_val < 1e6) {
    divby <- 1e3
    unit_word <- paste0(
      " (thousand",
      ifelse(unit == "", "s", paste0(" ", unit)),
      ")"
    )
    unit_wrd <- paste0("K", x_)
  } else if (grepl(pattern = "million", x = min_val1)) {
    divby <- 1e6
    unit_word <- paste0(
      " (million",
      ifelse(unit == "", "s", paste0(" ", unit)),
      ")"
    )
    unit_wrd <- paste0("M", x_)
  } else if (grepl(pattern = "billion", x = min_val1)) {
    divby <- 1e9
    unit_word <- paste0(
      " (billion",
      ifelse(unit == "", "s", paste0(" ", unit)),
      ")"
    )
    unit_wrd <- paste0("B", x_)
  } else if (grepl(pattern = "trillion", x = min_val1)) {
    divby <- 1e12
    unit_word <- paste0(
      " (trillion",
      ifelse(unit == "", "s", paste0(" ", unit)),
      ")"
    )
    unit_wrd <- paste0("T", x_)
  }


  return(list(
    "divby" = divby,
    "unit_word" = unit_word,
    "unit_wrd" = unit_wrd
  ))
}

biomass_round <- function(x){round(x,digits = 0)}

# https://github.com/geanders/weathermetrics/blob/master/R/temperature_conversions.R
c2f <- function(T.celsius, round = 2) {
  T.fahrenheit <- (9 / 5) * T.celsius + 32
  T.fahrenheit <- round(T.fahrenheit, digits = round)
  return(T.fahrenheit)
}

# Conversion factor thingies
divnmi2forkm2 <- 1 / 3.429904
divkm2fornmi2 <- 3.429904
divkm2forha <- 100
divmforft <- 0.3048
divftform <- 3.28084


# Text manipulation -------------------------------------------------------
#' Title
#'
#' @param x text string from soliciting the areas with the top CPUE or biomass or whatever
#'
#' @return a string with both of those areas
#' @export
#'
#' @examples
fix_co_greatest <- function(text_string){
  if(startsWith(x = text_string, "c(")){
    text_string2 <- sub("c(","",text_string,fixed = TRUE)
    text_string3 <- gsub("[^-,/a-zA-Z0-9[:space:]]+", "", text_string2, perl = TRUE)
    newtext <- gsub("[,]", ", ", text_string3, perl = TRUE)
  }else{
    newtext <- text_string
  }
  return(newtext)
}

#' Convert character version of a large number into a numeric one
#'
#' @param x vector of character strings like you would find in a standard GOA / AI data table. Usually contains stuff like "---" or "< something".
#'
#' @return a numeric version of the number, or NA if x was a series of dashes or something else.
#' @export
#'
#' @examples
chr_to_num <- function(x){
  y=x
  y[which(x=="---" | x == "<1")] = NA
  y = as.numeric(gsub(",", "", y))
  
  return(y)
}
  

# Species -----------------------------------------------

#' #' Make a list of the top 20 species by CPUE
#' #'
#' #' @param YEAR numeric - year for which you want the top spps
#' #' @param SRVY character - "GOA" or "AI"
#' #' @param cpue_raw dataframe or tibble containing raw CPUE data. Can be from FOSS or GOA/AI schemae. Columnscurrently include...
#' # c("survey", "year", "catchjoin", "hauljoin", "vessel", "cruise",
#' #   "haul", "stratum", "distance_fished", "net_width", "species_code",
#' #   "weight", "number_fish", "effort", "cpue_kgkm2", "numcpue", "species_name",
#' #   "common_name", "cruisejoin", "region", "haul_type", "performance",
#' #   "duration", "net_measured", "net_height", "start_latitude", "end_latitude",
#' #   "start_longitude", "end_longitude", "stationid", "gear_depth",
#' #   "bottom_depth", "bottom_type", "surface_temperature", "gear_temperature",
#' #   "wire_length", "gear", "accessories", "subsample", "abundance_haul",
#' #   "auditjoin", "start_time")
#' #' @param topn how many of the top species do you want? For the report it's the top 20; can be adjusted as needed
#' #' @return a tibble with CPUE and some other stuff by species, for the 20 most abundant spps in the survey.
#' #' @export
#' #'
#' #' @examples
#' make_top_cpue <- function(YEAR, SRVY, cpue_raw, topn = 20) { # Gives top 20 spps for each region
#'   
#'   cpue_districts <- cpue_raw %>%
#'     filter(year == YEAR & survey == SRVY & abundance_haul == "Y") %>% #
#'     dplyr::mutate(taxon = dplyr::case_when(
#'       species_code <= 31550 ~ "fish",
#'       species_code >= 40001 ~ "invert"
#'     )) %>%
#'     dplyr::mutate(common_name = case_when(
#'       species_code >= 30050 & species_code <= 30052 ~ "Rougheye / blackspotted rockfish complex",
#'       species_code == 405 ~ "Bathyraja sp.",
#'       TRUE ~ common_name
#'     )) %>%
#'     # Old skate check
#'     # dplyr::filter(species_code >=400 & species_code<=495) %>%
#'     left_join(region_lu, by = c("stratum" = "STRATUM")) %>%
#'     dplyr::group_by(
#'       survey, year, stratum, species_code,
#'       species_name, common_name, taxon,
#'       SURVEY, INPFC_AREA, `Depth range`,
#'       REGULATORY_AREA_NAME, AREA
#'     ) %>% # AREA is the area in km^2 of the stratum
#'     dplyr::summarize(
#'       stratum_cpue_kgkm2 = mean(cpue_kgkm2),
#'       stratum_cpue_kgkm2_var = var(cpue_kgkm2)
#'     ) %>%
#'     # mean cpue by stratum (not yet weighted)
#'     ungroup() %>%
#'     dplyr::left_join(INPFC_areas) %>%
#'     mutate(weight_for_mean = AREA / INPFC_AREA_AREA_km2)
#' 
#'   # what we want: a table with CPUE calculated for each region, based on the area-based weightings in the INPFC_areas table.
#'   districts <- cpue_districts %>%
#'     dplyr::group_by(INPFC_AREA, species_code) %>%
#'     dplyr::summarize(
#'       wgted_mean_cpue_kgkm2 = sum(stratum_cpue_kgkm2 * weight_for_mean)
#'     ) %>%
#'     ungroup() %>%
#'     mutate(wgted_mean_cpue_kgha = wgted_mean_cpue_kgkm2 / 100) %>%
#'     group_by(INPFC_AREA) %>%
#'     dplyr::slice_max(n = topn, order_by = wgted_mean_cpue_kgha, with_ties = FALSE) %>%
#'     dplyr::ungroup() %>%
#'     dplyr::left_join(species_names)
#' 
#'   if(SRVY=="AI"){
#'   total_aleutians_area_km2 <- INPFC_areas[which(INPFC_areas$INPFC_AREA == "All Aleutian Districts"), "INPFC_AREA_AREA_km2"] %>% as.numeric()
#' 
#'   aleutian_areas <- cpue_districts %>%
#'     mutate(survey_weight = AREA / total_aleutians_area_km2) %>%
#'     dplyr::group_by(species_code) %>%
#'     dplyr::summarize(wgted_mean_cpue_kgkm2 = sum(stratum_cpue_kgkm2 * survey_weight)) %>%
#'     ungroup() %>%
#'     mutate(wgted_mean_cpue_kgha = wgted_mean_cpue_kgkm2 / 100) %>%
#'     dplyr::slice_max(n = topn, order_by = wgted_mean_cpue_kgha, with_ties = FALSE) %>%
#'     dplyr::left_join(species_names) %>%
#'     mutate(INPFC_AREA = "All Aleutian Districts") 
#'   }  else{
#'     aleutian_areas <- data.frame()
#'   }
#'   
#'   total_survey_area_km2 <- INPFC_areas[which(INPFC_areas$INPFC_AREA == "All Districts"), "INPFC_AREA_AREA_km2"] %>% as.numeric()
#'   
#'  all_areas <- cpue_districts %>%
#'     mutate(survey_weight = AREA / total_survey_area_km2) %>%
#'     dplyr::group_by(species_code) %>%
#'     dplyr::summarize(wgted_mean_cpue_kgkm2 = sum(stratum_cpue_kgkm2 * survey_weight)) %>%
#'     ungroup() %>%
#'     mutate(wgted_mean_cpue_kgha = wgted_mean_cpue_kgkm2 / 100) %>%
#'     dplyr::slice_max(n = topn, order_by = wgted_mean_cpue_kgha, with_ties = FALSE) %>%
#'     dplyr::left_join(species_names) %>%
#'     mutate(INPFC_AREA = "All Areas Combined")
#' 
#'   
#'   bigtable <- bind_rows(districts, aleutian_areas, all_areas) %>%
#'     dplyr::mutate(scientific_name = case_when(
#'       common_name == "Rougheye / blackspotted rockfish complex" ~ "Sebastes aleutianus / Sebastes melanostictus",
#'       TRUE ~ scientific_name
#'     )) %>%
#'     dplyr::mutate(major_group = case_when(
#'       common_name == "Rougheye / blackspotted rockfish complex" ~ "Rockfishes",
#'       TRUE ~ major_group
#'     ))
#'   
#'   # bigtable
#'   
#'   return(bigtable)
#' }
#' 


#' Make summary table of biomass by depth range and mgmt area (assessment request)
#'
#' @param species_code five-digit species code (numeric)
#'
#' @return a table with total biomass in Aleutian and non-Aleutian areas by year for a given species.
#' @export
#'
#' @examples
#' make_depth_mgmt_area_summary(species_code = 10130)
make_depth_mgmt_area_summary <- function(species_code) {
  x <- biomass_stratum %>%
    filter(SPECIES_CODE == species_code) %>%
    left_join(region_lu, by = c("SURVEY", "STRATUM")) %>%
    dplyr::select(YEAR, REGULATORY_AREA_NAME, `Depth range`, STRATUM_BIOMASS) %>%
    dplyr::group_by(YEAR, REGULATORY_AREA_NAME, `Depth range`) %>% #
    dplyr::summarize(total_biomass = sum(STRATUM_BIOMASS, na.rm = TRUE)) %>%
    dplyr::ungroup()

  return(x)
}


#' Using a spreadsheet fromn the old methodology, make a list of the top 20 species by CPUE.
#'
#' @details Currently this is our chosen method becaus eit eliminates the weird tiny discrepancies that we get when calculating CPUEs by hand vs. using summary tables from the AI schema.
#' @param filepath The path of the file containing the CPUE table. Currently, Paul produces this using SQL plus some excel wizardry.
#'
#' @return a formatted table of top 20 CPUEs by area, analogous to the one produced by make_top_cpue()
#' @export
#'
#' @examples
prep_tab2 <- function(filepath = paste0(dir_in_premadetabs,"Table 2/","Table 2_AI2022_makeitlooklikethisplease.xlsx")){
  if (!file.exists(filepath)) {
    stop("Species Table 2 file missing from the folder. Check directory and make sure you're on the VPN and the filename you've specified is correct, including the year, region, and folder.")
  }
  raw <- readxl::read_excel(filepath)
  # not_all_na <- function(x) any(!is.na(x))
  # raw2 <- raw %>% 
  #   dplyr::select(where(not_all_na)) 
  
  haulcounts <- raw %>%
    dplyr::filter(species=="Number of hauls") %>%
    dplyr::rename("nhauls" = CPUE) %>%
    dplyr::select(-species)
  
  raw2 <- raw %>%
    dplyr::filter(species!="Number of hauls") %>%
    fuzzyjoin::regex_left_join(species_codes,by = c("species" = "COMMON_NAME")) %>%
    dplyr::select(-species) %>%
    dplyr::rename("wgted_mean_cpue_kgha"="CPUE",
                  "INPFC_AREA"="INPFC area",
                  "species_code" ="SPECIES_CODE",
                  "common_name"="COMMON_NAME",
                  "scientific_name"="SPECIES_NAME") %>%
    dplyr::arrange(factor(INPFC_AREA,levels=district_order))
  
  return(raw2)
}


#NOTE: THIS STILL DOESN"T WORK
# make_tab3 <- function(species_code = NULL, survey = NULL, year = NULL){
#   if(survey=="AI"){
#     # source: G:\ALEUTIAN\datareport\ai_cpuetable.sql] and one for GOA [G:\GOA\datareport\goa_cpuetable.sql]
#     sql_script <- readLines("sql/ai_cpuetable.sql")
#     goa_query <-  paste(sql_script, collapse = " ")
#     goa_query <- gsub("\t", " ", goa_query)
#     a <- RODBC::sqlQuery(channel, paste0("define xyear = '", as.character(year),"' ",goa_query))
#   }
#   
# if(survey=="GOA"){
#   # Read the SQL script from a file
#   sql_script <- readLines("sql/goa_cpuetable.sql")
#   goa_query <-  paste(sql_script, collapse = " ")
#   
#   # Combine the lines into a single string, removing carriage returns
#   sql_script <- paste(sql_script, collapse = " ")
#   a <- RODBC::sqlQuery(channel, paste0(
#     "define xsurvey_area = 'GOA' define b = 'goa.biomass_' define sz = 'goa.sizecomp_' define in = 'inpfc' define in2 = 'inpfc_' define a = 'area' define a2 = 'area_' define d = 'depth' define t = 'total' define cpue = 'goa.cpue' define bindy = &b&in2&d define bdy = &b&d define biny = &b&in define bay = &b&a define bady = &b&a2&d define szindy = &sz&in2&d define szdy = &sz&d define sziny= &sz&in define szady = &sz&a2&d define szty = &sz&t define cpuey = &cpue drop table summary_areas1; create table summary_areas1 as select distinct summary_area_depth summary_area from goa.goa_strata where survey = upper('&xsurvey_area') and stratum in (select distinct stratum from &cpuey); insert into summary_areas1 select distinct summary_area from goa.goa_strata where survey = upper('&xsurvey_area') and stratum in (select distinct stratum from &cpuey); insert into summary_areas1 select distinct summary_depth from goa.goa_strata where survey = upper('&xsurvey_area') and stratum in (select distinct stratum from &cpuey); insert into summary_areas1 values (999); drop table summary_areas; create table summary_areas as select summary_area, species_code from summary_areas1 s, goa.analysis_species g where biomass_flag = upper('&xsurvey_area') or biomass_flag = 'BOTH'; drop table biomass_tab; create table biomass_tab as select b.species_code, b.summary_area_depth summary_area, haul_count, catch_count, round(b.mean_wgt_cpue, 0) mean_wgt_cpue, area_biomass, min_biomass, max_biomass, round((area_biomass*1000)/area_pop,3) weight from &bindy b, &szindy s where b.year = &", year, " and s.year = &", year, "and b.summary_area_depth = s.summary_area_depth(+) and b.species_code = s.species_code(+) and area_pop > 0 group by b.species_code, b.summary_area_depth, haul_count, catch_count, mean_wgt_cpue, area_biomass, min_biomass, max_biomass, area_pop; insert into biomass_tab select b.species_code, b.summary_area, haul_count, catch_count, round(b.mean_wgt_cpue, 0) mean_wgt_cpue, area_biomass, min_biomass, max_biomass, round((area_biomass*1000)/area_pop,3) weight from &biny b, &sziny s 	where b.year = &",year," and s.year = &",year, "and b.summary_area = s.summary_area(+) and b.species_code = s.species_code(+) and area_pop > 0	group by b.species_code, b.summary_area, haul_count, catch_count, mean_wgt_cpue, area_biomass, min_biomass, max_biomass, area_pop; drop table bay; create table bay as select * from &bay; update bay set area_pop = 1 where area_pop = 0; insert into biomass_tab select b.species_code, 999 total_area, haul_count, catch_count, round(b.mean_wgt_cpue, 0) mean_wgt_cpue, area_biomass, min_biomass, max_biomass, round((area_biomass*1000)/area_pop,3) weight from bay b, &szty s where b.year = &",year, "and s.year = &", year,"and b.species_code = s.species_code(+) and regulatory_area_name = 'ALEUTIANS' group by b.species_code, haul_count, catch_count, mean_wgt_cpue, area_biomass, min_biomass, max_biomass, area_pop; drop table bady; create table bady as select * from &bady; update bady set area_pop = 1 where area_pop = 0; insert into biomass_tab select b.species_code, b.summary_depth, haul_count, catch_count,  round(b.mean_wgt_cpue, 0) mean_wgt_cpue, area_biomass, min_biomass, max_biomass, round((area_biomass*1000)/area_pop,3) weight	from bady b, &szady s where b.year = &", year, "and s.year= &", year,	"and b.species_code = s.species_code(+) and b.regulatory_area_name = 'GULF OF ALASKA' and b.regulatory_area_name = s.regulatory_area_name and b.summary_depth = s.summary_depth group by b.species_code, b.summary_depth, haul_count, catch_count, mean_wgt_cpue, area_biomass, min_biomass, max_biomass, area_pop; drop table haul_count; create table haul_count as select distinct summary_area, haul_count from biomass_tab; drop table table_biomass; create table table_biomass as select s.summary_area, o.sort_order, s.species_code, ltrim(to_char(h.haul_count,'990')) haul_count, ltrim(to_char(catch_count,'990')) catch_count, ltrim(to_char(mean_wgt_cpue, '999,999,990'), ' ') mean_wgt_cpue, ltrim(to_char(area_biomass, '999,999,990'), ' ') area_biomass, ltrim(to_char(min_biomass, '999,999,990'), ' ') min_biomass, ltrim(to_char(max_biomass, '999,999,990'), ' ') max_biomass, ltrim(to_char(weight, '990.000'), ' ') weight	from biomass_tab b, summary_areas s, haul_count h, goa.data_report_sort_order o	where s.summary_area = b.summary_area(+) and s.species_code = b.species_code(+) and h.summary_area(+) = s.summary_area and h.summary_area = o.summary_area; update table_biomass set haul_count = '0' where haul_count is null; update table_biomass set catch_count = '0' where catch_count is null; update table_biomass set mean_wgt_cpue = '---' where mean_wgt_cpue is null; update table_biomass set area_biomass = '---' where area_biomass is null; update table_biomass set min_biomass = '---' where min_biomass is null; update table_biomass set max_biomass = '---' where max_biomass is null; update table_biomass set weight = '---' where weight is null; update table_biomass set mean_wgt_cpue = '<1' where mean_wgt_cpue = '0'; update table_biomass set area_biomass = '<1' where area_biomass = '0'; update table_biomass set weight = '<0.001' where weight = '0.000'; drop table biomass_tab; drop table bay; drop table bady; drop table summary_areas1; drop table summary_areas"))
# }
#   
#   dir_out <- paste0("data/local_", tolower(survey), "_processed/table3_", species_code, "_", survey, "_", year, ".csv")
#   
#   write.csv(x = a, file = dir_out, row.names = FALSE)
# }

#' Make a rough draft of Table 4
#'
#' @param species_code species code (numeric)
#' @param survey survey code, "AI" or "GOA" (character)
#' @param year survey year (numeric)
#'
#' @return writes a csv file for each species for table 4. 
#' @export
#'
#' @examples
#' make_tab4(species_code = 30060, survey = "GOA", year = 2023)
#'
make_tab4 <- function(species_code = NULL, survey = NULL, year = NULL) {
  a <- RODBC::sqlQuery(channel, paste0(
    "SELECT DISTINCT INPFC_AREA SURVEY_DISTRICT, MIN_DEPTH||'-'||MAX_DEPTH DEPTH_M, DESCRIPTION SUBDISTRICT_NAME, HAUL_COUNT NUMBER_OF_HAULS, CATCH_COUNT HAULS_W_CATCH, MEAN_WGT_CPUE/100 CPUE_KG_HA, STRATUM_BIOMASS BIOMASS_T, MIN_BIOMASS LCL_T, MAX_BIOMASS UCL_T FROM GOA.GOA_STRATA a, ", survey, ".BIOMASS_STRATUM b WHERE a.SURVEY = \'", survey, "\' and b.YEAR = ", year,
    " and b.SPECIES_CODE = ", species_code,
    " and a.STRATUM = b.STRATUM order by -CPUE_KG_HA"
  ))
  
  dir_out <- paste0("data/local_", tolower(survey), "_processed/table4_", species_code, "_", survey, "_", year, ".csv")
  
  write.csv(x = a, file = dir_out, row.names = FALSE)
}

#' Create CPUE table formatted like the one in the AI 2018 report
#'
#' @param top_CPUE A dataframe created by either prep_tab2() or make_top_cpue() (still valid but minorly different from the historical cpue tables to we revert to the former in order to be consistent)
#'
#' @return a list of dataframes, each of which should be formatted as a flextable.
#' @export
#'
#' @examples
top_CPUE_formatted <- function(top_CPUE) {
  x <- top_CPUE %>%
    # existing changes in markdown file:
    dplyr::select(INPFC_AREA, common_name, wgted_mean_cpue_kgha) %>%
    dplyr::mutate(wgted_mean_cpue_kgha = round(wgted_mean_cpue_kgha, digits = 1)) %>%
    dplyr::rename(
      `INPFC area` = INPFC_AREA,
      Species = common_name,
      `CPUE (kg/ha)` = wgted_mean_cpue_kgha
    ) #%>%
    #group_split(`INPFC area`)
  
  # y <- lapply(x, pivot_wider, names_from = `INPFC area`, values_from = `CPUE (kg/ha)`)
  # for (i in 1:length(y)) {
  #   names(y)[i] <- names(y[[i]][2])
  #   colnames(y[[i]])[2] <- "CPUE (kg/ha)"
  # }
  # tophalf <- cbind(y$`Southern Bering Sea`, y$`Eastern Aleutians`, y$`Central Aleutians`)
  # colnames(tophalf) <- c("Species", "CPUE (kg/ha)","Species ", "CPUE (kg/ha) ","Species  ", "CPUE (kg/ha)  ")
  # bottomhalf <- cbind(y$`Western Aleutians`, y$`Combined Aleutian Districts`, y$`All Districts Combined`)
  # colnames(bottomhalf) <- colnames(tophalf)
  # uglydf <- rbind(tophalf,bottomhalf)
  return(x)
}

#' Retrieve Table 3's (biomass per area and depth) for a species
#'
#' @param species_code (numeric) 5-digit species code
#'
#' @return a nice clean dataframe ready for formatting for the data report
#' @export
#' @details  This will work with a list of tables that is already subsetted to year and region! This was built to incorporate tables built by Paul or Nate.
#'
#' @examples
#' prep_tab3(30060)
prep_tab3 <- function(speciescode) {
  filepath <- paste0(dir_in_premadetabs, "Table 3/", speciescode, paste0("_",maxyr,".csv"))
  if (!file.exists(filepath)) {
    stop("Species Table 3 file missing from the folder. Check directory and make sure you're on the VPN.")
  }
  x <- read.csv(file = filepath)
  cleaned_tab <- x %>%
    # want to eventually fix these so they are the right number of digits but... not urgent for now.
    # dplyr::mutate(CPUE..kg.ha. = case_when(CPUE..kg.ha. != "---" &
    #                                          CPUE..kg.ha. != "< 0.01" ~ round(as.numeric(CPUE..kg.ha.),digits = 1),
    #                                        TRUE ~ as.character(CPUE..kg.ha.))
    #                           ),
    #      dplyr::mutate(Weight...kg. = round(Weight...kg.,digits = 2))
    dplyr::rename(
      `Survey district` = Survey.District,
      `Depth (m)` = Depth..m.,
      `Haul count` = Haul.Count,
      `Hauls with catch` = Hauls.w.Catch,
      `CPUE (kg/ha)` = CPUE..kg.ha.,
      `Biomass (t)` = Biomass...t.,
      `Lower 95% CI` = X95..LCL..t.,
      `Upper 95% CI` = X95..UCL..t.,
      `Mean weight (kg)` = Weight...kg.
    ) 
    
 if(SRVY=="AI"){x <- x %>%
    dplyr::slice(21:25, 1:20) #sloppy way to slice off the SBS and move it to the top
 }
  

  return(cleaned_tab)
}

# NOTE: If this breaks in the future, it may be because this table contains character values.
prep_tab4 <- function(speciescode) {
  filepath <- paste0(dir_in_premadetabs, "Table 4/csv files/", speciescode, "_", maxyr, "_t4.csv")
  if (!file.exists(filepath)) {
    print("Check species", speciescode)
    stop("Species Table 4 file missing from the folder. Check directory and make sure you're on the VPN.")
  }

  x <- read.csv(file = filepath)
  cleaned_tab <- x %>%
    mutate(
      CPUE..kg.ha. = round(CPUE..kg.ha., digits = 1),
      Biomass..t. = round(Biomass..t., digits = 0),
      Lower.CI.Biomass = round(Lower.CI.Biomass, digits = 0),
      Upper.CI.Biomass = round(Upper.CI.Biomass, digits = 0)
    ) %>%
    dplyr::rename(
      `Survey district` = INPFC_AREA,
      `Depth range (m)` = DEPTH_RANGE,
      `Subdistrict` = Stratum.Name,
      `Number of hauls` = Number.of.Hauls,
      `Hauls with catch` = Hauls.with.Catch,
      `CPUE (kg/ha)` = CPUE..kg.ha.,
      `Biomass (t)` = Biomass..t.,
      `Lower 95% CI` = Lower.CI.Biomass,
      `Upper 95% CI` = Upper.CI.Biomass
    ) %>%
    arrange(factor(`Survey district`,levels = district_order),`Depth range (m)`)
  
  return(cleaned_tab)
}

# format appendix b contents so they will fit easily in a flextable
prep_appendix_b <- function(df) {
  df2 <- df %>%
    separate(species_name, c("species", "species_suffix"),
      sep = "(?= sp\\.)", extra = "merge", remove = FALSE
    ) %>%
    mutate(species_suffix = case_when(
      stringr::str_count(species, "\\w+") == 1 & is.na(species_suffix) ~ species,
      TRUE ~ species_suffix
    )) %>%
    separate(species, c("species_2", "egg_case"), sep = "(?= egg case)", remove = TRUE) %>%
    mutate(new_suffix = coalesce(species_suffix, egg_case)) %>%
    dplyr::select(-species_suffix, -egg_case)


  for (i in 1:nrow(df2)) {
    if (!is.na(df2$new_suffix[i])) {
      if (df2$species_2[i] == df2$new_suffix[i]) {
        df2$species_2[i] <- ""
      }
    }
  }
  return(df2)
}


#' find values based on strings
#'
#' @param x
#' @param col
#' @param str
#' @param str_not
#' @param col_out
#'
#' @return
#' @export
#'
#' @examples
#' find_codes(
#'   x = species, str = "skate", col = "common_name",
#'   col_out = "common_name"
#' )
#' find_codes(
#'   x = species, str = "skate", col = "common_name",
#'   col_out = "common_name", str_not = "Alaska skate"
#' )
#' find_codes(x = species, str = "skate", col = "common_name")
find_codes <- function(x, col = "common_name", str = NULL,
                       str_not = NULL, col_str_not = NULL,
                       col_out = "species_code") {
  out <- x

  if (is.null(col_str_not)) {
    col_str_not <- col
  }

  # 1. remove codes that we defintly dont want
  out <- out %>%
    dplyr::filter(
      !(grepl(
        pattern = " egg",
        x = unlist(out[, col]),
        ignore.case = TRUE
      ))
    )

  # 2. find the codes we do want
  if (!is.null(str)) {
    str <- str[!is.na(str)]
    str <- unique(str)

    for (i in 1:length(str)) {
      out <- out %>%
        dplyr::filter(
          grepl(
            pattern = str[i],
            x = as.character(unlist(out[, col])),
            ignore.case = TRUE
          )
        )
    }
  }

  # 3. remove codes that may have been included in codes we want (2)
  if (!is.null(str_not)) {
    str_not <- str_not[!is.na(str_not)]
    str_not <- unique(str_not)

    for (i in 1:length(str_not)) {
      out <- out %>%
        dplyr::filter(!(grepl(
          pattern = str_not[i],
          x = unlist(out[, col_str_not]),
          ignore.case = TRUE
        )))
    }
  }

  # clean codes
  out <- out %>%
    dplyr::select(all_of(col_out)) %>%
    unique() %>%
    unlist()

  names(out) <- NULL

  if (length(out) == 0) {
    out <- NA
  } else {
    out <- sort(out)
  }

  return(out)
}


species_table <- function(haul_spp,
                          spp_print,
                          # spp_sci,
                          SURVEY000,
                          SRVY000 = NA) {
  header <- paste0("Summary of environmental variables that ", spp_print, " (", spp_sci, ") have been found in across the ", SURVEY000, ifelse(sum(SRVY000 %in% c("NBS", "EBS")) == 2, "NEBS", paste0(" (", SRVY000, ")")))

  # Select data and make plot
  cols <- c(
    "start_latitude", "start_longitude", # "weight", "number_fish",
    "bottom_depth", "gear_temperature", "surface_temperature"
  )
  COLS <- c(
    "Latitude", "Longitude",
    # "Weight", "Abundance",
    "Bottom Depth", "Bottom Temperature", "Surface Temperature"
  )

  haul_spp <- haul_spp %>%
    dplyr::filter(SRVY %in% SRVY000)

  # if (nrow(haul_spp)==0) {

  # basiccontent0<-c()
  table_spp <- c()

  for (ii in 1:length(cols)) {
    table_spp <- rbind.data.frame(
      table_spp,
      data.frame(
        metric0 = cols[ii],
        Metric = COLS[ii],
        Min = ifelse((nrow(haul_spp) == 0), NA, min(haul_spp[, cols[ii]], na.rm = T)),
        Max = ifelse((nrow(haul_spp) == 0), NA, max(haul_spp[, cols[ii]], na.rm = T)),
        Mean = ifelse((nrow(haul_spp) == 0), NA, sum(haul_spp[, cols[ii]], na.rm = T) / nrow(haul_spp))
      )
    )
  }

  table_spp_print <- table_spp

  if (nrow(haul_spp) != 0) {
    for (ii in c("Min", "Max", "Mean")) {
      table_spp_print[, ii] <-
        trimws(formatC(
          x = table_spp_print[, ii],
          big.mark = ",",
          digits = 2, format = "f"
        ))
    }
  }
  table_spp_print$metric0 <- NULL

  # table_raw = table_spp
  # table_print = table_spp_print

  return(list(
    "header" = header,
    "raw" = table_spp,
    "print" = table_spp_print
  ))
}



# Plotting ----------------------------

# * idw plot fn depends on this one
set_breaks <- function(dat, var) {
  set.breaks0 <- classInt::classIntervals(
    var = as.numeric(unlist(dat[, var]))[as.numeric(unlist(dat[, var])) != 0],
    n = 5, style = "jenks"
  )$brks
  set.breaks <- c()

  for (i in 1:length(set.breaks0)) {
    if (i == length(set.breaks0)) {
      set.breaks <- c(set.breaks, ceiling(x = set.breaks0[i]))
    } else if (i == 1) {
      set.breaks <- c(set.breaks, 0)
    } else {
      set.breaks <- c(
        set.breaks,
        plyr::round_any(
          x = set.breaks0[i],
          accuracy = ifelse(max(set.breaks0[i]) > 300, 100,
            ifelse(max(set.breaks0[i]) > 100, 50,
              ifelse(max(set.breaks0[i]) > 20, 10,
                1
              )
            )
          ),
          f = ceiling
        )
      )
    }
  }
  set.breaks <- unique(set.breaks)

  return(set.breaks)
}


#' Lengthen a color palette
#'
#' @param x a vector of 1:n, where n is the number of unique colors you want
#' @param shortpal a short color palette, e.g., one from RColorBrewer()
#'
#' @return a vector of the same length as x, with n unique colors
#' @export
#'
#' @examples
lengthen_pal <- function(x = 1:10, shortpal) {
  ncolours <- length(unique(x))
  newpal <- colorRampPalette(shortpal)(ncolours)
  return(newpal)
}

#' Format numbers for classic "tons" format
#'
#' @param x any number
#'
#' @return a string with commas to be used in the Markdown report
#' @export
#'
#' @examples
#' format_tons(5222.168)
format_tons <- function(x){
  y <- prettyNum(round(x), big.mark = ",", scientific = FALSE)
  return(y)
}

# Save plot as a png (for using lapply with the list of figures)

make_png <- function(fig_list_element,
                     year, region,
                     savedir = dir_out_figures) {
  filename_x <- names(fig_list_element)
  png(
    filename = paste0(savedir, filename_x, "_", region, "_", year, ".png"),
    width = 10, height = 10, units = "in", res = 150
  )
  fig_list_element
  dev.off()
}

#' Make a bubble plot of the Aleutian Islands.
#'
#' @description Map of the Aleutian Islands with bubbles indicating species CPUE. Original code by Emily Markowitz. Modified and included in data reports for AI by Megsie Siple.
#'
#' @param yrs numeric vector of years for which you want plots. For data reports, this is `maxyr`.
#' @param dat dataframe of cpue by tow. Columns must include year, lat, long, cpue_kgha. For data reports, this is very similar to/the same as the table `thisyrshauldata``
#' @param key.title character - text for title of legend?
#' @param row0 numeric. I don't really know, too lazy to remove. Default is 2.
#' @param reg_dat sf object from calling akgfmaps::get_base_layers(). See example.
#' @param dist_unit string giving the units you want for distance in the scale bar
#' @param col_viridis what viridis
#' @param plot_coldpool logical. plot outline of cold pool?
#' @param plot_stratum logical. plot stratum lines?
#'
#' @return a ggplot object of a CPUE bubble map
#' @export
#'
#' @example
# library(magrittr)
#  reg_dat_ai <- akgfmaps::get_base_layers(select.region = "ai", set.crs = "auto") #
# # auto is set here under set_crs because the scale bar will be incorrect if it's set to a fixed crs (EPSG:3338)
# reg_dat_ai$survey.area <- reg_dat_ai$survey.area %>%
#   dplyr::mutate(
#     SRVY = "AI",
#     color = scales::alpha(colour = "grey80", 0.7),
#     SURVEY = "Aleutian Islands"
#   )
# reg_dat <- reg_dat_ai
# # cpue_raw is generated in prep_data.R and is a summary of cpue by species and station
# spcode <- 21921 #atka
# thisyrshauldata <- cpue_raw %>%
#   mutate(cpue_kgha = cpue_kgkm2 * 100) %>%
#   filter(year == maxyr & survey == SRVY & species_code == spcode) %>%
#   st_as_sf(
#     coords = c("start_longitude", "start_latitude"),
#     crs = "EPSG:4326"
#   ) %>%
#   st_transform(crs = reg_dat_ai$crs)
# fig <- plot_pa_xbyx(
#   spcode = spcode, dat = thisyrshauldata, yrs = c(2022), key.title = "",
#   row0 = 2, reg_dat = reg_dat_ai, dist_unit = "nm", # nautical miles
#   col_viridis = "mako", plot_coldpool = FALSE, plot_stratum = FALSE
# )
# png("Atka_bubble_2022.png", width = 8, height = 5.5, units = "in", res = 200)
# fig
# dev.off()
plot_pa_xbyx <- function(spcode, # speciescode
                         dat = thisyrshauldata,
                         yrs = c(2022),
                         key.title = "",
                         row0 = 2,
                         reg_dat = reg_dat_ai,
                         dist_unit = "nm", # nautical miles
                         col_viridis = "mako",
                         plot_coldpool = FALSE,
                         plot_stratum = FALSE) {
  legendtitle <- bquote(CPUE(kg / ha))

  f1 <- ggplot() +
    geom_sf(
      data = reg_dat$akland,
      color = NA,
      fill = "grey40"
    ) +
    geom_sf(
      data = filter(thisyrshauldata, cpue_kgha > 0),
      aes(size = cpue_kgha),
      alpha = 0.5,
      color = mako(n = 1, begin = .25, end = .75)
    ) +
    geom_sf( # x's for places where we sampled but didn't catch any of that species
      data = filter(thisyrshauldata, cpue_kgha == 0),
      alpha = 0.5,
      color = "grey5",
      shape = 4,
      size = 1
    ) +
    scale_size_area(
      name = legendtitle, max_size = 10
    )

  f2 <- f1 +
    geom_sf(
      data = reg_dat$survey.area,
      mapping = aes(color = SURVEY),
      fill = NA,
      shape = NA,
      size = .25,
      show.legend = FALSE
    ) +
    scale_color_manual(
      name = key.title,
      values = reg_dat$survey.area$color,
      breaks = rev(reg_dat$survey.area$SURVEY),
      labels = rev((reg_dat$survey.area$SRVY))
    )

  # if (plot_stratum) {
  #   figure <- figure +
  #     geom_sf(
  #       data = reg_dat$survey.strata,
  #       color = "grey50",
  #       size = 0.1,
  #       # alpha = 0,
  #       fill = NA
  #     )
  # }


  f3 <- f2 +
    ggplot2::scale_y_continuous(
      name = "", # "Latitude",
      limits = reg_dat$plot.boundary$y,
      breaks = reg_dat$lat.breaks
    ) +
    ggplot2::scale_x_continuous(
      name = "", # "Longitude",
      limits = reg_dat$plot.boundary$x,
      breaks = reg_dat$lon.breaks
    )

  f4 <- f3 +
    guides(
      size = guide_legend(
        order = 1,
        title.position = "top",
        label.position = "top",
        title.hjust = 0.5,
        nrow = 1
      )
    )


  figure <- f4 +
    theme( # set legend position and vertical arrangement
      panel.background = element_rect(
        fill = "white",
        colour = NA
      ),
      panel.border = element_rect(
        fill = NA,
        colour = "grey20"
      ),
      axis.text = element_text(size = ifelse(length(yrs) > 4 & row0 == 1, 6, 8)),
      strip.background = element_blank(),
      strip.text = element_text(size = 10, face = "bold"),
      legend.text = element_text(size = 9),
      legend.background = element_rect(
        colour = "transparent",
        fill = "transparent"
      ),
      legend.key = element_rect(
        colour = "transparent",
        fill = "transparent"
      ),
      legend.position = "bottom",
      legend.box = "horizontal"
    ) +
    labs(size = legendtitle)

  return(figure)
}

plot_idw_xbyx <- function(
    yrs,
    dat,
    lat,
    lon,
    var,
    year,
    key.title = "",
    grid = "extrapolation.grid",
    extrap.box,
    set.breaks = "auto", # seq(from = -2, to = 20, by = 2),
    grid.cell = c(0.02, 0.02),
    row0 = 2,
    region = "bs.south",
    dist_unit = "nm", # nautical miles
    col_viridis = "mako",
    plot_coldpool = FALSE,
    plot_stratum = TRUE,
    use.survey.bathymetry = FALSE) {
  reg_dat <- akgfmaps::get_base_layers(select.region = region)
  yrs <- as.numeric(sort(x = yrs, decreasing = T))
  figure <- ggplot()
  dat <- dat %>%
    dplyr::rename(
      year = as.character(year),
      lat = as.character(lat),
      lon = as.character(lon),
      var = as.character(var)
    ) %>%
    dplyr::select(year, lat, lon, var) %>%
    dplyr::mutate(
      year = as.numeric(year),
      lat = as.numeric(lat),
      lon = as.numeric(lon)
    )

  if (nrow(dat) != 0) {
    if (set.breaks[1] == "auto") {
      set.breaks <- set_breaks(dat = dat, var = "var")
    }

    # Select data and make plot
    for (ii in length(yrs):1) {
      temp <- dat %>%
        dplyr::filter(year == yrs[ii])

      temp1 <- akgfmaps::make_idw_map(
        LATITUDE = temp$lat,
        LONGITUDE = temp$lon,
        CPUE_KGHA = temp$var,
        use.survey.bathymetry = use.survey.bathymetry,
        region = region,
        out.crs = as.character(crs(reg_dat$bathymetry)),
        extrap.box = extrap.box,
        set.breaks = set.breaks,
        grid.cell = grid.cell,
        key.title = key.title
      )

      temp0 <- temp1[grid][[1]]

      if (ii == length(yrs)) {
        stars_list <- temp0
        names(stars_list)[names(stars_list) == "var1.pred"] <- paste0("y", yrs[ii])
      } else {
        stars_list$temp <- temp0$var1.pred
        names(stars_list)[names(stars_list) == "temp"] <- paste0("y", yrs[ii])
      }
    }


    # stars_list0<-stars_list

    # https://rpubs.com/michaeldorman/646276
    stars_list <- stars_list %>%
      dplyr::select(names(stars_list)[substr(start = 1, stop = 1, x = names(stars_list)) == "y"])
    names(stars_list) <- gsub(pattern = "y", replacement = "", x = names(stars_list))
    stars_list <- stars::st_redimension(stars_list)
    names(stars_list) <- "value"


    figure <- figure +
      geom_stars(data = stars_list, na.rm = TRUE)
  }



  if (plot_coldpool) {
    temp_break <- 2 # 2*C

    coords <- raster::coordinates(coldpool:::nbs_ebs_bottom_temperature)

    for (i in 1:length(yrs)) {
      sel_layer_df <- data.frame(
        x = coords[, 1],
        y = coords[, 2],
        temperature = coldpool:::nbs_ebs_bottom_temperature@data@values[, i]
      )
      sel_layer_df <- sel_layer_df[!is.na(sel_layer_df$temperature), ]
      sel_layer_df$year <- yrs[i]

      if (i == 1) {
        bt_year_df <- sel_layer_df
      } else {
        bt_year_df <- dplyr::bind_rows(bt_year_df, sel_layer_df)
      }
    }

    figure <- figure +
      ggplot2::geom_tile(
        data = bt_year_df %>%
          dplyr::filter(temperature <= temp_break) %>%
          dplyr::rename(new_dim = year),
        aes(
          x = x,
          y = y,
          group = new_dim
        ),
        fill = "magenta", # "gray80",
        alpha = 0.5
      )
  }

  if (length(yrs) == 0) {
    grid <- ""
    figure <- figure +
      ggplot2::geom_text(
        mapping = aes(
          x = mean(reg_dat$lon.breaks),
          y = mean(reg_dat$lat.breaks),
          label = "No data was available\nfor this species in this\nregion for this year."
        ),
        fontface = "bold"
      )
  } else if (length(yrs) > 1) {
    figure <- figure +
      facet_wrap(~new_dim, nrow = row0) +
      coord_equal()
  }


  if (plot_stratum) {
    figure <- figure +
      geom_sf(
        data = reg_dat$survey.strata,
        color = "grey50",
        size = 0.1,
        alpha = 0,
        fill = NA
      )
  }

  lon_break <- reg_dat$lon.breaks
  lat_break <- reg_dat$lat.breaks
  if (length(yrs) > 6) {
    lon_break <- reg_dat$lon.breaks[rep_len(x = c(FALSE, TRUE), length.out = length(lon_break))]
    lat_break <- reg_dat$lat.breaks[rep_len(x = c(FALSE, TRUE), length.out = length(lat_break))]
  }

  figure <- figure +
    geom_sf(
      data = reg_dat$graticule,
      color = "grey80",
      alpha = 0.2
    ) +
    geom_sf(
      data = reg_dat$akland,
      color = NA,
      fill = "grey50"
    ) +
    scale_y_continuous(
      name = "", # "Latitude",,
      # labels = lat_break,
      # labels = reg_dat$lat.breaks,
      limits = reg_dat$plot.boundary$y,
      breaks = lat_break
    ) +
    scale_x_continuous(
      name = "", # "Longitude"#,
      # labels = reg_dat$lon.breaks,
      limits = reg_dat$plot.boundary$x,
      breaks = lon_break
    ) +
    # coord_sf(xlim = reg_dat$plot.boundary$x,
    #          ylim = reg_dat$plot.boundary$y)  +
    ggsn::scalebar(
      data = reg_dat$survey.grid,
      location = "bottomleft",
      dist = 150,
      dist_unit = dist_unit,
      transform = FALSE,
      st.dist = ifelse(row0 > 2, 0.08, 0.04),
      height = ifelse(row0 > 2, 0.04, 0.02),
      st.bottom = FALSE, # ifelse(row0 <= 2, TRUE, FALSE),
      st.size = ifelse(row0 > 2, 2.5, 3), # 2.5
      model = reg_dat$crs
    )


  if (grid == "continuous.grid") {
    figure <- figure +
      scale_fill_viridis_c(
        option = col_viridis,
        # limits = range(set.breaks),
        na.value = "transparent",
        breaks = set.breaks,
        labels = set.breaks
      ) +
      guides(fill = guide_colourbar(
        title = key.title,
        title.position = "top",
        title.hjust = 0.5
      ))
  } else if (grid == "extrapolation.grid") {
    # temp <- factor(x = temp0$var1.pred, levels = levels(temp0$var1.pred), labels = levels(temp0$var1.pred), ordered = T)
    figure <- figure +
      scale_fill_manual(
        values = c(
          "gray90",
          viridis::mako(
            direction = -1,
            n = temp1$n.breaks,
            begin = 0,
            end = 0.80
          )
        ),
        name = key.title,
        na.value = "transparent",
        breaks = levels(temp0$var1.pred),
        labels = levels(temp0$var1.pred)
      )
  }

  figure <- figure +
    guides(
      fill = guide_legend(
        title.position = "top",
        label.position = "bottom",
        title.hjust = 0.5,
        nrow = 1
      )
    ) +

    # set legend position and vertical arrangement
    theme(
      # axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
      panel.background = element_rect(
        fill = "white",
        colour = NA
      ),
      panel.border = element_rect(
        fill = NA,
        colour = "grey20"
      ),
      axis.text = element_text(size = 8),
      strip.background = element_blank(),
      strip.text = element_text(size = 10, face = "bold"),
      # legend.title = element_text(size = 12), #, vjust = .5, hjust = .3),
      legend.text = element_text(size = 10),
      legend.background = element_rect(
        colour = "transparent",
        fill = "transparent"
      ),
      legend.key = element_rect(
        colour = "transparent",
        fill = "transparent"
      ),
      # legend.title.align = 0,#.1,
      legend.position = "bottom",
      # legend.box.just = "center",
      # legend.key.width = unit(.5, "in"),
      legend.box = "horizontal"
    )

  return(figure)
}

# LATER: Add an independent function to plot CPUE as bars instead of circles
# plot_ianelli <- function(region, species, year){
#   key.title <- ""
#   yrs <- c(2023)
#   row0 <- 2 # default
#   legendtitle <- bquote(CPUE(kg / ha)) # inside fn
# }

# Tables -----------------------------------------------------------------------
make_table_4 <- function(biomass_stratum = biomass_stratum,
                         region_lu = region_lu, # This is a modified goa_strata
                         species_code) {
  x <- region_lu %>%
    dplyr::select(SURVEY, INPFC_AREA, STRATUM, DESCRIPTION, MIN_DEPTH, MAX_DEPTH) %>%
    left_join(biomass_stratum, by = c("SURVEY", "STRATUM")) %>%
    filter(YEAR == maxyr & SPECIES_CODE == species_code) %>%
    dplyr::select(
      INPFC_AREA, MIN_DEPTH, MAX_DEPTH, DESCRIPTION, HAUL_COUNT,
      CATCH_COUNT, MEAN_WGT_CPUE,
      STRATUM_BIOMASS, MIN_BIOMASS, MAX_BIOMASS
    ) %>%
    arrange(desc(MEAN_WGT_CPUE))

  # Format and rename columns
  xx <- x %>%
    tidyr::unite("Depth (m)", MIN_DEPTH:MAX_DEPTH, sep = " - ", remove = FALSE) %>%
    mutate(MEAN_WGT_CPUE_KGHA = MEAN_WGT_CPUE / 100) %>% # convert CPUE from  kg/km2 to kg/ha
    dplyr::rename(
      `Number of hauls` = HAUL_COUNT,
      `Survey district` = INPFC_AREA,
      `Hauls with catch` = CATCH_COUNT,
      `Mean CPUE (kg/ha)` = MEAN_WGT_CPUE_KGHA,
      `Biomass (t)` = STRATUM_BIOMASS,
      `LCL (t)` = MIN_BIOMASS,
      `UCL (t)` = MAX_BIOMASS
    ) %>%
    dplyr::select(-MIN_DEPTH, -MAX_DEPTH)

  return(xx)
}


# Allow breaks between sections in flextables
break_position <- function(x) {
  z <- data.table::rleidv(x)
  c(z[-length(z)] != z[-1], FALSE)
}



# function to identify outliers in species IDs for each year
check_outlier <- function(species_code, year, catch_data, plot = FALSE) {
  sp_catch <- catch_data %>%
    filter(SPECIES_CODE == species_code) %>%
    dplyr::select(SPECIES_NAME, START_LONGITUDE, START_LATITUDE, YEAR)

  clustering <- dbscan(sp_catch[, 2:3], eps = 0.9, minPts = 2, borderPoints = FALSE)


  # outliers from this year
  tmp <- sp_catch %>%
    mutate(
      cluster = clustering$cluster,
      outlier = ifelse(cluster == 0, "flag", "")
    ) %>%
    filter(YEAR == year)


  # vector to plot
  o <- tmp[tmp$outlier != "", ]

  out <- tmp %>%
    dplyr::select(-cluster) %>%
    filter(outlier != "")


  if (plot & length(o) > 0) {
    world <- map_data("world2", wrap = c(40, 400)) %>%
      filter(region %in% c("Russia", "USA", "Canada"))
    sp <- sp_catch$SPECIES_NAME[1]

    p <- ggplot() +
      geom_polygon(
        data = world, aes(x = long, y = lat, group = group),
        col = "grey60", fill = "grey90", lwd = 0
      ) +
      coord_map(ylim = c(45, 70), xlim = c(150, 250)) +
      theme_bw() +
      labs(x = "Longitude", y = "Latitude") +
      geom_point(
        data = sp_catch,
        aes(x = START_LONGITUDE, y = START_LATITUDE), cex = 1
      ) +
      geom_point(
        data = o, aes(x = START_LONGITUDE, y = START_LATITUDE),
        col = "red", cex = 1.5
      ) +
      ggtitle(label = sp)
    print(p)
  }

  return(out)
}
