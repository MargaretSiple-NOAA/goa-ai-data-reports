# Conversions --------------------------------------------
biomass_round <- function(x) {
  round(x, digits = 0)
}

# https://github.com/geanders/weathermetrics/blob/master/R/temperature_conversions.R
c2f <- function(T.celsius, round = 2) {
  T.fahrenheit <- (9 / 5) * T.celsius + 32
  T.fahrenheit <- round(T.fahrenheit, digits = round)
  return(T.fahrenheit)
}

# Other conversions
divnmi2forkm2 <- 1 / 3.429904
divkm2fornmi2 <- 3.429904
divkm2forha <- 100
divmforft <- 0.3048
divftform <- 3.28084


#' Test whether there is a difference in mean size between the sexes
#'
#' @param species_lengths a data frame with columns "SURVEY"       "YEAR"         "SPECIES_CODE" "SUMMARY_AREA" "LENGTH"       "MALES"        "FEMALES"      "UNSEXED"    "TOTAL", filtered to the species and year that you are interested in. Can get it from the SIZECOMP table in the AI or GOA schemas or from GAP_PRODUCTS (currently it comes from the GOA and AI schemas)
#'
#' @return A character string as sentence that you can paste into the species chapter as a bullet point. If there is a sex difference in mean size, it will return a sentence about the direction. If there isn't a sex difference in size, it will return nothing.
#' @export
#'
#' @examples
sex_diff_size_statement <- function(species_lengths) {
  # Test df
  # species_lengths <- filter(sizecomp, SPECIES_CODE==10110 & YEAR==maxyr)
  if (length(unique(species_lengths$SURVEY)) > 1) {
    stop("Error in sex_diff_size_statement(). This dataset contains size data for more than one region. Fix the function or the dataset.")
  }

  # Divide by 1e4 to make the integers smaller; doesn't matter as long as proportions are right.
  species_lengths$MALES <- species_lengths$MALES / 1e4
  species_lengths$FEMALES <- species_lengths$FEMALES / 1e4

  males <- species_lengths$LENGTH * (species_lengths$MALES / sum(species_lengths$MALES))
  females <- species_lengths$LENGTH * (species_lengths$FEMALES / sum(species_lengths$FEMALES))



  z <- ks.test(males, females)
  diff <- ifelse(z$p.value < 0.05, TRUE, FALSE)
  if (diff) {
    pt_1 <- "Males and females of this species differed in size; "
    # which of the sexes are larger?
    if (sum(males) > sum(females)) {
      pt_2 <- paste0("males (mean FL ", round(sum(males) / 10, digits = 2), ") are generally longer than females (mean FL ", round(sum(females) / 10, digits = 2), " cm).")
    }
    if (sum(females) > sum(males)) {
      pt_2 <- paste0("females (mean FL ", round(sum(females) / 10, digits = 2), " cm) are generally longer than males (mean FL ", round(sum(males) / 10, digits = 2), " cm).")
    }
  } else {
    pt_1 <- ""
    pt_2 <- ""
  }
  statement <- paste0(pt_1, pt_2)
  return(statement)
}

# Function to calculate confidence intervals for lognormal distribution - hopefully will be deprecated starting with the AI 2024 DPR.
lognorm_ci <- function(mean, variance, alpha = 0.05) {
  sigma <- sqrt(log(1 + variance/mean^2)) # Calculate standard deviation
  z <- qnorm(1 - alpha/2) # Calculate z-value for given alpha (two-tailed)
  
  # Calculate lower and upper confidence bounds
  lower <- exp(log(mean) - z * sigma)
  upper <- exp(log(mean) + z * sigma)
  
  return(c(lower, upper))
}


# Text formatting -------------------------------------------------------
#' Fix inserted text when there are two areas or depths with the "greatest biomass" (i.e., if it's the same for both and they're both the max).
#'
#' @param x text string from soliciting the areas with the top CPUE or biomass or whatever
#'
#' @return a string with both of those areas
#' @export
#'
#' @examples
fix_co_greatest <- function(text_string) {
  if (startsWith(x = text_string, "c(")) {
    text_string2 <- sub("c(", "", text_string, fixed = TRUE)
    text_string3 <- gsub("[^-,/a-zA-Z0-9[:space:]]+", "", text_string2, perl = TRUE)
    newtext <- gsub("[,]", ", ", text_string3, perl = TRUE)
  } else {
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
chr_to_num <- function(x) {
  y <- x
  y[which(x == "---" | x == "<1")] <- NA
  y <- as.numeric(gsub(",", "", y))

  return(y)
}


# Tables -----------------------------------------------
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
    ) 
  return(x)
}


#' Using a spreadsheet from the old methodology, make a list of the top 20 species by CPUE.
#'
#' @details Currently this is our chosen method becaus eit eliminates the weird tiny discrepancies that we get when calculating CPUEs by hand vs. using summary tables from the AI schema.
#' @param filepath The path of the file containing the CPUE table. Currently, Paul produces this using SQL plus some excel wizardry.
#'
#' @return a formatted table of top 20 CPUEs by area, analogous to the one produced by make_top_cpue()
#' @export
#'
#' @examples
prep_tab2 <- function(filepath = paste0(dir_in_premadetabs, "Table 2/", "Table 2_AI2022_makeitlooklikethisplease.xlsx")) {
  if (!file.exists(filepath)) {
    stop("Species Table 2 file missing from the folder. Check directory and make sure you're on the VPN and the filename you've specified is correct, including the year, region, and folder.")
  }
  raw <- readxl::read_excel(filepath)
  # not_all_na <- function(x) any(!is.na(x))
  # raw2 <- raw %>%
  #   dplyr::select(where(not_all_na))

  haulcounts <- raw %>%
    dplyr::filter(species == "Number of hauls") %>%
    dplyr::rename("nhauls" = CPUE) %>%
    dplyr::select(-species)

  raw2 <- raw %>%
    dplyr::filter(species != "Number of hauls") %>%
    fuzzyjoin::regex_left_join(species_codes, by = c("species" = "COMMON_NAME")) %>%
    dplyr::select(-species) %>%
    dplyr::rename(
      "wgted_mean_cpue_kgha" = "CPUE",
      "INPFC_AREA" = "INPFC area",
      "species_code" = "SPECIES_CODE",
      "common_name" = "COMMON_NAME",
      "scientific_name" = "SPECIES_NAME"
    ) %>%
    dplyr::arrange(factor(INPFC_AREA, levels = district_order))

  return(raw2)
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
# prep_tab3 <- function(speciescode, premade = TRUE) {
#   if(premade){
#     filepath <- paste0(dir_in_premadetabs, "Table 3/", speciescode, paste0("_", maxyr, ".csv"))
#     if (!file.exists(filepath)) {
#       stop("Species Table 3 file missing from the folder. Check directory and make sure you're on the VPN.")
#     }
#     x <- read.csv(file = filepath)
#     # Fix this later
#     if (SRVY == "AI") {
#       x <- x %>%
#         dplyr::slice(21:25, 1:20) # sloppy way to slice off the SBS and move it to the top
#     } #/ AI exception
#     
#     cleaned_tab <- x %>%
#       dplyr::rename(
#         `Survey district` = Survey.District,
#         `Depth (m)` = Depth..m.,
#         `Haul count` = Haul.Count,
#         `Hauls with catch` = Hauls.w.Catch,
#         `CPUE (kg/ha)` = CPUE..kg.ha.,
#         `Biomass (t)` = Biomass...t.,
#         `Lower 95% CL` = X95..LCL..t.,
#         `Upper 95% CL` = X95..UCL..t.,
#         `Mean weight (kg)` = Weight...kg.
#       ) |>
#       dplyr::filter(`Depth (m)` != "701 - 1000")
#     
#     
#   } #/ if(premade)
#   
#   # NOTE: HERE I WANT TO ADD A VERSION THAT TAKES IN TABLE 3 FROM THE *processed/table_3_allspps.csv file that I painstakingly made from GAP_PRODUCTS!
# 
#   return(cleaned_tab)
# }

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
      `Lower 95% CL` = Lower.CI.Biomass,
      `Upper 95% CL` = Upper.CI.Biomass
    ) %>%
    arrange(factor(`Survey district`, levels = district_order), `Depth range (m)`)

  return(cleaned_tab)
}


#' Make "table 3" - haul count, hauls, CPUE, Biomass, confidence limits, and avg weight per haul
#'
#' @param species_code five-digit species code
#' @param survey "GOA" or "AI"
#' @param year  Year of survey
#' @param biomass_tbl BIOMASS table, taken straight from GAP_PRODUCTS, then filtered to the survey definition ID in report_settings.R
#' @param area_tbl AREA table taken straight from GAP_PRODUCTS
#'
#' @return a draft of table 3 like the ones produced by Paul before!
#' @export
#'
#' @examples
#' 
#biomass_tbl <- read.csv("./data/local_gap_products/biomass.csv",header=TRUE)
#area_tbl <- read.csv("./data/local_gap_products/area.csv",header=TRUE)
make_tab3 <- function(species_code = NULL, year = NULL, biomass_tbl, area_tbl) {
  if (length(unique(biomass_tbl$SURVEY_DEFINITION_ID)) > 1) {
    stop("More than one survey definition ID.")
  }
  # Filter the two raw tables
  biomass_yr <- biomass_tbl |> # might take this out of the function, not sure yet
    dplyr::filter(YEAR == year & SPECIES_CODE == species_code) |>
    dplyr::mutate(AVG_WEIGHT_KG = round((BIOMASS_MT / POPULATION_COUNT) * 1000, digits = 3))

  biomass_yr[which(biomass_yr$POPULATION_COUNT == 0), "AVG_WEIGHT_KG"] <- "--"
  #biomass_yr[which(biomass_yr$AVG_WEIGHT_KG < 0.001), "AVG_WEIGHT_KG"] <- "< 0.001"
  biomass_yr[which(biomass_yr$POPULATION_COUNT > 0 & biomass_yr$AVG_WEIGHT_KG < 0.001), "AVG_WEIGHT_KG"] <- "< 0.001"

  area_lookup0 <- area_tbl |>
    dplyr::filter(AREA_TYPE %in% c(
      "INPFC BY DEPTH",
      "INPFC",
      "DEPTH", "REGION"
    ))
  if (biomass_yr$SURVEY_DEFINITION_ID[1] == 47) {
    area_lookup <- area_lookup0 |>
      dplyr::filter(DESIGN_YEAR == ifelse(year < 2025, 1984, 2025)) # GOA design years
  } else {
    area_lookup <- area_lookup0 # All AI design years are 1980
  }


  combo0 <- area_lookup |>
    left_join(biomass_yr, by = join_by(SURVEY_DEFINITION_ID, AREA_ID)) |>
    dplyr::mutate(DEPTH_RANGE = paste(DEPTH_MIN_M, "-", DEPTH_MAX_M)) |>
    dplyr::mutate(DEPTH_RANGE = case_when(
      DEPTH_RANGE %in% c("1 - 500", "1 - 1000") ~ "All depths",
      DEPTH_RANGE == "NA - NA" ~ "All depths",
      TRUE ~ DEPTH_RANGE
    )) |>
    dplyr::select(
      AREA_NAME, DEPTH_RANGE,
      N_HAUL, N_WEIGHT,
      CPUE_KGKM2_MEAN,
      BIOMASS_MT,
      # BIOMASS_VAR,
      AVG_WEIGHT_KG
    )

  # Format the columns
  combo <- combo0 |>
    dplyr::rename(
      "Survey district" = AREA_NAME,
      "Depth (m)" = DEPTH_RANGE,
      "Total haul count" = N_HAUL,
      "Hauls with positive catch" = N_WEIGHT,
      "CPUE (kg/km2)" = CPUE_KGKM2_MEAN,
      "Biomass (mt)" = BIOMASS_MT,
      # "Biomass variance (t)" = BIOMASS_VAR,
      "Average weight (kg)" = AVG_WEIGHT_KG
    )

  # Format numbers in CPUE and biomass columns
  combo$`CPUE (kg/km2)` <- round(combo$`CPUE (kg/km2)`, digits = 1)
  combo$`Biomass (t)` <- format(round(combo$`Biomass (mt)`), big.mark = ",")

  combo_ord <- combo |>
    dplyr::arrange(factor(`Survey district`, levels = c(district_order, "All")))

  combo_ord$`Survey district`[which(combo_ord$`Survey district` == "All")] <- "All areas"

  return(combo_ord)
}

#' Make Table 4 - summary of CPUE and biomass by stratum
#'
#' @param species_code five-digit species code
#' @param year  Year of survey
#' @param biomass_tbl BIOMASS table, taken straight from GAP_PRODUCTS, then filtered to the survey definition ID in report_settings.R
#' @param area_tbl AREA table taken straight from GAP_PRODUCTS
#'
#' @return a draft of Table 4 for the data report - CPUE summarized by stratum
#' @export
#'
#' @examples
make_tab4 <- function(species_code = NULL, year = NULL, biomass_tbl, area_tbl) {
  if (length(unique(biomass_tbl$SURVEY_DEFINITION_ID)) > 1) {
    stop("More than one survey definition ID.")
  }

  # Filter the two raw tables
  biomass_yr <- biomass_tbl |> # might take this out of the function, not sure yet
    dplyr::filter(YEAR == year & SPECIES_CODE == species_code)

  area_lookup <- area_tbl |>
    dplyr::filter(AREA_TYPE == "STRATUM") |>
    dplyr::mutate(DEPTH_RANGE = paste(DEPTH_MIN_M, "-", DEPTH_MAX_M))

  combo0 <- area_lookup |>
    dplyr::left_join(biomass_yr, by = c("SURVEY_DEFINITION_ID", "AREA_ID")) |>
    dplyr::select(
      AREA_NAME, DEPTH_RANGE,
      N_HAUL, N_WEIGHT,
      CPUE_KGKM2_MEAN, BIOMASS_MT
    )

  combo <- combo0 |>
    dplyr::rename(
      "Area name" = AREA_NAME,
      "Depth (m)" = DEPTH_RANGE,
      "Total haul count" = N_HAUL,
      "Hauls with positive catch" = N_WEIGHT,
      "CPUE (kg/km2)" = CPUE_KGKM2_MEAN,
      "Biomass (mt)" = BIOMASS_MT
    ) |>
    dplyr::filter(`Hauls with positive catch` > 0) # only show lines for strata where the species appeared

  combo$`CPUE (kg/km2)` <- round(combo$`CPUE (kg/km2)`, digits = 1)
  combo$`Biomass (mt)` <- format(round(combo$`Biomass (mt)`), big.mark = ",")

  return(combo)
}

#' Make a rough draft of Table 4 with SQL - old and likely going to be deprecated bc uses old tables
#'
#' @param species_code species code (numeric)
#' @param survey survey code, "AI" or "GOA" (character)
#' @param year survey year (numeric)
#'
#' @return writes a csv file for each species for table 4.
#' @export
#' @details
#' This function uses a table called GOA_STRATA but that table does contain all strata (both AI and GOA)
#'
#' @examples
#' source("R/get_connected.R")
#' make_tab4(species_code = 30060, survey = "GOA", year = 2023)
#'
# make_tab4_sql <- function(species_code = NULL, survey = NULL, year = NULL) {
#   a <- RODBC::sqlQuery(channel, paste0(
#     "SELECT DISTINCT INPFC_AREA SURVEY_DISTRICT, MIN_DEPTH||'-'||MAX_DEPTH DEPTH_M, DESCRIPTION SUBDISTRICT_NAME, HAUL_COUNT NUMBER_OF_HAULS, CATCH_COUNT HAULS_W_CATCH, MEAN_WGT_CPUE/100 CPUE_KG_HA, STRATUM_BIOMASS BIOMASS_T, MIN_BIOMASS LCL_T, MAX_BIOMASS UCL_T FROM GOA.GOA_STRATA a, ", survey, ".BIOMASS_STRATUM b WHERE a.SURVEY = \'", survey,"\' and b.YEAR = ", year," and b.SPECIES_CODE = ",species_code," and a.STRATUM = b.STRATUM order by -CPUE_KG_HA"
#   ))
#   
#   dir_out <- paste0("data/local_", tolower(survey), "_processed/table4_", species_code, "_", survey, "_", year, ".csv")
#   
#   write.csv(x = a, file = dir_out, row.names = FALSE)
# }


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


# Allow breaks between sections in flextables
break_position <- function(x) {
  z <- data.table::rleidv(x)
  c(z[-length(z)] != z[-1], FALSE)
}


# Working with data -------------------------------------------
#' Title
#'
#' @param x an akgf maps object like ai_east
#' @param stratum_lookup_tab a region-specific lookup table of strata and the max depth associated with each
#'
#' @return an akgfmaps object with max depth now included in the survey.strata list element. Should be indispensable for figures! I think!
#' @export
#'
#' @examples
add_depths <- function(x, stratum_lookup_tab = stratum_lookup) {
  y <- x$survey.strata |>
    left_join(stratum_lookup_tab, by = c("STRATUM" = "AREA_ID"))
  z <- x
  z$survey.strata <- y

  return(z)
}

# Plots ----------------------------

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
format_tons <- function(x) {
  y <- prettyNum(round(x), big.mark = ",", scientific = FALSE)
  return(y)
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

