# PRESENTATION FIGURES 
# Project: Automated GOA-AI data reports and slides
# Author: Megsie Siple
# Notes: Use this to make pptx slides for the Joint Groundfish Plan Team presentation. This uses tables from Oracle (RACEBASE, GAP_PRODUCTS, and AI and GOA schemas [though these should soon be deprecated]) and builds the figures and some summary stats.
# ---


# Table of contents (toggle true/false to make some plots but not others):

# 1. Biomass indices relative to LT mean
make_biomass_timeseries <- TRUE
# 2. Catch composition
make_catch_comp <- TRUE
# 3. CPUE bubble map (Aleutians only)
make_cpue_bubbles <- FALSE
make_cpue_bubbles_strata <- TRUE
# The map that Jim I requested a while ago. It has bars instead of bubbles. Some assessment ppl like it:
make_cpue_ianelli <- FALSE
# 4. Length frequency plots by region and depth stratum (probably deprecated - not annual increments)
make_length_freqs <- FALSE
# 5. Length frequency plots as joy division plots (preferred length plot by stock assessment folx)
make_joy_division_length <- TRUE
# 6. CPUE IDW maps
make_cpue_idw <- FALSE
# 7. Plots of surface and bottom temperature
make_temp_plot <- TRUE
# make a special combined biomass plot for rebs
make_complexes_figs <- TRUE


# Report settings -------------------------------------------------------------
source("R/00_report_settings.R")
source("R/01_directories.R")

SRVY <- "GOA"
maxyr <- 2025 # Change this for the year!
compareyr <- 2023
dates_conducted <- "May 25th through August 3rd 2025" # EDIT
if (SRVY == "GOA") {
  all_allocation <- read.csv(here::here("data", "local_goa", "goa_station_allocation.csv"))
  preassignedstationstatement <- "This year, we pre-assigned XX% of the total XXX stations allocated as “new” meaning the each vessel had to trawl around a dozen previously untrawled stations last summer
and this will become a permanent feature of our station allocations in the future."
}

# Load packages and functions -------------------------------------------------
source("R/02_load_packages.R")
source("R/03_functions.R")

# Get data from Oracle --------------------------------------------------------
x <- FALSE
if (x) {
  dir.create("data/local_racebase", recursive = TRUE)
  source("R/05_download_data_from_oracle.R")
}


# General data -----------------------------------------------------------------
# Get species table
if (SRVY == "AI") report_species0 <- read.csv("data/ai_report_specieslist.csv")
if (SRVY == "GOA") report_species0 <- read.csv("data/goa_report_specieslist.csv")

report_species <- report_species0 |>
  dplyr::filter(presentation == 1) 


# Get a table of the strata and depths / regions
dat <- read.csv("data/goa_strata.csv", header = TRUE) # includes GOA and AI strata

# Prep values and prelim tables -------------------------------------------
source("R/06_prep_data.R") # takes a min

# Data for plots ------------------------------------------------------------
# All the species for which we want to make plots
head(report_species)

# Get key/table of names (common, scientific, etc)
common_names <- read.csv(here::here("data", "local_racebase", "species.csv"), header = TRUE)

# Haul data from RACEBASE
haul <- read.csv(here::here("data", "local_racebase", "haul.csv"))

nyears <- length(unique(filter(haul, REGION == SRVY)$CRUISE))

# Haul summary table
haul2 <- haul %>%
  mutate(YEAR = stringr::str_extract(CRUISE, "^\\d{4}")) %>%
  filter(YEAR == maxyr & REGION == SRVY)

haul_maxyr <- haul %>%
  mutate(YEAR = as.numeric(gsub("(^\\d{4}).*", "\\1", CRUISE))) %>% # extract year
  filter(REGION == SRVY & YEAR == maxyr)

nstations <- haul2 %>%
  filter(ABUNDANCE_HAUL == "Y") %>%
  distinct(STATIONID, STRATUM) %>%
  nrow() # checked in 2023

nsuccessfulhauls <- haul2 %>%
  filter(ABUNDANCE_HAUL == "Y") %>%
  nrow()

# Data for text ---------------------------------------------------
# Length data from racebase:
L <- read.csv(here::here("data/local_racebase/length.csv"))
L <- L %>%
  mutate(YEAR = as.numeric(gsub("(^\\d{4}).*", "\\1", CRUISE)))
length_maxyr <- filter(L, YEAR == maxyr & REGION == SRVY)

# Lengths collected
lengths_collected <- sum(length_maxyr$FREQUENCY) %>%
  format(big.mark = ",")

nfishlengths <- sum(length_maxyr %>%
  filter(LENGTH_TYPE %in% c(1, 5, 11)) %>%
  dplyr::select(FREQUENCY)) %>%
  sum() %>%
  format(big.mark = ",")

nfishlengths_reportspps <- length_maxyr |>
  filter(LENGTH_TYPE %in% c(1, 5, 11)) |>
  filter(SPECIES_CODE %in% report_species$species_code) |>
  dplyr::select(FREQUENCY) |>
  sum() |>
  format(big.mark = ",")

nsquidlengths <- sum(length_maxyr |>
  dplyr::filter(LENGTH_TYPE == 12) |>
  dplyr::select(FREQUENCY)) |>
  format(big.mark = ",")

# Otoliths collected
S <- read.csv(here::here("data", "local_racebase", "specimen.csv"))
specimen_maxyr <- S |>
  mutate(YEAR = as.numeric(gsub("(^\\d{4}).*", "\\1", CRUISE))) |>
  filter(YEAR == maxyr & REGION == SRVY)

total_otos_collected <- specimen_maxyr |>
  filter(SPECIMEN_SAMPLE_TYPE == 1) |>
  nrow() |>
  format(big.mark = ",")

n_oto_species <- specimen_maxyr |>
  filter(SPECIMEN_SAMPLE_TYPE == 1) |>
  count(SPECIES_CODE) |>
  nrow()

otos_collected <- specimen_maxyr |>
  filter(SPECIMEN_SAMPLE_TYPE == 1) |> # SAMPLE_TYPE==1 means it's an oto collection
  dplyr::left_join(haul_maxyr, by = c(
    "CRUISEJOIN", "HAULJOIN", "HAUL",
    "REGION", "VESSEL", "YEAR"
  )) |>
  dplyr::left_join(region_lu, by = c("STRATUM")) |>
  group_by(INPFC_AREA, `Depth range`) |>
  dplyr::summarize("Pairs of otoliths collected" = n()) |>
  ungroup() |>
  arrange(factor(INPFC_AREA, levels = district_order))

# Temperature info
minbottomtemp <- min(haul_maxyr$GEAR_TEMPERATURE[which(haul_maxyr$GEAR_TEMPERATURE > 0)],
  na.rm = T
)
maxbottomtemp <- max(haul_maxyr$GEAR_TEMPERATURE,
  na.rm = T
)

minsurfacetemp <- min(haul_maxyr$SURFACE_TEMPERATURE[which(haul_maxyr$SURFACE_TEMPERATURE > 0)],
  na.rm = T
)
maxsurfacetemp <- max(haul_maxyr$SURFACE_TEMPERATURE,
  na.rm = T
)


# Base maps ---------------------------------------------------------------
if (SRVY == "AI") {
  ai_east <- akgfmaps::get_base_layers(
    select.region = "ai.east",
    set.crs = "auto"
  ) 
  ai_central <- akgfmaps::get_base_layers(
    select.region = "ai.central",
    set.crs = "auto"
  )
  ai_west <- akgfmaps::get_base_layers(
    select.region = "ai.west",
    set.crs = "auto"
  )
  
  # Make a category that is just the depth of the stratum, for easy labeling
  ai_east$survey.strata <- ai_east$survey.strata |>
    mutate(strat_depth = substr(STRATUM, 3, 3))
  
  nstrata <- length(unique(floor(ai_east$survey.grid$STRATUM / 10)))
}

if (SRVY == "GOA") {
#   a <- read.csv("data/goa_strata.csv")
#   a <- dplyr::filter(a, MIN_DEPTH < 700 & SURVEY == "GOA")
#   nstrata <- length(unique(a$STRATUM))
# } else {
#   a <- read.csv("data/goa_strata.csv")
#   a <- dplyr::filter(a, SURVEY == "AI")
#   nstrata <- length(unique(a$STRATUM))
  
  stratum_lookup <- read.csv("data/local_gap_products/area.csv") |>
    dplyr::filter(AREA_TYPE == "STRATUM" & DESIGN_YEAR == ifelse(maxyr < 2025, 1984, 2025)) |>
    dplyr::select(AREA_ID, DEPTH_MAX_M)
  nstrata <- length(unique(stratum_lookup$AREA_ID))
  
  ndepths <- length(unique(stratum_lookup$DEPTH_MAX_M)) # number of max depth intervals (for AI, it's 100,200,300,500)
  
  goa_all <- akgfmaps::get_base_layers(
    select.region = "goa",
    set.crs = "auto",
    design.year = ifelse(maxyr < 2025, 1984, 2025)
  ) |>
    add_depths()
  
  goa_inpfc <- goa_all$inpfc.strata
  
  geo_order <- c("Shumagin", "Chirikof", "Kodiak", "Yakutat", "Southeastern")
  
  goa_all$survey.area <- goa_all$survey.area |>
    dplyr::mutate(
      SRVY = "GOA",
      color = scales::alpha(colour = "grey80", 0.7),
      SURVEY = "Gulf of Alaska"
    )
  
  reg_data <- goa_all
}

# Load map stuff if making any kind of bubble maps
if (make_cpue_bubbles | make_cpue_ianelli | make_cpue_bubbles_strata | make_complexes_figs) {
  if (SRVY == "GOA") {
    # MAY NEED TO ADD THIS BACK IN
    # reg_dat_goa <- akgfmaps::get_base_layers(
    #   select.region = "goa",
    #   set.crs = "EPSG:3338"
    # )
    reg_data$survey.area <- reg_data$survey.area |>
      dplyr::mutate(
        SRVY = "GOA",
        color = scales::alpha(colour = "grey80", 0.7),
        SURVEY = "Gulf of Alaska"
      )
  }
  
  if (SRVY == "AI") {
    reg_dat_ai <- akgfmaps::get_base_layers(
      select.region = "ai",
      set.crs = "EPSG:3338"
    )
    reg_dat_ai$survey.area <- reg_dat_ai$survey.area |>
      dplyr::mutate(
        SRVY = "AI",
        color = scales::alpha(colour = "grey80", 0.7),
        SURVEY = "Aleutian Islands"
      )
    reg_data <- reg_dat_ai
  }
}


# Themes ------------------------------------------------------

bubbletheme <- theme(
  panel.background = element_rect(
    fill = "white",
    colour = NA
  ),
  panel.border = element_rect(
    fill = NA,
    colour = "grey20"
  ),
  axis.text = element_text(size = 9),
  strip.background = element_blank(),
  strip.text = element_text(size = 10, face = "bold"),
  legend.text = element_text(size = 8),
  legend.background = element_rect(
    colour = "transparent",
    fill = "transparent"
  ),
  legend.key = element_rect(
    colour = "transparent",
    fill = "transparent"
  ),
  legend.box = "horizontal",
  axis.title.x = element_blank(),
  axis.title.y = element_blank(),
  plot.title = element_text(
    size = 10
  ),
  plot.subtitle = element_text(
    size = 11
  ),
  legend.title = element_text(size = 10)
)


linetheme <- theme_bw(base_size = 16)


bartheme <- ggpubr::theme_classic2(base_size = 14) +
  theme(strip.background = element_blank())


# Palettes ----------------------------------------------------------------

if (SRVY == "AI") {
  stratumpal <- lengthen_pal(
    shortpal = RColorBrewer::brewer.pal(n = 9, name = "PuBu"), #PNWColors::pnw_palette(name = "Winter",n = 8), 
    x = 1:nstrata
  ) |>
    colorspace::lighten(amount = 0.3, space = "HCL")
} else {
  stratumpal <- lengthen_pal(
    shortpal = RColorBrewer::brewer.pal(n = 9, name = "PuBu"), #PNWColors::pnw_palette(name = "Winter",n = 8),
    x = 1:nstrata
  )
}

# Palette for lines
linecolor <- RColorBrewer::brewer.pal(n = 9, name = "Blues")[9]
accentline <- RColorBrewer::brewer.pal(n = 9, name = "Blues")[8]

# Palette for depth shading for strata
#depthcolor <- RColorBrewer::brewer.pal(n = 9, name = "Blues")[1:4]
depthpal <- lengthen_pal(x = unique(stratum_lookup$DEPTH_MAX_M), shortpal = RColorBrewer::brewer.pal(n = 9, name = "Blues"))

# Palette for joy div plot
joypal <- lengthen_pal(shortpal = RColorBrewer::brewer.pal(n = 9, name = "Blues"), 
                       x = 1:nyears)

# Palette for species colors and fills
speciescolors <- lengthen_pal(
  shortpal = c("#dd7867", "#b83326", "#c8570d", "#edb144", "#8cc8bc", "#7da7ea", "#5773c0", "#1d4497"),
  x = 1:(nrow(report_species[which(!grepl("[A-Za-z]",report_species$species_code)),]) + 1)
)

################### CHUNKS ##################################################
# These can be run individually as needed. For example, if you want to modify all the biomass time series plots at once. If you know you already have satisfactory versions of all these plots, you don't need to re-run this code! The presentation knitting section will check if figs are available and will load them if not.
# ###########################################################################

# 1. Biomass index relative to LT mean ---------------------------------------

if (make_biomass_timeseries) {
  list_biomass_ts <- list()
  for (i in 1:nrow(report_species)) {
    sp <- report_species$species_code[i]
    name_bms <- report_species$spp_name_informal[i]

    dat <- biomass_total %>%
      filter(SPECIES_CODE == report_species$species_code[i])
    lta <- mean(dat$BIOMASS_MT)

    p1 <- dat %>%
      ggplot(aes(x = YEAR, y = BIOMASS_MT)) +
      geom_hline(yintercept = lta, color = accentline, lwd = 0.7, lty = 2) +
      geom_point(color = linecolor, size = 2) +
      geom_errorbar(aes(ymin = MIN_BIOMASS, ymax = MAX_BIOMASS), color = linecolor, linewidth = 0.9, width = 0.7) +
      ylab("Estimated total biomass (t)") +
      xlab("Year") +
      scale_y_continuous(labels = scales::label_comma()) +
      annotate(label = name_bms, geom ="label", x = Inf, y = Inf, hjust = 1, vjust = 1) +
      linetheme
    p1


    list_biomass_ts[[i]] <- p1
    png(
      filename = paste0(dir_out_figures, name_bms, "_", SRVY, "_", maxyr, "_biomass_ts.png"),
      width = 7, height = 7, units = "in", res = 150
    )
    print(p1)
    dev.off()
  }
  names(list_biomass_ts) <- as.character(report_species$species_code)
  save(list_biomass_ts, file = paste0(dir_out_figures, "list_biomass_ts.rdata"))
  print("Done with biomass time series plots.")
}

# 1b. Complexes: Biomass & CPUE maps------------------------------------------
# Use gapindex package with GROUP variable to plot biomass for the complexes through time. Complexes like REBS don't get generated automatically for tables in GAP_PRODUCTS so we have to make them by hand here. These species are assessed as a complex so assessment folks want to see Plan Team plots for the complex together. 
if(make_complexes_figs){
  print(paste("Generating figs for", unique(complex_lookup$complex)))
  
  # Long term averages
  lta <- biomass_df_complexes |>
    group_by(SPECIES_CODE) |>
    summarize(lta_biomass = mean(BIOMASS_MT)) |>
    ungroup()
  
  # Biomass time series
  biomass_ts_complexes <- list()

  for (i in 1:length(unique(complex_lookup$complex))) {
    complex <- unique(complex_lookup$complex)[i]
    p1 <- biomass_df_complexes |>
      dplyr::filter(SPECIES_CODE == complex) |>
      ggplot(aes(x = YEAR, y = BIOMASS_MT)) +
      geom_hline(
        yintercept = as.numeric(lta[which(lta$SPECIES_CODE == complex), "lta_biomass"]),
        color = accentline, lwd = 0.7, lty = 2
      ) +
      geom_point(color = linecolor, size = 2) +
      geom_errorbar(aes(ymin = MIN_BIOMASS, ymax = MAX_BIOMASS),
        color = linecolor, linewidth = 0.9, width = 0.7
      ) +
      ylab("Estimated total biomass (t)") +
      xlab("Year") +
      scale_y_continuous(labels = scales::label_comma()) +
      annotate(label = name_bms, geom ="label", x = Inf, y = Inf, hjust = 1, vjust = 1) +
      linetheme +
      annotate("text",
        x = 2000, y = 50000,
        label = "* Uses 2SD approximation for confidence intervals"
      )

    png(
      filename = paste0(dir_out_figures, complex, "_", SRVY, "_", maxyr, "_biomass_ts.png"),
      width = 7, height = 7, units = "in", res = 150
    )
    print(p1)
    dev.off()

    biomass_ts_complexes[[i]] <- p1
    names(biomass_ts_complexes)[i] <- complex
    }
  
  save(biomass_ts_complexes, file = paste0(dir_out_figures, "biomass_ts_complexes.rdata"))


  # CPUE maps
  list_cpue_bubbles_strata_complexes <- list()
  
  for (i in 1:length(unique(complex_lookup$complex))) {
    complex_code <- unique(complex_lookup$complex)[i]
    namebubble <- switch(complex_code, 
                         REBS = "Rougheye/blackspotted rockfish",
                         OROX = "Other rockfish",
                         OFLATS = "Other flatfish")
    
    thisyrshauldata <- cpue_table_complexes |>
      janitor::clean_names() |>
      #dplyr::mutate(cpue_kgha = cpue_kgkm2 / 100) |>
      dplyr::filter(year == maxyr & survey == SRVY & species_code == complex_code) |>
      st_as_sf(
        coords = c("longitude_dd_start", "latitude_dd_start"),
        crs = "EPSG:4326"
      ) %>%
      st_transform(crs = reg_data$crs)
    
    # MAPS
    p3a <- ggplot() +
      geom_sf(
        data = ai_east$survey.strata,
        mapping = aes(
          fill = factor(STRATUM),
          color = factor(STRATUM)
        )
      ) +
      scale_fill_manual(values = stratumpal, guide = "none") +
      scale_color_manual(values = stratumpal, guide = "none") +
      geom_sf(data = reg_data$akland) +
      geom_sf(data = filter(thisyrshauldata, cpue_kgkm2>0), 
              aes(size = cpue_kgkm2), alpha = 0.5) + 
      scale_size(limits = c(1, max(thisyrshauldata$cpue_kgkm2)), guide = "none") +
      geom_sf( # x's for places where cpue=0
        data = filter(thisyrshauldata, cpue_kgkm2 == 0),
        alpha = 1,
        color = "red",
        shape = 4,
        size = 1
      ) +
      coord_sf(
        xlim = ai_east$plot.boundary$x,
        ylim = ai_east$plot.boundary$y
      ) +
      scale_x_continuous(breaks = reg_data$lon.breaks) +
      scale_y_continuous(breaks = reg_data$lat.breaks) +
      labs(subtitle = "Eastern Aleutians \nand Southern Bering Sea") +
      bubbletheme
    
    p3b <- ggplot() +
      geom_sf(
        data = ai_central$survey.strata,
        mapping = aes(
          fill = factor(STRATUM),
          color = factor(STRATUM)
        )
      ) +
      scale_fill_manual(values = stratumpal, guide = "none") +
      scale_color_manual(values = stratumpal, guide = "none") +
      geom_sf(data = ai_central$akland) +
      geom_sf(data = filter(thisyrshauldata, cpue_kgkm2>0), 
              aes(size = cpue_kgkm2), alpha = 0.5) +
      scale_size(bquote("CPUE" ~ (kg / km^2)), 
                 limits = c(1, max(thisyrshauldata$cpue_kgkm2))) +
      geom_sf( # x's for places where cpue=0
        data = filter(thisyrshauldata, cpue_kgkm2 == 0),
        alpha = 1,
        color = "red",
        shape = 4,
        size = 1
      ) +
      coord_sf(
        xlim = ai_central$plot.boundary$x,
        ylim = ai_central$plot.boundary$y
      ) +
      scale_x_continuous(breaks = ai_central$lon.breaks) +
      scale_y_continuous(breaks = ai_central$lat.breaks) +
      labs(subtitle = "Central Aleutians") +
      bubbletheme +
      theme(legend.position = "left")
    
    p3c <- ggplot() +
      geom_sf(
        data = ai_west$survey.strata,
        mapping = aes(
          fill = factor(STRATUM),
          color = factor(STRATUM)
        )
      ) +
      scale_fill_manual(values = stratumpal, guide = "none") +
      scale_color_manual(values = stratumpal, guide = "none") +
      geom_sf(data = ai_west$akland) +
      geom_sf(data = filter(thisyrshauldata, cpue_kgkm2>0), 
              aes(size = cpue_kgkm2), alpha = 0.5) +
      scale_size(limits = c(1, max(thisyrshauldata$cpue_kgkm2)), guide = "none") +
      geom_sf( # x's for places where cpue=0
        data = filter(thisyrshauldata, cpue_kgkm2 == 0),
        alpha = 1,
        color = "red",
        shape = 4,
        size = 1
      ) +
      coord_sf(
        xlim = ai_east$plot.boundary$x,
        ylim = ai_east$plot.boundary$y
      ) +
      coord_sf(
        xlim = ai_west$plot.boundary$x,
        ylim = ai_west$plot.boundary$y
      ) +
      scale_x_continuous(breaks = ai_west$lon.breaks) +
      scale_y_continuous(breaks = ai_west$lat.breaks) +
      labs(subtitle = paste0(namebubble, " - Western Aleutians - ", YEAR)) +
      bubbletheme
    
    toprow <- cowplot::plot_grid(p3c, NULL, rel_widths = c(2, 1))
    bottomrow <- cowplot::plot_grid(NULL, p3a, rel_widths = c(1, 2))
    final_obj <- cowplot::plot_grid(toprow, p3b, bottomrow, ncol = 1)
    
    # ,out.width=9,out.height=8
    png(
      filename = paste0(dir_out_figures, complex_code, "_", maxyr, "_bubble.png"),
      width = 9, height = 8, units = "in", res = 200
    )
    print(final_obj)
    
    dev.off()
    
    list_cpue_bubbles_strata_complexes[[i]] <- final_obj
    names(list_cpue_bubbles_strata_complexes)[i] <- complex_code
  }#/all complexes cpue loop
  
  
} #/ all complexes figs

# 2. Catch composition -------------------------------------------------------
if (make_catch_comp) {
  head(biomass_total)
  biomass_total_filtered <- biomass_total |>
    dplyr::mutate(SPECIES_CODE = as.character(SPECIES_CODE)) |>
    left_join(report_species,
      by = c("SPECIES_CODE" = "species_code")
    ) %>%
    mutate(spp_name_informal = tidyr::replace_na(data = spp_name_informal, replace = "Other species"))

  biomass_total_filtered$spp_name_informal <- factor(biomass_total_filtered$spp_name_informal,
    levels = c(report_species$spp_name_informal, "Other species")
  )

  p2 <- biomass_total_filtered |>
    filter(!grepl(pattern = "[A-Za-z]", SPECIES_CODE)) |> 
    ggplot(aes(fill = spp_name_informal, y = BIOMASS_MT / 1e6, x = YEAR)) +
    geom_bar(position = "stack", stat = "identity") +
    scale_fill_manual("", values = speciescolors) +
    xlab("Year") +
    ylab(expression(paste("Total estimated \nbiomass (\u00D7", 10^6, " t)"))) +
    scale_y_continuous(expand = c(0, 0)) +
    bartheme +
    theme(legend.position = "bottom")

  png(
    filename = paste0(dir_out_figures, maxyr, "_biomass_catchcomp.png"),
    width = 12, height = 8, units = "in", res = 150
  )
  print(p2)
  dev.off()

  save(p2, file = paste0(dir_out_figures, "catch_comp.rdata"))
}

# 3b. CPUE bubble maps with strata --------------------------------------------

if (make_cpue_bubbles_strata) { # / end make stratum bubble figs
  
  # * * COMPLEXES ----------
  list_cpue_bubbles_strata_complexes <- list()
  
  for (i in 1:length(unique(complex_lookup$complex))) {
    # which complex to plot:
    complex_code <- unique(complex_lookup$complex)[i]
    
    # title for plot title:
    namebubble <- switch(complex_code,
                         REBS = "rougheye/blackspotted rockfish",
                         OROX = "other rockfish",
                         OFLATS = "other flatfish",
                         DEEPFLATS = "deep-water flatfish",
                         DSROX = "demersal shelf rockfish",
                         SWFLATS = "shallow-water flatfish",
                         SHARKS = "sharks",
                         SKATES = "skates",
                         THORNYHEADS = "thornyheads"
    )
    
    thisyrshauldata <- cpue_table_complexes |>
      janitor::clean_names() |>
      # dplyr::mutate(cpue_kgha = cpue_kgkm2 / 100) |>
      dplyr::filter(year == maxyr & survey == SRVY & species_code == complex_code) |>
      st_as_sf(
        coords = c("longitude_dd_start", "latitude_dd_start"),
        crs = "EPSG:4326"
      ) %>%
      st_transform(crs = reg_data$crs)
    
    # MAPS
    if (SRVY == "AI") {
      p3a <- ggplot() +
        geom_sf(
          data = ai_east$survey.strata,
          mapping = aes(
            fill = factor(DEPTH_MAX_M),
            color = factor(DEPTH_MAX_M)
          )
        ) +
        scale_fill_manual("Maximum \nstratum depth (m)",
                          values = depthpal, guide = "legend"
        ) +
        scale_color_manual(values = depthpal, guide = "none") +
        geom_sf(data = reg_data$akland) +
        geom_sf( # x's for places where cpue=0
          data = filter(thisyrshauldata, cpue_kgkm2 == 0),
          alpha = 1,
          color = "darkred",
          shape = 4,
          size = 1,
          stroke = 1
        ) +
        geom_sf(
          data = filter(thisyrshauldata, cpue_kgkm2 > 0),
          aes(size = cpue_kgkm2), alpha = 0.7,
          color = "black"
        ) +
        scale_size(bquote("CPUE" ~ (kg / km^2)),
                   limits = c(1, max(thisyrshauldata$cpue_kgkm2)),
                   guide = "legend"
        ) +
        coord_sf(
          xlim = ai_east$plot.boundary$x,
          ylim = ai_east$plot.boundary$y
        ) +
        scale_x_continuous(breaks = reg_data$lon.breaks) +
        scale_y_continuous(breaks = reg_data$lat.breaks) +
        labs(subtitle = "Eastern Aleutians \nand Southern Bering Sea") +
        bubbletheme +
        theme(legend.position = "left")
      
      p3b <- ggplot() +
        geom_sf(
          data = ai_central$survey.strata,
          mapping = aes(
            fill = factor(DEPTH_MAX_M),
            color = factor(DEPTH_MAX_M)
          )
        ) +
        scale_fill_manual(values = depthpal, guide = "none") +
        scale_color_manual(values = depthpal, guide = "none") +
        geom_sf(data = ai_central$akland) +
        geom_sf( # x's for places where cpue=0
          data = filter(thisyrshauldata, cpue_kgkm2 == 0),
          alpha = 1,
          color = "darkred",
          shape = 4,
          size = 1,
          stroke = 1
        ) +
        geom_sf(
          data = filter(thisyrshauldata, cpue_kgkm2 > 0),
          aes(size = cpue_kgkm2), alpha = 0.7,
          color = "black"
        ) +
        scale_size(
          limits = c(1, max(thisyrshauldata$cpue_kgkm2))
        ) +
        coord_sf(
          xlim = ai_central$plot.boundary$x,
          ylim = ai_central$plot.boundary$y
        ) +
        scale_x_continuous(breaks = ai_central$lon.breaks) +
        scale_y_continuous(breaks = ai_central$lat.breaks) +
        labs(subtitle = "Central Aleutians") +
        bubbletheme
      
      p3c <- ggplot() +
        geom_sf(
          data = ai_west$survey.strata,
          mapping = aes(
            fill = factor(DEPTH_MAX_M),
            color = factor(DEPTH_MAX_M)
          )
        ) +
        scale_fill_manual(values = depthpal, guide = "none") +
        scale_color_manual(values = depthpal, guide = "none") +
        geom_sf(data = ai_west$akland) +
        scale_size(limits = c(1, max(thisyrshauldata$cpue_kgkm2)), guide = "none") +
        geom_sf( # x's for places where cpue=0
          data = filter(thisyrshauldata, cpue_kgkm2 == 0),
          alpha = 1,
          color = "darkred", # d95f0e
          shape = 4,
          size = 1,
          stroke = 1
        ) +
        geom_sf(
          data = filter(thisyrshauldata, cpue_kgkm2 > 0),
          aes(size = cpue_kgkm2), alpha = 0.7, color = "black"
        ) +
        coord_sf(
          xlim = ai_east$plot.boundary$x,
          ylim = ai_east$plot.boundary$y
        ) +
        coord_sf(
          xlim = ai_west$plot.boundary$x,
          ylim = ai_west$plot.boundary$y
        ) +
        scale_x_continuous(breaks = ai_west$lon.breaks) +
        scale_y_continuous(breaks = ai_west$lat.breaks) +
        labs(subtitle = paste0(namebubble, " - Western Aleutians - ", YEAR)) +
        bubbletheme
      
      toprow <- cowplot::plot_grid(p3c, NULL, rel_widths = c(2, 1))
      bottomrow <- cowplot::plot_grid(p3a, rel_widths = c(1, 2))
      final_obj <- cowplot::plot_grid(toprow, p3b, bottomrow, ncol = 1)
    } else { # /GOA stratum bubble maps
      final_obj <- ggplot() +
        geom_sf(
          data = reg_data$survey.strata,
          mapping = aes(
            fill = factor(DEPTH_MAX_M),
            color = factor(DEPTH_MAX_M)
          )
        ) +
        scale_fill_manual(values = depthpal, guide = "none") +
        scale_color_manual(values = depthpal, guide = "none") +
        geom_sf(data = reg_data$akland) +
        geom_sf( # x's for places where cpue=0
          data = filter(thisyrshauldata, cpue_kgkm2 == 0),
          alpha = 1,
          color = "darkred",
          shape = 4,
          size = 1,
          stroke = 1
        ) +
        geom_sf(
          data = filter(thisyrshauldata, cpue_kgkm2 > 0),
          aes(size = cpue_kgkm2), alpha = 0.7, color = "black"
        ) +
        scale_size(limits = c(1, max(thisyrshauldata$cpue_kgkm2)), guide = "none") +
        coord_sf(
          xlim = reg_data$plot.boundary$x,
          ylim = reg_data$plot.boundary$y
        ) +
        scale_x_continuous(breaks = reg_data$lon.breaks) +
        scale_y_continuous(breaks = reg_data$lat.breaks) +
        bubbletheme
    } # /GOA stratum bubble maps for complexes
    
    # ,out.width=9,out.height=8
    png(
      filename = paste0(dir_out_figures, maxyr, "_", complex_code, "_bubble.png"),
      width = 9, height = 8, units = "in", res = 200
    )
    print(final_obj)
    
    dev.off()
    
    list_cpue_bubbles_strata_complexes[[i]] <- final_obj
    names(list_cpue_bubbles_strata_complexes)[i] <- complex_code
  } # /all complexes cpue loop
  
  #  * * SPECIES ----------
  list_cpue_bubbles_strata_species <- list()
  
  bubble_index <- which(!report_species$species_code %in% c(
    "OROX", "REBS", "OFLATS",
    "DEEPFLATS", "DSROX", "NRSSRS",
    "SWFLATS", "SHARKS", "SKATES",
    "THORNYHEADS"
  ))
  
  for (i in 1:length(bubble_index)) {
    spbubble <- report_species$species_code[bubble_index[i]]
    namebubble <- report_species$spp_name_informal[bubble_index[i]]
    
    thisyrshauldata <- cpue_raw |>
      # dplyr::mutate(cpue_kgha = cpue_kgkm2 / 100) |>
      dplyr::filter(year == maxyr & survey == SRVY & species_code == spbubble) |>
      st_as_sf(
        coords = c("longitude_dd_start", "latitude_dd_start"),
        crs = "EPSG:4326"
      ) |>
      st_transform(crs = reg_data$crs)
    
    # MAPS
    if (SRVY == "AI") {
      p3a <- ggplot() +
        geom_sf(
          data = ai_east$survey.strata,
          mapping = aes(
            fill = factor(DEPTH_MAX_M),
            color = factor(DEPTH_MAX_M)
          )
        ) +
        scale_fill_manual("Maximum \nstratum depth (m)",
                          values = depthpal,
                          guide = "legend"
        ) +
        scale_color_manual(values = depthpal, guide = "none") +
        geom_sf(data = reg_data$akland) +
        geom_sf( # x's for places where cpue=0
          data = filter(thisyrshauldata, cpue_kgkm2 == 0),
          alpha = 1,
          color = "darkred",
          shape = 4,
          size = 1,
          stroke = 1
        ) +
        geom_sf(
          data = filter(thisyrshauldata, cpue_kgkm2 > 0),
          aes(size = cpue_kgkm2), alpha = 0.7, color = "black"
        ) +
        scale_size(bquote("CPUE" ~ (kg / km^2)),
                   limits = c(1, max(thisyrshauldata$cpue_kgkm2)),
                   guide = "legend"
        ) +
        coord_sf(
          xlim = ai_east$plot.boundary$x,
          ylim = ai_east$plot.boundary$y
        ) +
        scale_x_continuous(breaks = reg_data$lon.breaks) +
        scale_y_continuous(breaks = reg_data$lat.breaks) +
        labs(subtitle = "Eastern Aleutians \nand Southern Bering Sea") +
        bubbletheme +
        theme(legend.position = "left")
      
      p3b <- ggplot() +
        geom_sf(
          data = ai_central$survey.strata,
          mapping = aes(
            fill = factor(DEPTH_MAX_M),
            color = factor(DEPTH_MAX_M)
          )
        ) +
        scale_fill_manual(values = depthpal, guide = "none") +
        scale_color_manual(values = depthpal, guide = "none") +
        geom_sf(data = ai_central$akland) +
        geom_sf( # x's for places where cpue=0
          data = filter(thisyrshauldata, cpue_kgkm2 == 0),
          alpha = 1,
          color = "darkred",
          shape = 4,
          size = 1,
          stroke = 1
        ) +
        geom_sf(
          data = filter(thisyrshauldata, cpue_kgkm2 > 0),
          aes(size = cpue_kgkm2), alpha = 0.7, color = "black"
        ) +
        scale_size(
          limits = c(1, max(thisyrshauldata$cpue_kgkm2))
        ) +
        coord_sf(
          xlim = ai_central$plot.boundary$x,
          ylim = ai_central$plot.boundary$y
        ) +
        scale_x_continuous(breaks = ai_central$lon.breaks) +
        scale_y_continuous(breaks = ai_central$lat.breaks) +
        labs(subtitle = "Central Aleutians") +
        bubbletheme
      
      p3c <- ggplot() +
        geom_sf(
          data = ai_west$survey.strata,
          mapping = aes(
            fill = factor(DEPTH_MAX_M),
            color = factor(DEPTH_MAX_M)
          )
        ) +
        scale_fill_manual(values = depthpal, guide = "none") +
        scale_color_manual(values = depthpal, guide = "none") +
        geom_sf(data = ai_west$akland) +
        geom_sf( # x's for places where cpue=0
          data = filter(thisyrshauldata, cpue_kgkm2 == 0),
          alpha = 1,
          color = "darkred",
          shape = 4,
          size = 1,
          stroke = 1
        ) +
        geom_sf(
          data = filter(thisyrshauldata, cpue_kgkm2 > 0),
          aes(size = cpue_kgkm2), alpha = 0.7, color = "black"
        ) +
        scale_size(limits = c(1, max(thisyrshauldata$cpue_kgkm2)), guide = "none") +
        coord_sf(
          xlim = ai_east$plot.boundary$x,
          ylim = ai_east$plot.boundary$y
        ) +
        coord_sf(
          xlim = ai_west$plot.boundary$x,
          ylim = ai_west$plot.boundary$y
        ) +
        scale_x_continuous(breaks = ai_west$lon.breaks) +
        scale_y_continuous(breaks = ai_west$lat.breaks) +
        labs(subtitle = paste0(namebubble, " - Western Aleutians - ", YEAR)) +
        bubbletheme
      
      toprow <- cowplot::plot_grid(p3c, NULL, rel_widths = c(2, 1))
      bottomrow <- cowplot::plot_grid(p3a, rel_widths = c(1, 2))
      final_obj <- cowplot::plot_grid(toprow, p3b, bottomrow, ncol = 1)
    } else {
      final_obj <- ggplot() +
        geom_sf(
          data = reg_data$survey.strata,
          mapping = aes(
            fill = factor(DEPTH_MAX_M),
            color = factor(DEPTH_MAX_M)
          )
        ) +
        scale_fill_manual(values = depthpal, guide = "none") +
        scale_color_manual(values = depthpal, guide = "none") +
        geom_sf(data = reg_data$akland) +
        geom_sf(
          data = filter(thisyrshauldata, cpue_kgkm2 > 0),
          aes(size = cpue_kgkm2), alpha = 0.7, color = "black"
        ) +
        scale_size(limits = c(1, max(thisyrshauldata$cpue_kgkm2)), guide = "none") +
        geom_sf( # x's for places where cpue=0
          data = filter(thisyrshauldata, cpue_kgkm2 == 0),
          alpha = 1,
          color = "darkred",
          shape = 4,
          size = 1,
          stroke = 1
        ) +
        coord_sf(
          xlim = reg_data$plot.boundary$x,
          ylim = reg_data$plot.boundary$y
        ) +
        scale_x_continuous(breaks = reg_data$lon.breaks) +
        scale_y_continuous(breaks = reg_data$lat.breaks) +
        bubbletheme
    } # / end bubble stratum maps for individual species
    # ,out.width=9,out.height=8
    png(
      filename = paste0(dir_out_figures, maxyr, "_", namebubble, "_bubble.png"),
      width = 9, height = 8, units = "in", res = 200
    )
    print(final_obj)
    
    dev.off()
    
    list_cpue_bubbles_strata_species[[i]] <- final_obj # save fig to list
  } # /end species loop
  names(list_cpue_bubbles_strata_species) <- report_species$species_code[bubble_index]
  
  
  list_cpue_bubbles_strata <- c(list_cpue_bubbles_strata_species, list_cpue_bubbles_strata_complexes)
  
  save(list_cpue_bubbles_strata, file = paste0(dir_out_figures, "list_cpue_bubbles_strata.rdata"))
  
  # Remove intermediary fig lists
  rm(list = c(
    "list_cpue_bubbles_strata_species",
    "list_cpue_bubbles_strata_complexes"
  ))
  
  print("Done with CPUE bubble maps showing stratum areas.")
}

# 4. CPUE IDW plots -------------------------------------------------------

# The function used to generate this CPUE map is Emily's "plot_idw_xbyx()"
# THIS SHOULD ONLY BE USED FOR THE GOA - in the AI, the area is too narrow for a raster map of CPUE and we should instead be using the bubble plots of CPUE or Jim Ianelli's bar plot thing.

if (make_cpue_idw) {
  list_idw_cpue <- list()
  for (s in 1:nrow(report_species)) { # 1:nrow(report_species)
    sp <- report_species$species_code[s]
    namebubble <- report_species$spp_name_informal[s]

    dat2plot <- cpue_raw %>%
      filter(survey == SRVY & species_code == sp & year == maxyr)
    cpue_res <- 0.05 # 0.05 will take less time, 0.01 is best looking but takes ~10 mins per plot.

    fig <- plot_idw_xbyx(
      yrs = maxyr,
      dat = dat2plot,
      lat = "start_latitude",
      lon = "start_longitude",
      var = "cpue_kgkm2",
      year = "year",
      key.title = "CPUE (kg/km2)", # report_species$spp_name_informal[s]
      grid = "extrapolation.grid",
      extrap.box = c(xmin = -180, xmax = -135, ymin = 52, ymax = 62),
      grid.cell = c(cpue_res, cpue_res),
      row0 = 1,
      region = "goa"
    ) +
      theme(axis.text = element_text(size = 11))

    png(
      filename = paste0(dir_out_figures, namebubble, "_", maxyr, "_cpue_idw.png"),
      width = 11, height = 10, units = "in", res = 200
    )
    print(fig)

    dev.off()

    list_idw_cpue[[s]] <- fig # save fig to list
    cat("done with", report_species$spp_name_informal[s], " (", s, "/", nrow(report_species), ")\n")
  }

  names(list_idw_cpue) <- report_species$species_code
  save(list_idw_cpue, file = paste0(dir_out_figures, "list_idw_cpue.rdata"))
}


# 5. CPUE Ianelli plots --------------------------------------------------
# Same base layer as the bubble plot but with bars for each haul


if (make_cpue_ianelli) {
  ianelli_style <- TRUE
  key.title <- ""
  yrs <- unique(cpue_raw$year[cpue_raw$year>minyr]) ##
  row0 <- 2 # default
  legendtitle <- bquote(CPUE(kg / ha)) # inside fn


  list_ianelli <- list()
  for (i in 1:nrow(report_species)) {
    for (y in 1:length(yrs)) {
      # i = 8
      thisyrshauldata <- cpue_raw |>
        #dplyr::mutate(cpue_kgha = cpue_kgkm2 / 100) |>
        dplyr::filter(year %in% yrs[y] &
          survey == SRVY &
          species_code == report_species$species_code[i]) |>
        sf::st_as_sf(
          coords = c("longitude_dd_start", "latitude_dd_start"),
          crs = "EPSG:4326"
        ) |>
        sf::st_transform(crs = reg_data$crs)

      if (nrow(thisyrshauldata) == 0) {
        print(paste0("No data for this year; skipping ", yrs[y]))
        next
      }

      f1 <- ggplot() +
        geom_sf(
          data = reg_data$akland,
          color = NA,
          fill = "grey40"
        ) +
        geom_sf( # x's for places where we sampled but didn't catch any of that species
          data = dplyr::filter(thisyrshauldata, cpue_kgkm2 == 0),
          alpha = 0.8,
          color = "red",
          shape = 4,
          size = 1
        )

      f2 <- f1 +
        geom_sf(
          data = reg_data$survey.area,
          mapping = aes(color = SURVEY),
          fill = NA,
          shape = NA,
          size = .25,
          show.legend = FALSE
        ) +
        scale_color_manual(
          name = key.title,
          values = reg_data$survey.area$color,
          breaks = rev(reg_data$survey.area$SURVEY),
          labels = rev((reg_data$survey.area$SRVY))
        )

      f3 <- f2 +
        ggplot2::scale_y_continuous(
          name = "", # "Latitude",
          limits = reg_data$plot.boundary$y,
          breaks = reg_data$lat.breaks
        ) +
        ggplot2::scale_x_continuous(
          name = "", # "Longitude",
          limits = reg_data$plot.boundary$x,
          breaks = reg_data$lon.breaks
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

      if (ianelli_style) {
        pos_cpue <- thisyrshauldata # filter(thisyrshauldata, cpue_kgkm2>0)

        # get coordinates into dataframe from so you can use
        coords_df <- data.frame(st_coordinates(pos_cpue[, "geometry"]))
        pos_cpue2 <- bind_cols(pos_cpue, coords_df)
        pos_cpue2$bar_ymax <- pos_cpue2$Y + pos_cpue2$cpue_kgkm2 * scale

        pos_cpue3 <- pos_cpue2 |> 
          dplyr::filter(bar_ymax<reg_data$plot.boundary$y[2])    
        
        scale <- 1 # 10
        width <- 9000

        f5 <- f4 +
          geom_rect(
            data = pos_cpue3, aes(
              xmin = X - width / 2,
              xmax = X + width / 2,
              ymin = Y,
              ymax = bar_ymax
            ),
            fill = "#797EF6AA"
          ) # "#3E356BFF"

        # Plot CPUE values that go off the chart with an arrow at the top
        veryhighcpue <- pos_cpue2 |>
          dplyr::filter(bar_ymax>reg_data$plot.boundary$y[2]) |>
          mutate(arrow_ymax = reg_data$plot.boundary$y[2]*0.99)
        # print(veryhighcpue)
        if (nrow(veryhighcpue) > 0) {
          f5b <- f5 + geom_segment(
            data = veryhighcpue,
            aes(x = X, y = Y, yend = arrow_ymax),
            arrow = arrow(length = unit(0.2, "cm")),
            color = "#797EF6"
          )
          # f5b <- f5 + geom_point(
          #   data = veryhighcpue,
          #   aes(x = X, y = Y),
          #   color = "orange"
          # )
        }
      } else {
        f5b <- f4
      }

      # Add theme and background and stuff
      figure <- f5b +
        theme( # set legend position and vertical arrangement
          panel.background = element_rect(
            fill = "white",
            colour = NA
          ),
          panel.border = element_rect(
            fill = NA,
            colour = "grey20"
          ),
          axis.text = element_text(size = ifelse(length(yrs) > 4 & row0 == 1, 10, 12)),
          strip.background = element_blank(),
          strip.text = element_text(size = 12, face = "bold"),
          legend.text = element_text(size = 12),
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
        labs(size = legendtitle) +
        ggtitle(paste(yrs[y]))

      png(
        filename = paste0(dir_out_figures, report_species$spp_name_informal[i], "_", yrs[y], "_cpue_ianelli.png"),
        width = 11, height = 8, units = "in", res = 200
      )
      print(figure)

      dev.off()
    } # /yr loop
    
    list_ianelli[[i]] <- figure
  } # /species loop
  names(list_ianelli) <- report_species$species_code
  save(list_ianelli, file = paste0(dir_out_figures, "list_ianelli.rdata"))
}




# 6. % changes in biomass since last survey ----------------------------

compare_tab <- biomass_total |>
  filter(YEAR %in% c(maxyr, compareyr) &
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
head(compare_tab)

compare_tab2 <- compare_tab |>
  dplyr::mutate_at('SPECIES_CODE',as.character) |>
  left_join(report_species, by = c("SPECIES_CODE" = "species_code")) |>
  dplyr::arrange(-yr_2025)

compare_tab_pres <- compare_tab2 |>
  dplyr::select(spp_name_informal, yr_2023, yr_2025, percent_change) |>
  dplyr::arrange(-yr_2025) |>
  dplyr::mutate(across(starts_with("yr_"), ~round(.x))) |>
  dplyr::rename('Biomass in 2025 (t)' = yr_2025,
                'Biomass in 2023 (t)' = yr_2023,
                'Percent change' = percent_change)

write.csv(compare_tab_pres, file = paste0(dir_out_tables, "compare_tab_pres.csv"))

# 7. Joy division plots - Length frequency -----------------------------

if (make_joy_division_length) {
  list_joy_length <- list()
  if (file.exists(paste0("data/", maxyr, "_", SRVY, "_report_pseudolengths.csv"))) {
    report_pseudolengths <- read.csv(paste0("data/", maxyr, "_", SRVY, "_report_pseudolengths.csv"))
  } else {
    cat("Pseudolength file not found. Sourcing data prep file (sorry this will take a while... \n")
    source("R/06_prep_data.R")
  }

  # This is repeated; deal with it later
  L0 <- read.csv(here::here("data/local_racebase/length.csv"))
  L <- L0 |>
    dplyr::mutate(YEAR = as.numeric(gsub("(^\\d{4}).*", "\\1", CRUISE)))
  length_maxyr <- filter(L, YEAR == maxyr & REGION == SRVY)
  
  L2 <- L |> # L is the big length table from RACEBASE
    mutate(YEAR = stringr::str_extract(CRUISE, "^\\d{4}")) %>%
    filter(REGION == SRVY) # want to keep all years for this fig
  
  L3 <- L2 |>
    left_join(haul2, by = c("HAULJOIN", "YEAR", "CRUISEJOIN", "VESSEL", "CRUISE", "HAUL")) |>
    dplyr::select(VESSEL, YEAR, LENGTH, FREQUENCY, SEX, GEAR_DEPTH, STRATUM, SPECIES_CODE) |>
    left_join(region_lu, by = "STRATUM") |>
    mutate(Sex = case_when(
      SEX == 1 ~ "Male",
      SEX == 2 ~ "Female",
      SEX == 3 ~ "Unsexed"
    )) |>
    dplyr::select(-SEX, -MIN_DEPTH, -MAX_DEPTH)
  
  # get samples for individual species
  species_sample_sizes <- L3 |>
    dplyr::filter(grepl("[0-9]", SPECIES_CODE)) |># filter to not complexes
    dplyr::filter(SPECIES_CODE %in% (report_species$species_code)) |> # filter to report species
    dplyr::filter(YEAR >= minyr) |>
    dplyr::group_by(YEAR, SPECIES_CODE, Sex) |>
    dplyr::summarize(n = sum(FREQUENCY)) |>
    ungroup() |>
    mutate(YEAR = as.integer(YEAR), 
           SPECIES_CODE = as.character(SPECIES_CODE))
  
  # get sample sizes for complexes
  complex_sample_sizes <- L3 |>
    dplyr::filter(SPECIES_CODE %in% complex_lookup$species_code) |>
    dplyr::filter(YEAR >= minyr) |>
    dplyr::mutate(SPECIES_CODE = case_when(SPECIES_CODE %in% complex_lookup$species_code[which(complex_lookup$complex=="OROX")] ~ "OROX",
                                           SPECIES_CODE %in% complex_lookup$species_code[which(complex_lookup$complex=="REBS")] ~ "REBS",
                                           SPECIES_CODE %in% complex_lookup$species_code[which(complex_lookup$complex=="OFLATS")] ~ "OFLATS",
                                           .default = as.character(SPECIES_CODE))) |>
    dplyr::mutate(SPECIES_CODE = as.character(SPECIES_CODE)) |>
    dplyr::group_by(YEAR, SPECIES_CODE, Sex) |>
    dplyr::summarize(n = sum(FREQUENCY)) |>
    ungroup() |>
    mutate(YEAR = as.integer(YEAR))

  sample_sizes <- bind_rows(species_sample_sizes, complex_sample_sizes)
  # Loop thru species
  for (i in 1:nrow(report_species)) {
    
    len2plot <- report_pseudolengths %>%
      filter(SPECIES_CODE == report_species$species_code[i])

    # SSTH or darkfin sculpin only show Unsexed; all other spps show only sexed lengths
    if (report_species$species_code[i] %in% c(30020, 21341)) {
      len2plot <- len2plot 
    }else{
      len2plot <- len2plot %>%
        filter(Sex != "Unsexed")
    }

    # Save median lengths by year and sex for species i
    medlines_sp <- report_pseudolengths %>%
      filter(SPECIES_CODE == report_species$species_code[i]) %>%
      group_by(YEAR, Sex) %>%
      dplyr::summarize(medlength = median(LENGTH, na.rm = T)) %>%
      ungroup()
    
    # Lengths to plot
    len2plot2 <- len2plot %>%
      left_join(sample_sizes %>%
        filter(SPECIES_CODE == report_species$species_code[i]))
    
    yrbreaks <- unique(len2plot2$YEAR)
    lengthlimits <- range(len2plot2$LENGTH)

    n_labels <- len2plot2 %>%
      distinct(YEAR, Sex, .keep_all = TRUE)

    joyplot <- len2plot2 %>%
      ggplot(mapping = aes(x = LENGTH, y = YEAR, group = YEAR, fill = after_stat(x))) +
      ggridges::geom_density_ridges_gradient( #
        bandwidth = 5,
        rel_min_height = 0,
        quantile_lines = T,
        quantile_fun = median,
        vline_color = "white",
        vline_width = 0.6,
        vline_linetype = "dotted"
        # "A1"
      ) +
      scale_y_reverse(breaks = yrbreaks, expand = c(0, 0)) +
      scale_x_continuous(expand = c(0, 0), limits = lengthlimits) + #
      scale_linetype_manual(values = c("solid", "dashed")) +
      facet_grid(~Sex) +
      xlab("Length (mm)") +
      ylab("Year") +
      theme_ridges(font_size = 10) +
      scale_fill_gradientn(colours = joypal) +
      geom_label(
        data = n_labels,
        mapping = aes(label = paste0("n = ", n), x = Inf),
        fill = "white", label.size = NA,
        nudge_x = 0,
        nudge_y = 1,
        hjust = "inward", size = 3
      ) +
      theme(
        strip.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none",
        axis.title.x = element_text(hjust = 0.5),
        axis.title.y = element_text(hjust = 0.5),
        panel.spacing.x = unit(4, "mm"),
        axis.line.x = element_line(lineend = "square")
      )


    # lookup table is referenced below

    # did the species used to be ID'ed differently somehow? E.g., arrowtooth and kams
    # if (report_species$species_code[i] %in% complex_lookup$polycode) {
    #   # Add label to plot of the species so ppl can compare it with the combined one
    #   joyplot <- joyplot + labs(title = paste(report_species$spp_name_informal[i]))
    # 
    #   # Make a title for the combined plot (single species + combined congeners)
    #   plot_title <- complex_lookup$complex_name[which(complex_lookup$polycode == report_species$species_code[i])]
    # 
    #   complex_sp <- complex_lookup$complex[which(complex_lookup$polycode == report_species$species_code[i])]
    #   polycode_vec <- complex_lookup$polycode[which(complex_lookup$complex == complex_sp)]
    #   star_yr <- switch(complex_sp,
    #     nrs_srs = 1996,
    #     kam_atf = 1992,
    #     rebs = 2006
    #   )
    #   yrlabels <- yrbreaks
    #   yrlabels[which(yrlabels < star_yr)] <- paste0(yrlabels[which(yrlabels < star_yr)], "*")
    #   yrlabels <- as.character(yrlabels)
    # 
    #   medlines_sp <- report_pseudolengths %>%
    #     filter(SPECIES_CODE %in% polycode_vec) %>%
    #     group_by(YEAR, Sex) %>%
    #     dplyr::summarize(medlength = median(LENGTH, na.rm = T)) %>%
    #     ungroup()
    # 
    # 
    #   sample_sizes_comb <- sample_sizes %>%
    #     filter(SPECIES_CODE %in% polycode_vec) %>%
    #     group_by(YEAR, Sex) %>%
    #     dplyr::summarize(n = sum(n)) %>%
    #     ungroup()
    # 
    #   len2plot_comb <- report_pseudolengths %>%
    #     filter(SPECIES_CODE %in% polycode_vec) %>%
    #     filter(Sex != "Unsexed") %>%
    #     left_join(sample_sizes_comb)
    # 
    #   testlabdf_comb <- len2plot_comb %>%
    #     distinct(YEAR, Sex, .keep_all = TRUE)
    # 
    #   joyplot2 <- len2plot_comb %>%
    #     ggplot(
    #       mapping = aes(x = LENGTH, y = YEAR, group = YEAR),
    #       fill = "grey"
    #     ) +
    #     ggridges::geom_density_ridges_gradient(
    #       bandwidth = 5,
    #       rel_min_height = 0,
    #       quantile_lines = T,
    #       quantile_fun = median,
    #       vline_color = "white",
    #       vline_width = 0.6,
    #       vline_linetype = "dotted" # "A1"
    #     ) +
    #     scale_y_reverse(breaks = yrbreaks, labels = yrlabels, expand = c(0, 0)) +
    #     scale_linetype_manual(values = c("solid", "dashed")) +
    #     geom_label(
    #       data = testlabdf_comb,
    #       mapping = aes(label = paste0("n = ", n), x = Inf),
    #       fill = "white", label.size = NA,
    #       nudge_x = -100, nudge_y = 1, hjust = "inward", size = 3
    #     ) +
    #     facet_grid(~Sex) +
    #     xlab("Length (mm)") +
    #     ylab("Year") +
    #     theme_ridges(font_size = 7) +
    #     labs(title = plot_title) +
    #     theme(
    #       strip.background = element_blank(),
    #       panel.grid.major = element_blank(),
    #       panel.grid.minor = element_blank(),
    #       legend.position = "none",
    #       axis.title.x = element_text(hjust = 0.5),
    #       axis.title.y = element_text(hjust = 0.5),
    #       panel.spacing.x = unit(4, "mm"),
    #       axis.line.x = element_line(lineend = "square")
    #     )
    # 
    #   joyplot <- joyplot + joyplot2
    # }


    png(filename = paste0(
      dir_out_figures,
      report_species$spp_name_informal[i], "_", maxyr, "_joyfreqhist.png"
    ), width = 7, height = 5, units = "in", res = 200)
    print(joyplot)
    dev.off()

    list_joy_length[[i]] <- joyplot
  }
  names(list_joy_length) <- report_species$species_code

  save(list_joy_length, file = paste0(dir_out_figures, "list_joy_length.rdata"))
  print("Done with joy division plots for length comp.")
}


# 8. Surface and bottom temperatures --------------------------------------
if (make_temp_plot) {
  list_temperature <- list()

  sstdat <- haul %>%
    mutate(YEAR = stringr::str_extract(CRUISE, "^\\d{4}")) %>%
    filter(YEAR >= 1990 & REGION == SRVY & YEAR != 1997) %>%
    group_by(YEAR) %>%
    dplyr::summarize(
      bottom = mean(GEAR_TEMPERATURE, na.rm = TRUE),
      surface = mean(SURFACE_TEMPERATURE, na.rm = TRUE)
    ) %>%
    ungroup() %>%
    as.data.frame() %>%
    mutate(YEAR = as.numeric(YEAR))

  if (SRVY == "GOA") {
    sstdat <- sstdat %>% filter(YEAR != 2001) # They didn't finish the GOA survey in 2001
  }

  sst_summary <- sstdat %>%
    mutate(
      bottom_stz = bottom - mean(bottom, na.rm = T),
      surface_stz = surface - mean(surface, na.rm = T)
    ) %>%
    pivot_longer(cols = bottom:surface_stz)

  plotdat <- haul %>%
    mutate(YEAR = as.numeric(stringr::str_extract(CRUISE, "^\\d{4}"))) %>%
    filter(REGION == SRVY & YEAR != 1997 & YEAR >= 1990) %>%
    filter(CRUISE != 201402) %>% # remove study from Makushin bay in 2014 (contains a zero BT)
    filter(HAULJOIN != -17737) # Filter out the situation with BT=0 in 2018

  howmanyboats <- haul |>
    dplyr::filter(REGION == SRVY) |>
    mutate(YEAR = as.numeric(gsub("(^\\d{4}).*", "\\1", CRUISE))) |>
    dplyr::group_by(YEAR) |>
    dplyr::distinct(VESSEL) |>
    dplyr::ungroup() |>
    dplyr::group_by(YEAR) |>
    dplyr::summarize(nboats = length(VESSEL)) %>%
    dplyr::ungroup() |>
    filter(YEAR >= 1990 & YEAR != 1998) |> # filter to fit the same years as above
    dplyr::mutate(annotation_star = case_when(
      nboats == 1 ~ "",
      nboats == 2 ~ "",
      nboats == 3 ~ "*",
      nboats == 4 ~ "*"
    )) |>
    dplyr::filter(YEAR %in% unique(plotdat$YEAR))




  bottom_temp_20yr <- plotdat |>
    filter(YEAR >= (maxyr - 20)) |>
    dplyr::summarize(mean(GEAR_TEMPERATURE, na.rm = T)) |>
    as.numeric()
  bottom_temp_10yr <- plotdat |>
    filter(YEAR >= (maxyr - 10)) |>
    dplyr::summarize(mean(GEAR_TEMPERATURE, na.rm = T)) |>
    as.numeric()
  bottom_temp_avgs <- data.frame(
    "Average" = c("10-year", "20-year"),
    "Value" = c(bottom_temp_20yr, bottom_temp_10yr),
    Start_year = c(maxyr - 10, maxyr - 20)
  )

  # ylims <- range(plotdat$GEAR_TEMPERATURE,na.rm=TRUE)

  bottom_temp_plot <- plotdat %>%
    ggplot(aes(y = GEAR_TEMPERATURE, x = YEAR)) +
    ggdist::stat_interval(linewidth = 3) +
    ggdist::stat_halfeye(
      fill = "tan", alpha = 0.5,
      interval_color = "grey27", point_color = "grey27"
    ) +
    rcartocolor::scale_color_carto_d("Quantile", palette = "Peach") +
    scale_fill_ramp_discrete(na.translate = FALSE) +
    labs(x = "Year", y = expression("Bottom temperature "(degree * C))) +
    scale_x_continuous(
      breaks = howmanyboats$YEAR, # add labels w nboats
      labels = paste0(
        howmanyboats$YEAR,
        howmanyboats$annotation_star
      )
    ) +
    geom_segment(data = bottom_temp_avgs, aes(
      y = Value, yend = Value,
      linetype = Average,
      x = Start_year, xend = maxyr
    )) +
    ylim(c(2, 7)) + # NEED TO CHANGE IN THE FUTURE
    theme_light(base_size = 14) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

  surface_temp_20yr <- plotdat |>
    filter(YEAR >= (maxyr - 20)) |>
    dplyr::summarize(mean(SURFACE_TEMPERATURE, na.rm = T)) |>
    as.numeric()
  surface_temp_10yr <- plotdat |>
    filter(YEAR >= (maxyr - 10)) |>
    dplyr::summarize(mean(SURFACE_TEMPERATURE, na.rm = T)) |>
    as.numeric()
  surface_temp_avgs <- data.frame(
    "Average" = c("10-year", "20-year"),
    "Value" = c(surface_temp_20yr, surface_temp_10yr),
    "Start_year" = c(maxyr - 10, maxyr - 20)
  )

  surface_temp_plot <- plotdat %>%
    ggplot(aes(y = SURFACE_TEMPERATURE, x = YEAR)) +
    ggdist::stat_interval(linewidth = 3) +
    ggdist::stat_halfeye(
      fill = "tan", alpha = 0.5,
      interval_color = "grey27", point_color = "grey27"
    ) +
    rcartocolor::scale_color_carto_d("Quantile", palette = "Peach") +
    scale_fill_ramp_discrete(na.translate = FALSE) +
    labs(x = "Year", y = expression("Surface temperature "(degree * C))) +
    geom_segment(data = surface_temp_avgs, aes(
      y = Value, yend = Value,
      linetype = Average,
      x = Start_year, xend = maxyr
    )) +
    scale_x_continuous(
      breaks = howmanyboats$YEAR,
      labels = paste0(
        howmanyboats$YEAR,
        howmanyboats$annotation_star
      )
    ) +
    theme_light(base_size = 14) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

  png(
    filename = paste0(
      dir_out_figures, maxyr, "_bottomtemp.png"
    ),
    width = 8, height = 5.5, units = "in", res = 200
  )
  print(bottom_temp_plot)
  dev.off()

  png(
    filename = paste0(
      dir_out_figures, maxyr, "_surfacetemp.png"
    ),
    width = 8, height = 5.5, units = "in", res = 200
  )
  print(surface_temp_plot)
  dev.off()

  list_temperature[[1]] <- bottom_temp_plot
  list_temperature[[2]] <- surface_temp_plot

  names(list_temperature) <- c("bottomtemp", "surfacetemp")

  save(list_temperature, file = paste0(dir_out_figures, "list_temperature.rdata"))
  print("Done with temperature plots.")
} #/temperature plots

# ################### SLIDE PRODUCTION #######################################
# If you already made all the figures and you just need to knit them into a presentation, you can start here. Make sure you modify figuredate and tabledate to reflect the date you want to use figs and tables from.
# ~###########################################################################

# Make those slides! --------------------------------------------------------
# figuredate <- "2024-10-11" # hard coded, **k it!
# tabledate <- "2024-10-11"
# 
# cat(
#   "Using report data from", tabledate, "for tables. \n",
#   "Using report data from", figuredate, "for figures."
# )

# If some plots aren't loaded into the environment, load them:

if (!exists("list_biomass_ts")) {
  load(paste0("output/", SRVY, "_", maxyr, "/", "figures/", "list_biomass_ts.rdata"))
}

if (!exists("list_cpue_bubbles_strata")) {
  load(paste0("output/", SRVY, "_", maxyr, "/", "figures/", "list_cpue_bubbles_strata.rdata"))
}

if (!exists("list_joy_length")) {
  load(paste0("output/", SRVY, "_", maxyr, "/", "figures/", "list_joy_length.rdata"))
}
if (!exists("list_temperature")) {
  load(paste0("output/", SRVY, "_", maxyr, "/", "figures/", "list_temperature.rdata"))
}
if (!exists("p2")) {
  load(paste0("output/", figuredate, "/", "figures/", "catch_comp.rdata"))
}
if (!exists("compare_tab_pres")) {
  read.csv(file = paste0("output/", tabledate, "/", "tables/", "compare_tab_pres.csv"))
}

# GPT slides special: Order slides in order of highest to lowest biomass
report_species <- biomass_total |>
  dplyr::filter(YEAR == maxyr) |>
  dplyr::right_join(report_species, by = c('SPECIES_CODE'='species_code')) |>
  dplyr::arrange(-BIOMASS_MT) |>
  dplyr::select(group, spp_name_informal, SPECIES_CODE, spp_name_scientific, BIOMASS_MT) |>
  janitor::clean_names()


# Render the Markdown file
starttime <- Sys.time() # timer in case you want to know how long everything takes

rmarkdown::render(paste0(dir_markdown, "/PLAN_TEAM_SLIDES.Rmd"),
  output_dir = dir_out_chapters,
  output_file = paste0("PLAN_TEAM_SLIDES.pptx")
)

print(Sys.time() - starttime)
