# PRESENTATION FIGURES ----------------------------------------------------
# Project: Automated GOA-AI data reports and slides
# Author: Megsie Siple
# Notes: Use this to make pptx slides for the Joint Groundfish Plan Team presentation. This uses tables from Oracle (RACEBASE, AI and GOA schemas) and builds the figures and some summary stats.
# ---


# Table of contents (toggle true/false to make some plots but not others):

# 1. Biomass indices relative to LT mean
make_biomass_timeseries <- TRUE
# 2. Catch composition
make_catch_comp <- TRUE
# 3. CPUE bubble map (Aleutians only)
make_cpue_bubbles <- FALSE
# 4. Length frequency plots by region and depth stratum (probably deprecated - not annual increments)
make_length_freqs <- FALSE
# 5. Length frequency plots as joy division plots (preferred length plot by stock assessment folx)
make_joy_division_length <- TRUE
# 6. CPUE IDW maps
make_cpue_idw <- TRUE
# 7. Plots of surface and bottom temperature
make_temp_plot <- TRUE

# Report settings -------------------------------------------------------------
source("R/00_report_settings.R")
source("R/01_directories.R")
SRVY <- "GOA"
maxyr <- 2023 # Change this for the year!
compareyr <- 2021
dates_conducted <- "May 18th through August 6th, 2023" # EDIT
if (SRVY == "GOA") {
  all_allocation <- read.csv(here::here("data", "local_goa", "goa_station_allocation.csv"))
}

# Load packages and functions -------------------------------------------------
source("R/02_load_packages.R")
source("R/05_functions.R")

# Get data from RACEBASE --------------------------------------------------
x <- askYesNo(msg = "Do you want to download local versions of Oracle tables now?")
if (x) {
  dir.create("data/local_racebase", recursive = TRUE)
  source("R/03_download_data_from_oracle.R")
}

# General data -----------------------------------------------------------------
# Get species table
if (SRVY == "AI") report_species <- read.csv("data/ai_report_specieslist.csv")
if (SRVY == "GOA") report_species <- read.csv("data/goa_report_specieslist.csv")

report_species <- filter(report_species, presentation == 1)

# Get a table of the strata and depths / regions
dat <- read.csv("data/goa_strata.csv", header = TRUE)
region_lu <- dat %>%
  filter(SURVEY == SRVY) %>%
  dplyr::select(SURVEY, STRATUM, INPFC_AREA, MIN_DEPTH, MAX_DEPTH) %>%
  tidyr::unite("Depth range", MIN_DEPTH:MAX_DEPTH, sep = " - ", remove = FALSE) %>%
  mutate(`Depth range` = paste0(`Depth range`, " m"))

if (SRVY == "AI") {
  region_lu <- region_lu %>% filter(STRATUM >= 211 & STRATUM <= 794)
}

# Data to plot ------------------------------------------------------------
# All the species for which we want to make plots
head(report_species)
report_species <- report_species %>%
  arrange(-species_code)

# Get key/table of names (common, scientific, etc)
common_names <- read.csv(here::here("data", "local_racebase", "species.csv"), header = TRUE)

# Load total biomass data (currently taking from local copy; download/update to new one by running the setup script again and downloading fresh tables from oracle)
if (SRVY == "AI") {
  biomass_total <- read.csv("data/local_ai/biomass_total.csv")
}
if (SRVY == "GOA") {
  biomass_total <- read.csv("data/local_goa/biomass_total.csv")
}

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

# CPUE
if (SRVY == "AI") {
  x <- read.csv(file = here::here("data", "local_ai", "cpue.csv"), header = TRUE)

  # This is already 0-filled
  cpue_raw <- x %>%
    left_join(common_names) %>%
    dplyr::select(-YEAR_ADDED) %>%
    dplyr::left_join(haul) %>%
    janitor::clean_names() %>% # need to add common name lookup
    dplyr::rename(cpue_kgkm2 = wgtcpue) %>%
    janitor::clean_names()
}

if (SRVY == "GOA") {
  x <- read.csv(file = here::here("data", "local_goa", "cpue.csv"), header = TRUE)

  # This is already 0-filled
  cpue_raw <- x %>%
    left_join(common_names) %>%
    dplyr::select(-YEAR_ADDED) %>%
    dplyr::left_join(haul) %>%
    janitor::clean_names() %>% # need to add common name lookup
    dplyr::rename(cpue_kgkm2 = wgtcpue) %>%
    janitor::clean_names()
}


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

nfishlengths_reportspps <- length_maxyr %>%
  filter(LENGTH_TYPE %in% c(1, 5, 11)) %>%
  filter(SPECIES_CODE %in% report_species$species_code) %>%
  dplyr::select(FREQUENCY) %>%
  sum() %>%
  format(big.mark = ",")

nsquidlengths <- sum(length_maxyr %>%
  filter(LENGTH_TYPE == 12) %>% dplyr::select(FREQUENCY)) %>%
  format(big.mark = ",")

# Otoliths collected
S <- read.csv(here::here("data", "local_racebase", "specimen.csv"))
specimen_maxyr <- S %>%
  mutate(YEAR = as.numeric(gsub("(^\\d{4}).*", "\\1", CRUISE))) %>%
  filter(YEAR == maxyr & REGION == SRVY)

otos_collected <- specimen_maxyr %>%
  filter(SPECIMEN_SAMPLE_TYPE == 1) %>% # this means it's an oto collection
  dplyr::left_join(haul_maxyr, by = c(
    "CRUISEJOIN", "HAULJOIN", "HAUL",
    "REGION", "VESSEL", "YEAR"
  )) %>%
  dplyr::left_join(region_lu, by = c("STRATUM")) %>%
  group_by(INPFC_AREA, `Depth range`) %>%
  dplyr::summarize("Pairs of otoliths collected" = n()) %>%
  ungroup() %>%
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
  nstrata <- length(unique(floor(ai_east$survey.grid$STRATUM / 10)))
}

if (SRVY == "GOA") {
  a <- read.csv("data/local_goa/goagrid.csv")
  nstrata <- length(unique(a$STRATUM))
}


# Aesthetic settings ------------------------------------------------------

bubbletheme <- theme(
  legend.position = "none",
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
  legend.text = element_text(size = 10),
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
  plot.title = element_text(margin = margin(b = -30))
)


linetheme <- theme_bw(base_size = 16)


bartheme <- ggpubr::theme_classic2(base_size = 12) +
  theme(strip.background = element_blank())

# Palettes!
# MetBrewer (dark colors)
stratumpal <- lengthen_pal(
  shortpal = MetBrewer::met.brewer(palette_name = "Hokusai1", type = "continuous"),
  x = 1:nstrata
)

# Palette for lines
linecolor <- RColorBrewer::brewer.pal(n = 9, name = "Blues")[9]
accentline <- RColorBrewer::brewer.pal(n = 9, name = "Blues")[8]

# Palette for joy div plot
joypal <- lengthen_pal(shortpal = RColorBrewer::brewer.pal(n = 9, name = "Blues"), x = 1:nyears)

# Palette for species colors and fills
# speciescolors <- nmfspalette::nmfs_palette("regional web")(nrow(report_species) + 1)
speciescolors <- lengthen_pal(
  shortpal = MetBrewer::met.brewer(palette_name = "VanGogh2", type = "discrete", direction = -1),
  x = 1:(nrow(report_species) + 1)
)

################### CHUNKS ##################################################
# These can be run individually as needed. For example, if you want to modify all the biomass time series plots at once. If you know you already have satisfactory versions of all these plots, you don't need to re-run this code! The presentation knitting section will check if figs are available and will load them if not.
# ~###########################################################################

# 1. Biomass index relative to LT mean ---------------------------------------

if (make_biomass_timeseries) {
  list_biomass_ts <- list()
  for (i in 1:nrow(report_species)) {
    sp <- report_species$species_code[i]
    name_bms <- report_species$spp_name_informal[i]

    dat <- biomass_total %>%
      filter(SPECIES_CODE == report_species$species_code[i])
    lta <- mean(dat$TOTAL_BIOMASS)

    p1 <- dat %>%
      ggplot(aes(x = YEAR, y = TOTAL_BIOMASS)) +
      geom_hline(yintercept = lta, color = accentline, lwd = 0.7, lty = 2) +
      geom_point(color = linecolor, size = 2) +
      geom_errorbar(aes(ymin = MIN_BIOMASS, ymax = MAX_BIOMASS), color = linecolor, linewidth = 0.9, width = 0.7) +
      ylab("Estimated total biomass (mt)") +
      xlab("Year") +
      # labs(title = paste0(name_bms)) +
      scale_y_continuous(labels = scales::label_comma()) +
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
}


# 2. Catch composition -------------------------------------------------------
if (make_catch_comp) {
  head(biomass_total)
  biomass_total_filtered <- biomass_total %>%
    left_join(report_species,
      by = c("SPECIES_CODE" = "species_code")
    ) %>%
    mutate(spp_name_informal = tidyr::replace_na(data = spp_name_informal, replace = "Other species"))

  biomass_total_filtered$spp_name_informal <- factor(biomass_total_filtered$spp_name_informal,
    levels = c(report_species$spp_name_informal, "Other species")
  )

  p2 <- biomass_total_filtered %>%
    ggplot(aes(fill = spp_name_informal, y = TOTAL_BIOMASS / 10e6, x = YEAR)) +
    geom_bar(position = "stack", stat = "identity") +
    scale_fill_manual("", values = speciescolors) +
    xlab("Year") +
    ylab(expression(paste("Total estimated \nbiomass (\u00D7 ", 10^6, " mt)"))) +
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

# 3. CPUE bubble maps (Aleutians only) -----------------------------------------
if (make_cpue_bubbles) {
  list_cpue_bubbles <- list()
  for (i in 1:nrow(report_species)) {
    spbubble <- report_species$species_code[i]
    namebubble <- report_species$spp_name_informal[i]

    # CPUE data
    thisyrshauldata <- cpue_raw %>%
      filter(year == maxyr & srvy == SRVY & species_code == spbubble) %>%
      st_as_sf(
        coords = c("longitude_dd", "latitude_dd"),
        crs = "EPSG:4326"
      ) %>%
      st_transform(crs = ai_east$crs)

    # MAPS
    p3a <- ggplot() +
      geom_sf(
        data = ai_east$survey.grid,
        mapping = aes(
          fill = factor(floor(STRATUM / 10)),
          color = factor(floor(STRATUM / 10))
        )
      ) +
      scale_fill_manual(values = stratumpal) +
      scale_color_manual(values = stratumpal) +
      geom_sf(data = ai_east$akland) +
      geom_sf(data = thisyrshauldata, aes(size = cpue_kgha), alpha = 0.5) +
      scale_size(limits = c(0, max(thisyrshauldata$cpue_kgha))) +
      coord_sf(
        xlim = ai_east$plot.boundary$x,
        ylim = ai_east$plot.boundary$y
      ) +
      scale_x_continuous(breaks = ai_east$lon.breaks) +
      scale_y_continuous(breaks = ai_east$lat.breaks) +
      labs(subtitle = "Eastern Aleutians") +
      bubbletheme

    p3b <- ggplot() +
      geom_sf(
        data = ai_central$survey.grid,
        mapping = aes(
          fill = factor(floor(STRATUM / 10)),
          color = factor(floor(STRATUM / 10))
        )
      ) +
      scale_fill_manual(values = stratumpal) +
      scale_color_manual(values = stratumpal) +
      geom_sf(data = ai_central$akland) +
      geom_sf(data = thisyrshauldata, aes(size = cpue_kgha), alpha = 0.5) +
      scale_size(limits = c(0, max(thisyrshauldata$cpue_kgha))) +
      coord_sf(
        xlim = ai_east$plot.boundary$x,
        ylim = ai_east$plot.boundary$y
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
        data = ai_west$survey.grid,
        mapping = aes(
          fill = factor(floor(STRATUM / 10)),
          color = factor(floor(STRATUM / 10))
        )
      ) +
      scale_fill_manual(values = stratumpal) +
      scale_color_manual(values = stratumpal) +
      geom_sf(data = ai_west$akland) +
      geom_sf(data = thisyrshauldata, aes(size = cpue_kgha), alpha = 0.5) +
      scale_size(limits = c(0, max(thisyrshauldata$cpue_kgha))) +
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

    png(
      filename = paste0(dir_out_figures, namebubble, "_", maxyr, "_bubble_example.png"),
      width = 10, height = 10, units = "in", res = 200
    )
    print(final_obj)

    dev.off()

    list_cpue_bubbles[[i]] <- final_obj # save fig to list
  } # /end species loop
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

# 5. Percent changes in biomass since last survey ----------------------------

head(biomass_total)

compare_tab <- biomass_total %>%
  filter(YEAR %in% c(maxyr, compareyr) &
    SPECIES_CODE %in% report_species$species_code) %>%
  dplyr::select(YEAR, SPECIES_CODE, TOTAL_BIOMASS) %>%
  dplyr::arrange(YEAR) %>%
  tidyr::pivot_wider(
    names_from = YEAR,
    values_from = TOTAL_BIOMASS,
    names_prefix = "yr_"
  ) %>%
  as.data.frame()

compare_tab$percent_change <- round((compare_tab[, 3] - compare_tab[, 2]) / compare_tab[, 2] * 100, digits = 1)
names(compare_tab)

compare_tab2 <- compare_tab %>%
  left_join(report_species, by = c("SPECIES_CODE" = "species_code")) %>%
  arrange(-SPECIES_CODE) %>%
  dplyr::select(-(presentation:reportorder))

write.csv(
  x = compare_tab2,
  file = paste0(dir_out_tables, maxyr, "_comparison_w_previous_survey.csv")
)


# 6. Length frequency by area/depth stratum ------------------------------------
# Uses only the most recent year (no comparison)

if (make_length_freqs) {
  # Load expanded lengths. If these aren't generated above, they can be made in the prep_data file.
  # lengths_expanded <- read.csv(paste0("data/",maxyr,"_",SRVY,"_report_pseudolengths.csv"))
  # dat2plot <- lengths_expanded %>%
  #   filter(YEAR==maxyr)
  #
  # list_length_freq <- list()
  #
  # for(i in 1:nrow(report_species)){
  #   dat_sp <- dat2plot %>%
  #     dplyr::filter(SPECIES_CODE==report_species$species_code[i])
  #  # p1 <-
  # }
  #
  #
  #   png(filename = paste0(
  #     dir_out_figures, maxyr, "_",
  #     report_species$spp_name_informal[i], "_lengthfreqhist.png"
  #   ), width = 9, height = 9, units = "in", res = 200)
  #   print(lfplot2)
  #   dev.off()
  #
  #   list_length_freq[[i]] <- lfplot2
  # }
  # save(list_length_freq, file = paste0(dir_out_figures, "list_length_freq.rdata"))
}

# 7. Joy division plots - Length frequency -----------------------------

if (make_joy_division_length) {
  list_joy_length <- list()

  report_pseudolengths <- read.csv(paste0("data/", maxyr, "_", SRVY, "_report_pseudolengths.csv"))

  # This is repeated; deal with it later
  L <- read.csv(here::here("data/local_racebase/length.csv"))
  L <- L %>%
    mutate(YEAR = as.numeric(gsub("(^\\d{4}).*", "\\1", CRUISE)))
  length_maxyr <- filter(L, YEAR == maxyr & REGION == SRVY)

  length2 <- L %>% # L is the big length table from RACEBASE
    mutate(YEAR = stringr::str_extract(CRUISE, "^\\d{4}")) %>%
    filter(REGION == SRVY) # want to keep all years for this fig

  length3 <- length2 %>%
    left_join(haul2, by = c("HAULJOIN", "YEAR", "CRUISEJOIN", "VESSEL", "CRUISE", "HAUL")) %>%
    dplyr::select(VESSEL, YEAR, LENGTH, FREQUENCY, SEX, GEAR_DEPTH, STRATUM, SPECIES_CODE) %>%
    left_join(region_lu, by = "STRATUM") %>%
    mutate(Sex = case_when(
      SEX == 1 ~ "Male",
      SEX == 2 ~ "Female",
      SEX == 3 ~ "Unsexed"
    )) %>%
    dplyr::select(-SEX, -MIN_DEPTH, -MAX_DEPTH)

  sample_sizes <- length3 %>%
    filter(YEAR >= minyr) %>%
    dplyr::group_by(YEAR, SPECIES_CODE, Sex) %>%
    dplyr::summarize(n = sum(FREQUENCY)) %>%
    ungroup() %>%
    mutate(YEAR = as.integer(YEAR))

  # NRS/SRS complex: create a lumped plot with the full complex for the various species that used to be lumped
  complex_lookup <- data.frame(
    polycode = c(
      c(10260, 10261, 10262, 10263),
      c(10110, 10112),
      c(30050, 30051, 30052)
    ),
    complex = c(
      rep("nrs_srs", times = 4),
      rep("kam_atf", times = 2),
      rep("rebs", times = 3)
    )
  ) %>%
    mutate(complex_name = case_when(
      complex == "nrs_srs" ~ "Northern and southern rock sole",
      complex == "kam_atf" ~ "Kamchatka flounder and arrowtooth flounder",
      complex == "rebs" ~ "Rougheye/blackspotted rockfish"
    ))

  # If Gulf survey, don't do the combined plot for ATF/kam (there aren't enough kam)
  if (SRVY == "GOA") {
    complex_lookup <- filter(complex_lookup, complex != "kam_atf")
  }

  # Loop thru species
  for (i in 1:nrow(report_species)) {
    # These are multipliers for where the sample size geom_text falls on the y axis

    len2plot <- report_pseudolengths %>%
      filter(SPECIES_CODE == report_species$species_code[i])

    # Only sexed lengths included, unless it's SSTH
    if (report_species$species_code[i] != 30020) {
      len2plot <- len2plot %>%
        filter(Sex != "Unsexed")
    }

    # Save median lengths by year and sex for species i
    medlines_sp <- report_pseudolengths %>%
      filter(SPECIES_CODE == report_species$species_code[i]) %>%
      group_by(YEAR, Sex) %>%
      dplyr::summarize(medlength = median(LENGTH, na.rm = T)) %>%
      ungroup()

    # ylocs <- report_pseudolengths %>%
    #   filter(SPECIES_CODE == report_species$species_code[i]) %>%
    #   group_by(YEAR, Sex) %>%
    #   dplyr::summarize(maxlength = max(LENGTH,na.rm=T)) %>%
    #   mutate(yloc = Inf) %>%
    #   ungroup() %>%
    #   filter(YEAR == 2012) %>%
    #   dplyr::select(-YEAR)

    write.csv(
      x = medlines_sp,
      file = paste0(dir_out_tables, maxyr, "_", report_species$spp_name_informal[i], "_median_lengths", ".csv"),
      row.names = FALSE
    )

    len2plot2 <- len2plot %>%
      left_join(sample_sizes %>%
        filter(SPECIES_CODE == report_species$species_code[i]))
    # %>%
    #   left_join(ylocs)

    yrbreaks <- unique(len2plot2$YEAR)
    lengthlimits <- range(len2plot2$LENGTH)

    testlabdf <- len2plot2 %>%
      distinct(YEAR, Sex, .keep_all = TRUE)

    joyplot <- len2plot2 %>%
      ggplot(mapping = aes(x = LENGTH, y = YEAR, group = YEAR, fill = after_stat(x))) +
      ggridges::geom_density_ridges_gradient( #
        bandwidth = 5,
        rel_min_height = 0,
        quantile_lines = T,
        quantile_fun = median,
        vline_color = "white",
        vline_size = 0.6,
        vline_linetype = "dotted"
        # "A1"
      ) +
      scale_y_reverse(breaks = yrbreaks, expand = c(0, 0)) +
      scale_x_continuous(expand = c(0, 0), limits = lengthlimits) + #
      scale_linetype_manual(values = c("solid", "dashed")) +
      facet_grid(~Sex) +
      xlab("Length (mm)") +
      ylab("Year") +
      theme_ridges(font_size = 8) +
      scale_fill_gradientn(colours = joypal) +
      geom_label(
        data = testlabdf,
        mapping = aes(label = paste0("n = ", n), x = Inf),
        fill = "white", label.size = NA,
        nudge_x = 0,
        nudge_y = 1,
        hjust = "inward", size = 2
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

    # is the species in one of the complexes? (or, species that used to be ID'ed differently somehow)
    if (report_species$species_code[i] %in% complex_lookup$polycode) {
      # Add label to plot of the species so ppl can compare it with the combined one
      joyplot <- joyplot + labs(title = paste(report_species$spp_name_informal[i]))

      # Make a title for the combined plot (single species + combined congeners)
      plot_title <- complex_lookup$complex_name[which(complex_lookup$polycode == report_species$species_code[i])]

      complex_sp <- complex_lookup$complex[which(complex_lookup$polycode == report_species$species_code[i])]
      polycode_vec <- complex_lookup$polycode[which(complex_lookup$complex == complex_sp)]
      star_yr <- switch(complex_sp,
        nrs_srs = 1996,
        kam_atf = 1992,
        rebs = 2006
      )
      yrlabels <- yrbreaks
      yrlabels[which(yrlabels < star_yr)] <- paste0(yrlabels[which(yrlabels < star_yr)], "*")
      yrlabels <- as.character(yrlabels)

      medlines_sp <- report_pseudolengths %>%
        filter(SPECIES_CODE %in% polycode_vec) %>%
        group_by(YEAR, Sex) %>%
        dplyr::summarize(medlength = median(LENGTH, na.rm = T)) %>%
        # dplyr::mutate(yloc = medlength * multiplier) %>%
        ungroup()


      sample_sizes_comb <- sample_sizes %>%
        filter(SPECIES_CODE %in% polycode_vec) %>%
        group_by(YEAR, Sex) %>%
        dplyr::summarize(n = sum(n)) %>%
        ungroup()

      len2plot_comb <- report_pseudolengths %>%
        filter(SPECIES_CODE %in% polycode_vec) %>%
        filter(Sex != "Unsexed") %>%
        left_join(sample_sizes_comb)

      testlabdf_comb <- len2plot_comb %>%
        distinct(YEAR, Sex, .keep_all = TRUE)

      joyplot2 <- len2plot_comb %>%
        ggplot(
          mapping = aes(x = LENGTH, y = YEAR, group = YEAR),
          fill = "grey"
        ) +
        ggridges::geom_density_ridges_gradient(
          bandwidth = 5,
          rel_min_height = 0,
          quantile_lines = T,
          quantile_fun = median,
          vline_color = "white",
          vline_size = 0.6,
          vline_linetype = "dotted" # "A1"
        ) +
        scale_y_reverse(breaks = yrbreaks, labels = yrlabels, expand = c(0, 0)) +
        scale_linetype_manual(values = c("solid", "dashed")) +
        geom_label(
          data = testlabdf_comb,
          mapping = aes(label = paste0("n = ", n), x = Inf),
          fill = "white", label.size = NA,
          nudge_x = -100, nudge_y = 1, hjust = "inward", size = 2
        ) +
        facet_grid(~Sex) +
        xlab("Length (mm)") +
        ylab("Year") +
        theme_ridges(font_size = 8) +
        labs(title = plot_title) +
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

      joyplot <- joyplot + joyplot2
    }


    png(filename = paste0(
      dir_out_figures, maxyr, "_",
      report_species$spp_name_informal[i], "_joyfreqhist.png"
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

  # haul info (source: RACEBASE)
  haul <- read.csv(here::here("data", "local_racebase", "haul.csv"))

  sstdat <- haul %>%
    mutate(YEAR = stringr::str_extract(CRUISE, "^\\d{4}")) %>%
    filter(YEAR >= 1994 & REGION == SRVY & YEAR != 1997) %>%
    group_by(YEAR) %>%
    dplyr::summarize(
      bottom = mean(GEAR_TEMPERATURE, na.rm = TRUE),
      surface = mean(SURFACE_TEMPERATURE, na.rm = TRUE)
    ) %>%
    ungroup() %>%
    as.data.frame() %>%
    mutate(YEAR = as.numeric(YEAR))

  sst_summary <- sstdat %>%
    mutate(
      bottom_stz = bottom - mean(bottom, na.rm = T),
      surface_stz = surface - mean(surface, na.rm = T)
    ) %>%
    pivot_longer(cols = bottom:surface_stz)

  plotdat <- haul %>%
    mutate(YEAR = stringr::str_extract(CRUISE, "^\\d{4}")) %>%
    filter(YEAR >= 1994 & REGION == SRVY & YEAR != 1997) %>%
    filter(CRUISE != 201402) %>% # remove study from Makushin bay in 2014 (contains a zero BT)
    filter(HAULJOIN != -17737) # Filter out the situation with BT=0 in 2018


  bottom_temp_plot <- plotdat %>%
    ggplot(aes(y = GEAR_TEMPERATURE, x = YEAR)) +
    ggdist::stat_interval() +
    ggdist::stat_halfeye(fill = "tan", alpha = 0.3) +
    geom_point(size = 0.5, color = "gray5") +
    rcartocolor::scale_color_carto_d("Quantile", palette = "Peach") +
    scale_fill_ramp_discrete(na.translate = FALSE) +
    labs(x = "Year", y = expression("Bottom temperature "(degree * C))) + #
    theme_light()

  surface_temp_plot <- plotdat %>%
    ggplot(aes(y = SURFACE_TEMPERATURE, x = YEAR)) +
    ggdist::stat_interval() +
    ggdist::stat_halfeye(fill = "tan", alpha = 0.3) +
    geom_point(size = 0.5, color = "gray5") +
    rcartocolor::scale_color_carto_d("Quantile", palette = "Peach") +
    scale_fill_ramp_discrete(na.translate = FALSE) +
    labs(x = "Year", y = expression("Surface temperature "(degree * C))) +
    theme_light()

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
  print("Done with joy division plots for length comp.")
}


# ################### SLIDE PRODUCTION #######################################
# If you already made all the figures and you just need to knit them into a presentation, you can start here. Make sure you modify figuredate and tabledate to reflect the date you want to use figs and tables from.
# ~###########################################################################

# Make those slides! --------------------------------------------------------
figuredate <- "2023-09-11" # hard coded, **k it!
tabledate <- "2023-09-11"

cat(
  "Using report data from", tabledate, "for tables. \n",
  "Using report data from", figuredate, "for figures."
)

# If some plots aren't loaded into the environment, load them:
if (!exists("list_idw_cpue")) {
  load(paste0("output/", figuredate, "/", "figures/", "list_idw_cpue.rdata"))
}

if (!exists("list_biomass_ts")) {
  load(paste0("output/", figuredate, "/", "figures/", "list_biomass_ts.rdata"))
}
if (!exists("list_joy_length")) {
  load(paste0("output/", figuredate, "/", "figures/", "list_joy_length.rdata"))
}
if (!exists("list_temperature")) {
  load(paste0("output/", figuredate, "/", "figures/", "list_temperature.rdata"))
}


# Make sure you have the list of species
if (!exists("report_species")) {
  if (SRVY == "AI") report_species <- read.csv("data/ai_report_specieslist.csv")
  if (SRVY == "GOA") report_species <- read.csv("data/goa_report_specieslist.csv")
}


# Render the Markdown file
starttime <- Sys.time() # timer in case you want to know how long everything takes

rmarkdown::render(paste0(dir_markdown, "/PLAN_TEAM_SLIDES.Rmd"),
  output_dir = dir_out_chapters,
  output_file = paste0("PLAN_TEAM_SLIDES.pptx")
)

print(Sys.time() - starttime)
