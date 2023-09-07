# PRESENTATION FIGURES ----------------------------------------------------
# Table of contents (toggle true/false to make some plots but not others):
# 1. Biomass indices relative to LT mean
starttime <- Sys.time() # timer in case you want to know how long everything takes

make_biomass_timeseries <- TRUE
# 2. Catch composition plot
make_catch_comp <- TRUE
# 3. CPUE bubble maps
make_cpue_bubbles <- FALSE
# 4. Length frequency plots by region and depth stratum
make_length_freqs <- TRUE
# 5. Length frequency plots as joy division plots
make_joy_division_length <- TRUE
# 6. CPUE IDW maps
make_cpue_idw <- TRUE

# Report settings -------------------------------------------------------------
source("R/00_report_settings.R")
source("R/01_directories.R")
SRVY <- "GOA"
maxyr <- 2023 # Change this for the year!
compareyr <- 2021

# Load packages and functions -------------------------------------------------
source("R/02_load_packages.R")
source("R/05_functions.R")

# Get data from RACEBASE --------------------------------------------------
x <- askYesNo(msg = "Do you want to download local versions of Oracle tables now?")
if (x) {
  dir.create("data/local_racebase", recursive = TRUE)
  source("R/03_download_data_from_oracle.R")
}
#
# # Get text from Google Drive ----------------------------------------------
# y <- askYesNo(msg = "Do you want to re-download Google Drive files now?")
# if (y) {
#   source("R/04_get_gdrive_chapters.R")
# }

# Data --------------------------------------------------------------------
# Get species table
if (SRVY == "AI") report_species <- read.csv("data/ai_report_specieslist.csv")
if (SRVY == "GOA") report_species <- read.csv("data/goa_report_specieslist.csv")

report_species <- filter(report_species, presentation==1)

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


# Begin figure creation/report prep ---------------------------------------
# Libraries ---------------------------------------------------------------
library(akgfmaps)
library(patchwork)

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

nstations <- haul2 %>%
  filter(ABUNDANCE_HAUL == "Y") %>%
  distinct(STATIONID, STRATUM) %>%
  nrow() # checked in 2023

nsuccessfulhauls <- haul2 %>%
  filter(ABUNDANCE_HAUL == "Y") %>%
  nrow()

# CPUE 
if(SRVY=="AI"){
  x <- read.csv(file = here::here("data", "local_ai","cpue.csv"), header = TRUE) 

  # This is already 0-filled
cpue_raw <- x %>%
  left_join(common_names) %>%
  dplyr::select(-YEAR_ADDED) %>%
  dplyr::left_join(haul) %>%
  janitor::clean_names() %>% # need to add common name lookup
  dplyr::rename(cpue_kgkm2 = wgtcpue) %>%
  janitor::clean_names()
}

if(SRVY=="GOA"){
  x <- read.csv(file = here::here("data", "local_goa","cpue.csv"), header = TRUE) 
  
  # This is already 0-filled
  cpue_raw <- x %>%
    left_join(common_names) %>%
    dplyr::select(-YEAR_ADDED) %>%
    dplyr::left_join(haul) %>%
    janitor::clean_names() %>% # need to add common name lookup
    dplyr::rename(cpue_kgkm2 = wgtcpue) %>%
    janitor::clean_names()
}


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

if(SRVY=="GOA"){
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
bartheme <- theme_classic2(base_size = 16) +
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
      labs(title = paste0(name_bms)) +
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
  # names(list_biomass_ts) <- report_species$species_code # not sure if I want to keep this
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
    ylab(expression(paste("Total estimated biomass (\u00D7 ", 10^6, " mt)"))) +
    scale_y_continuous(expand = c(0, 0)) +
    bartheme +
    theme(legend.position = "bottom")

  png(
    filename = paste0(dir_out_figures, maxyr, "_biomass_catchcomp.png"),
    width = 12, height = 8, units = "in", res = 150
  )
  print(p2)
  dev.off()
}

# 3. CPUE bubble maps ----------------------------------------------------------
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
# THIS SHOULD ONLY BE USED FOR THE GOA - in the AI, the area is too narrow for a raster map of CPUE and we should instead be using the bubble plots of CPUE.

if (make_cpue_idw) {
  list_idw_cpue <- list()
  for(s in 1:nrow(report_species)){
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
      key.title = paste(report_species$spp_name_informal[s],"(kg/km2)"),
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
  }
  
  names(list_idw_cpue) <- report_species$species_code
  save(list_idw_cpue, file = paste0(dir_out_figures, "list_idw_cpue.rdata"))
}

# ** 4b. Percent changes in biomass since last survey ----------------------------

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


# 5. Length frequency by area/depth stratum ------------------------------------
# Uses only the most recent year (no comparison)

if (make_length_freqs) {
  length <- read.csv(here::here("data", "local_racebase", "length.csv"))

  list_length_freq <- list()

  length2 <- length %>%
    mutate(YEAR = stringr::str_extract(CRUISE, "^\\d{4}")) %>%
    filter(REGION == SRVY) # YEAR == maxyr &

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

  length4 <- length3 %>%
    tidyr::uncount(FREQUENCY) %>%
    mutate(INPFC_AREA = factor(INPFC_AREA, levels = c("Western Aleutians", "Central Aleutians", "Eastern Aleutians", "Southern Bering Sea"), labels = c("Western Aleutians", "Central Aleutians", "Eastern Aleutians", "S. Bering Sea"))) %>%
    group_split(Sex) # turn freq column into rows for histogramming

  lengthpal <- MetBrewer::met.brewer(palette_name =  "Nizami", n = 8)[c(2, 5, 7)] # order: red (females), turquoise (unsexed), blue (males)

  samplesizes <- length3 %>%
    group_by(INPFC_AREA, `Depth range`) %>%
    count() %>%
    ungroup()

  # TODO : add sample numbers to plots.

  for (i in 1:nrow(report_species)) {
    dat2plot <- purrr::map(length4, ~ filter(.x, SPECIES_CODE == report_species$species_code[i]))

    lfplot <- ggplot() +
      # MALES
      geom_histogram(data = dat2plot[[2]], aes(x = LENGTH / 10, fill = Sex), fill = lengthpal[3], alpha = 0.6) + #
      # UNSEXED
      geom_histogram(data = dat2plot[[3]], aes(x = LENGTH / 10, fill = Sex), fill = lengthpal[2], alpha = 0.4) + #
      # FEMALES
      geom_histogram(data = dat2plot[[1]], aes(x = LENGTH / 10, fill = Sex), fill = lengthpal[1], alpha = 0.6) + #
      facet_grid(`Depth range` ~ INPFC_AREA, scales = "free_y", labeller = labeller(groupwrap = label_wrap_gen(10))) +
      labs(title = paste0(YEAR, " - ", report_species$spp_name_informal[i])) +
      xlab("Length (cm)") +
      ylab("Count in length subsample") +
      theme_classic2(base_size = 10) +
      theme(strip.background = element_blank()) +
      theme(legend.position = "bottom")

    legplot <- ggplot(data = length3, aes(x = YEAR, fill = Sex)) +
      geom_histogram(stat = "count", show.legend = TRUE, alpha = 0.6) +
      scale_fill_manual("Sex", values = lengthpal[c(1, 3, 2)]) +
      theme(legend.position = "right")


    legend <- cowplot::get_legend(legplot)

    lfplot2 <- cowplot::ggdraw(cowplot::plot_grid(
      cowplot::plot_grid(lfplot, ncol = 1, align = "v"),
      cowplot::plot_grid(NULL, legend, ncol = 1),
      rel_widths = c(1, 0.2)
    ))

    png(filename = paste0(
      dir_out_figures, maxyr, "_",
      report_species$spp_name_informal[i], "_lengthfreqhist.png"
    ), width = 9, height = 9, units = "in", res = 200)
    print(lfplot2)
    dev.off()

    list_length_freq[[i]] <- lfplot2
  }
  save(list_length_freq, file = paste0(dir_out_figures, "list_length_freq.rdata"))
}


# 6. Length frequency plots - joy division plots -----------------------------

if (make_joy_division_length) {
  list_joy_length <- list()


  length <- read.csv(here::here("data", "local_racebase", "length.csv"))

  length2 <- length %>%
    mutate(YEAR = stringr::str_extract(CRUISE, "^\\d{4}")) %>%
    filter(REGION == SRVY)

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
  for (i in 1:nrow(report_species)) {
    joyplot <- length3 %>%
      filter(SPECIES_CODE == report_species$species_code[i]) %>%
      ggplot(aes(x = LENGTH, y = YEAR, group = YEAR, fill = YEAR)) +
      geom_density_ridges() +
      scale_y_discrete(limits = rev) +
      facet_wrap(~Sex) +
      xlab("Length(mm)") +
      ylab("Year") +
      theme_ridges() +
      scale_fill_manual(values = joypal) +
      labs(title = paste(report_species$spp_name_informal[i]))

    png(filename = paste0(
      dir_out_figures, maxyr, "_",
      report_species$spp_name_informal[i], "_joyfreqhist.png"
    ), width = 12, height = 8, units = "in", res = 200)
    print(joyplot)
    dev.off()

    list_joy_length[[i]] <- joyplot
  }
  save(list_joy_length, file = paste0(dir_out_figures, "list_joy_length.rdata"))
}

# Make those slides! --------------------------------------------------------

rmarkdown::render(paste0(dir_markdown, "/PLAN_TEAM_SLIDES.Rmd"),
  output_dir = dir_out_chapters,
  output_file = paste0("PLAN_TEAM_SLIDES.pptx")
)

print(Sys.time() - starttime)
