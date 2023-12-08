# This script should load the necessary data and build PNGs of all the figures for the paper, so they can be loaded in the Markdown part.

# Figures & their name in the list ----------------------------------------
# Fig 1: district map
# Fig 2. CPUE map 1 "cpue_bubbles" - list_cpue_bubbles
# Fig 3. CPUE map 2 - raster maps of CPUE - ONLY FOR GOA
# Fig 4. size comp plot 2 - "joy_division_length" - list_joy_length
# Fig 5. Historical biomass plots - list_biomass_ts

make_biomass_timeseries <- FALSE
# 2. Catch composition plot
make_catch_comp <- TRUE
# 3. CPUE bubble maps - strata are shaded in. These were presented at GPT 2022
make_cpue_bubbles_strata <- FALSE
# 3b. CPUE bubble maps, Emily M edition - strata not shown. Bubbles are purple. Scale bar and legend with CPUE scale are shown clearly.
make_cpue_bubbles <- TRUE
# 5. Length frequency plots as joy division plots
make_joy_division_length <- TRUE
# 5b. Length vs. depth, facetted by district
make_ldscatter <- TRUE
make_ldcloud <- FALSE
# 6. Plot of surface and bottom SST with long term avg
make_temp_plot <- TRUE
# XX. Make a map of the full survey area with strata and stations
make_total_surv_map <- TRUE

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
  # From Ned
  a <- read.csv("data/goa_strata.csv")
  a <- dplyr::filter(a, MIN_DEPTH < 700 & SURVEY=="GOA" )
  nstrata <-  length(unique(a$STRATUM))
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


linetheme <- theme_bw(base_size = 12)

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
joypal <- c("#d1eeea", "#a8dbd9", "#85c4c9", "#68abb8", "#4f90a6", "#3b738f", "#2a5674") # Mint palette
joypal <- c("#d2fbd4", "#a5dbc2", "#7bbcb0", "#559c9e", "#3a7c89", "#235d72", "#123f5a") # more green palette
joypal <- lengthen_pal(shortpal = RColorBrewer::brewer.pal(n = 9, name = "Blues"), x = 1:nyears)
joypal_grey <- grey.colors(n = 7)

# Palette for survey regions
dispal <- c(met.brewer(
  palette_name = "Nizami", type = "discrete", n = 4,
  direction = -1
), "black")

dispal <- c("#441151","#90be6d","#de541e","#a7a5c6","#2d3047")

# Palette for species colors and fills
speciescolors <- lengthen_pal(
  shortpal = MetBrewer::met.brewer(palette_name = "VanGogh2", type = "discrete", direction = -1),
  x = 1:(nrow(report_species) + 1)
)


# Fonts -------------------------------------------------------------------
# windowsFonts("Montserrat" = windowsFont("Montserrat"))
# if(full_font_import){
#   extrafont::font_import()
#   loadfonts(device = "win")
# }

# 0. Static figure: INPFC areas ----------------------------------------------
#**** TODO: Load png or whatever and put it here. Can I store it in a list?
# Needed to use magick pkg to convert the image to a png, for some reason the usual image reader in Windows does not actually convert the format.
# error_file = magick::image_read("img/AleutiansMap.png")
# right_png <- magick::image_convert(image = error_file, "png")
# magick::image_write(right_png, path = "img/AleutiansMap.png", format = "png")
if (SRVY == "AI") {
  img1_path <- "img/AleutiansMap.png"
}
if (SRVY == "GOA") {
  img1_path <- "img/INPFC_areas_GOA.png"
}

img1 <- png::readPNG(img1_path)
# attr(img1, "info")


# 0b: INPFC areas with stations sampled -----------------------------------
if (make_total_surv_map) {
  if (SRVY == "GOA") {
    reg_dat_goa <- akgfmaps::get_base_layers(
      select.region = "goa",
      set.crs = "EPSG:3338"
    )
    reg_dat_goa$survey.area <- reg_dat_goa$survey.area |>
      dplyr::mutate(
        SRVY = "GOA",
        color = scales::alpha(colour = "grey80", 0.7),
        SURVEY = "Gulf of Alaska"
      )
    reg_data <- reg_dat_goa
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

  goa_all <- akgfmaps::get_base_layers(select.region = "goa", set.crs = "auto")

  goa_inpfc <- akgfmaps::get_base_layers(select.region = "inpfc.goa", set.crs = "auto")
  # goa_nmfs <- akgfmaps::get_base_layers(select.region = "nmfs", set.crs = "auto")

  geo_order <- c("Shumagin", "Chirikof", "Kodiak", "Yakutat", "Southeastern")
  palette_map <- MetBrewer::met.brewer(palette_name = "Nizami", n = 6, type = "discrete", direction = 1)[c(1, 4, 2, 5, 3)]

  thisyrshauldata <- cpue_raw %>%
    dplyr::filter(year == maxyr & survey == SRVY) %>%
    st_as_sf(
      coords = c("start_longitude", "start_latitude"),
      crs = "EPSG:4326"
    ) %>%
    st_transform(crs = reg_data$crs)

  p1 <- ggplot() +
    geom_sf(data = goa_all$akland) +
    geom_sf(data = goa_inpfc, aes(fill = INPFC_STRATUM)) +
    scale_fill_manual("INPFC area", values = palette_map, breaks = geo_order) +
    # geom_sf(data = GOA$bathymetry) +
    geom_sf(data = goa_all$survey.area, fill = NA) +
    coord_sf(
      xlim = goa_all$plot.boundary$x,
      ylim = goa_all$plot.boundary$y
    ) +
    theme_light() +
    theme(legend.position = "bottom")


  station_map <- p1 +
    geom_sf(data = thisyrshauldata, size = 0.5) +
    coord_sf(
      xlim = goa_all$plot.boundary$x,
      ylim = goa_all$plot.boundary$y
    ) +
    guides(fill=guide_legend(nrow=2, byrow=TRUE))

  png(
    filename = paste0(dir_out_figures, maxyr, "_station_map.png"),
    width = 8, height = 5, units = "in", res = 150
  )
  print(station_map)
  dev.off()

  save(station_map, file = paste0(dir_out_figures, maxyr, "_station_map.RDS"))
}

# 1. Biomass index relative to LT mean ---------------------------------------

if (make_biomass_timeseries) {
  list_biomass_ts <- list()
  for (i in 1:nrow(report_species)) { #
    sp <- report_species$species_code[i]
    name_bms <- report_species$spp_name_informal[i]

    dat <- biomass_total %>%
      filter(SPECIES_CODE == report_species$species_code[i])
    lta <- mean(dat$TOTAL_BIOMASS)

    p1 <- dat %>%
      ggplot(aes(x = YEAR, y = TOTAL_BIOMASS)) +
      geom_hline(yintercept = lta, color = accentline, lwd = 0.7, lty = 2) +
      geom_point(color = linecolor, size = 2) +
      geom_errorbar(aes(ymin = MIN_BIOMASS, ymax = MAX_BIOMASS),
        color = linecolor, linewidth = 0.9, width = 0.7
      ) +
      ylab("Estimated total biomass (mt)") +
      xlab("Year") +
      scale_y_continuous(labels = scales::label_comma()) +
      linetheme
    p1

    list_biomass_ts[[i]] <- p1
    names(list_biomass_ts)[[i]] <- report_species$species_code[i]

    png(
      filename = paste0(dir_out_figures, name_bms, "_", YEAR, "_biomass_ts.png"),
      width = 7, height = 7, units = "in", res = 150
    )
    print(p1)
    dev.off()
  } # /end species loop
  names(list_biomass_ts) <- report_species$species_code
  save(list_biomass_ts, file = paste0(dir_out_figures, "biomass_ts.rdata"))
  print("Done making biomass time series plots.")
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
    bartheme

  png(
    filename = paste0(dir_out_figures, YEAR, "_biomass_catchcomp.png"),
    width = 7, height = 7, units = "in", res = 150
  )
  print(p2)
  dev.off()
}

# 3. CPUE bubble maps - strata colored in (presented at GPT 2022) ----------------------------------------------------------
if (make_cpue_bubbles_strata) {
  list_cpue_bubbles_strata <- list()
  for (i in 1:nrow(report_species)) {
    spbubble <- report_species$species_code[i]
    namebubble <- report_species$spp_name_informal[i]

    thisyrshauldata <- cpue_raw %>%
      filter(year == maxyr & survey == SRVY & species_code == spbubble) %>%
      st_as_sf(
        coords = c("start_longitude", "start_latitude"), # TODO NEED TO CHANGE TO THE RIGHT COORDS
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
      geom_sf(data = thisyrshauldata, aes(size = cpue_kgkm2), alpha = 0.5) + # USED TO BE cpue_kgha
      scale_size(limits = c(0, max(thisyrshauldata$cpue_kgkm2))) +
      coord_sf(
        xlim = ai_east$plot.boundary$x,
        ylim = ai_east$plot.boundary$y
      ) +
      scale_x_continuous(breaks = ai_east$lon.breaks) +
      scale_y_continuous(breaks = ai_east$lat.breaks) +
      labs(subtitle = "Eastern Aleutians and Southern Bering Sea") +
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
      geom_sf(data = thisyrshauldata, aes(size = cpue_kgkm2), alpha = 0.5) +
      scale_size(limits = c(0, max(thisyrshauldata$cpue_kgkm2))) +
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
      geom_sf(data = thisyrshauldata, aes(size = cpue_kgkm2), alpha = 0.5) +
      scale_size(limits = c(0, max(thisyrshauldata$cpue_kgkm2))) +
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
      filename = paste0(dir_out_figures, namebubble, "_", maxyr, "_bubble.png"),
      width = 10, height = 10, units = "in", res = 200
    )
    print(final_obj)

    dev.off()

    list_cpue_bubbles_strata[[i]] <- final_obj # save fig to list
  } # /end species loop
  names(list_cpue_bubbles_strata) <- report_species$species_code
  save(list_cpue_bubbles_strata, file = paste0(dir_out_figures, "cpue_bubbles_strata.rdata"))
  print("Done with CPUE bubble maps showing stratum areas.")
}

# 3b. CPUE bubble maps for AI - b&w Emily M style bubble plots ----------------------------------------------------------
if (make_cpue_bubbles) {
  if (SRVY == "GOA") {
    reg_dat_goa <- akgfmaps::get_base_layers(
      select.region = "goa",
      set.crs = "EPSG:3338"
    )
    reg_dat_goa$survey.area <- reg_dat_goa$survey.area |>
      dplyr::mutate(
        SRVY = "GOA",
        color = scales::alpha(colour = "grey80", 0.7),
        SURVEY = "Gulf of Alaska"
      )
    reg_data <- reg_dat_goa
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
  
  list_cpue_bubbles <- list()

  for (i in 1:nrow(report_species)) {
    spbubble <- report_species$species_code[i]

    # cpue_raw is generated in prep_data.R and is a summary of cpue by sps and station
    thisyrshauldata <- cpue_raw %>%
      dplyr::mutate(cpue_kgha = cpue_kgkm2 / 100) %>%
      dplyr::filter(year == maxyr & survey == SRVY & species_code == spbubble) %>%
      st_as_sf(
        coords = c("start_longitude", "start_latitude"),
        crs = "EPSG:4326"
      ) %>%
      st_transform(crs = reg_data$crs)

    fig <- plot_pa_xbyx(
      spcode = spbubble,
      dat = thisyrshauldata,
      yrs = c(maxyr),
      key.title = "",
      row0 = 2, reg_dat = reg_data, dist_unit = "nm", # nautical miles
      col_viridis = "mako", plot_coldpool = FALSE, plot_stratum = FALSE
    )
    list_cpue_bubbles[[i]] <- fig

    png(filename = paste0(
      dir_out_figures, maxyr, "_",
      report_species$spp_name_informal[i], "_CPUE_cpue_bubble.png"
    ), width = 8, height = 5.5, units = "in", res = 200)
    print(fig)
    dev.off()
  }
  names(list_cpue_bubbles) <- report_species$species_code
  save(list_cpue_bubbles, file = paste0(dir_out_figures, "cpue_bubbles.rdata"))
  print("Done with bubble maps of CPUE.")
}

# 5. Length frequency plots - joy division plots -----------------------------

if (make_joy_division_length) {
  list_joy_length <- list()

  if (file.exists(paste0("data/", maxyr, "_", SRVY, "_report_pseudolengths.csv"))) {
    report_pseudolengths <- read.csv(paste0("data/", maxyr, "_", SRVY, "_report_pseudolengths.csv"))
  } else {
    cat("Pseudolength file not found. Sourcing data prep file... \n")
    source("R/06_prep_data.R")
  }
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
      # c(10110, 10112),
      c(30050, 30051, 30052)
    ),
    complex = c(
      rep("nrs_srs", times = 4),
      # rep("kam_atf", times = 2),
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

    if (SRVY == "AI" & report_species$species_code[i] %in% c(10110, 10112)) {
      len2plot <- len2plot %>%
        dplyr::filter(YEAR >= 1994)
    }

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

    write.csv(
      x = medlines_sp,
      file = paste0(dir_out_tables, maxyr, "_", report_species$spp_name_informal[i], "_median_lengths", ".csv"),
      row.names = FALSE
    )

    len2plot2 <- len2plot %>%
      left_join(sample_sizes %>%
        filter(SPECIES_CODE == report_species$species_code[i]))

    yrbreaks <- unique(len2plot2$YEAR)
    lengthlimits <- range(len2plot2$LENGTH)

    testlabdf <- len2plot2 %>%
      distinct(YEAR, Sex, .keep_all = TRUE)



    joyplot <- len2plot2 %>%
      ggplot(mapping = aes(x = LENGTH, y = YEAR, group = YEAR, fill = after_stat(x))) +
      ggridges::geom_density_ridges_gradient(
        bandwidth = 5,
        rel_min_height = 0,
        quantile_lines = T,
        quantile_fun = median,
        vline_color = "white",
        vline_size = 0.6,
        vline_linetype = "dotted" # "A1"
      ) +
      scale_y_reverse(breaks = yrbreaks, expand = c(0, 0)) +
        scale_x_continuous(expand = c(0,0), limits = lengthlimits) +
      scale_linetype_manual(values = c("solid", "dashed")) +
      facet_grid(~Sex) +
      xlab("Length (mm)") +
      ylab("Year") +
      theme_ridges(font_size = 10) +
      scale_fill_gradientn(colours = joypal) +
      geom_label(
        data = testlabdf,
        mapping = aes(label = paste0("n = ", n), x = Inf),
        fill = "white", label.size = NA,
        nudge_x = 0, nudge_y = 1,
        hjust = "inward",
        size = 3
      ) +
      # labs(title = paste(report_species$spp_name_informal[i])) +
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
          nudge_x = -100, nudge_y = 1, hjust = "inward", size = 3
        ) +
        facet_grid(~Sex) +
        xlab("Length (mm)") +
        ylab("Year") +
        theme_ridges(font_size = 7) +
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

  save(list_joy_length, file = paste0(dir_out_figures, "joy_length.rdata"))
  print("Done with joy division plots for length comp.")
}


# 5b. Scatter plots of length by depth by region --------------------------

if (make_ldcloud) {
  list_ldcloud <- list()
  #depth_trend_df <- data.frame(report_species = NA, depth_trend = NA)
  load(paste0(dir_in_tables,"/sizecomps_expanded.RDS"))
  
  for (i in 1:nrow(report_species)) {
    ltoplot <- sizecomps_expanded |> 
      filter(SPECIES_CODE==report_species$species_code[i])
    
    ltoplot2 <- ltoplot |>
      mutate(AREA_NAME = "All districts") |>
      bind_rows(ltoplot)

    ltoplot2$AREA_NAME <- factor(ltoplot2$AREA_NAME, 
                                 levels = c(district_order, "All districts"))
    ltoplot2$depth_range <- factor(ltoplot2$depth_range, 
                                   levels = rev(unique(ltoplot2$depth_range)))
    
    plot <- ltoplot2 |>
      ggplot(aes(depth_range, LENGTH_CM)) +
      ggdist::stat_halfeye(adjust = .5, width = .6, .width = 0, justification = -.3, point_colour = NA) + 
      geom_boxplot(width = .1, outlier.shape = NA) +
      coord_flip() + 
      facet_wrap(~AREA_NAME) +
      xlab("Bottom depth") +
      ylab("Length (cm)") +
      theme_light(base_size = 14) +
      theme(
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none",
        strip.background = element_blank(),
        strip.text = element_text(colour = 'black')
      )
    
    png(filename = paste0(
      dir_out_figures, maxyr, "_",
      report_species$spp_name_informal[i], "_ldcloud.png"
    ), width = 9, height = 2, units = "in", res = 200)
    print(ldscatter)
    dev.off()
    
    list_ldscatter[[i]] <- ldscatter
    print(paste("Done with length by depth raincloud plot of", report_species$spp_name_informal[i]))
  }
  names(list_ldscatter) <- report_species$species_code
  save(list_ldscatter, file = paste0(dir_out_figures, "list_ldcloud.rdata"))
  print("Done with length by depth raincloud plots.")
}

if (make_ldscatter) {
  lscale <- 10
  dscale <- 100

  list_ldscatter <- list()
  # L_maxyr is subsetted to region (SRVY) and the year you're making the report for
  for (i in 1:nrow(report_species)) {
    ltoplot <- L_maxyr %>%
      dplyr::filter(SPECIES_CODE == report_species$species_code[i]) %>%
      dplyr::left_join(haul2, by = c(
        "CRUISEJOIN", "HAULJOIN", "HAUL",
        "REGION", "VESSEL", "CRUISE"
      )) %>%
      dplyr::left_join(region_lu, by = "STRATUM") %>%
      dplyr::filter(ABUNDANCE_HAUL == "Y") %>%
      dplyr::filter(HAULJOIN != -21810) # take out haul 191 from OEX 2022 which i JUST DISCOVERED has a depth of zero
    # make a new INPF_AREA that is all of them combined
    ltoplot <- ltoplot %>%
      mutate(INPFC_AREA = "All districts") %>%
      bind_rows(ltoplot)

    library(mgcv)
    ltoplot$HAULJOIN <- as.factor(ltoplot$HAULJOIN)
    ltoplot$INPFC_AREA <- as.factor(ltoplot$INPFC_AREA)
    ltoplot$dummy_var <- 0

    mod1 <- gam(data = ltoplot, formula = LENGTH ~ s(BOTTOM_DEPTH, by = INPFC_AREA, k = 4) + s(HAULJOIN, bs = "re", by = dummy_var), na.action = "na.omit")
    ltoplot[c("predicted", "se")] <- predict(mod, newdata = ltoplot, se.fit = TRUE)

    ltoplot$INPFC_AREA <- factor(ltoplot$INPFC_AREA, levels = c(district_order, "All districts"))

    # color scale
    ncols <- length(unique(ltoplot$INPFC_AREA))
    pal <- c(rep("#FF773D", times = ncols - 1), "#809BCE")

    ldscatter <- ggplot(ltoplot, aes(x = BOTTOM_DEPTH / dscale, y = LENGTH / lscale)) +
      geom_point(alpha = 0.2) +
      geom_ribbon(aes(ymin = (predicted - 1.96 * se) / lscale, ymax = (predicted + 1.96 * se) / lscale, fill = INPFC_AREA), alpha = 0.2) +
      geom_line(aes(y = predicted / lscale, color = INPFC_AREA), linewidth = 1) +
      scale_color_manual(values = pal) +
      scale_fill_manual(values = pal) +
      xlab(paste("Bottom depth (x", dscale, "m)")) +
      ylab(ifelse(lscale == 10, "Length (cm)", "Length (mm)")) +
      facet_wrap(~INPFC_AREA, nrow = 1) +
      theme_light(base_size = 10) +
      theme(
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none",
        strip.background = element_blank(),
        strip.text = element_text(colour = "black")
      )

    png(filename = paste0(
      dir_out_figures, maxyr, "_",
      report_species$spp_name_informal[i], "_ldscatter.png"
    ), width = 9, height = 2, units = "in", res = 200)
    print(ldscatter)
    dev.off()

    list_ldscatter[[i]] <- ldscatter
    print(paste("Done with length by depth scatter plot of", report_species$spp_name_informal[i]))
  }
  names(list_ldscatter) <- report_species$species_code
  save(list_ldscatter, file = paste0(dir_out_figures, "list_ldscatter.rdata"))
  print("Done with length by depth scatter plots.")
}


# 6. Surface and bottom temp -----------------------------------------------

if (make_temp_plot) {
  list_temperature <- list()

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
  
  if(SRVY=="GOA"){
    sstdat <- sstdat %>% filter(YEAR != 2001) #They didn't finish the GOA survey in 2001
  }

  sst_summary <- sstdat %>%
    mutate(
      bottom_stz = bottom - mean(bottom, na.rm = T),
      surface_stz = surface - mean(surface, na.rm = T)
    ) %>%
    pivot_longer(cols = bottom:surface_stz)

  # sst_summary %>%
  #   filter(grepl("_stz", name)) %>%
  #   ggplot(aes(YEAR, value, color = name)) +
  #   geom_point(size = 2) +
  #   scale_color_manual(values = c("purple", "orange"))

  # library(ggdist)

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
  print("Done with temeprature plots.")
}



# (7.) GOA CPUE map ----------------------------------------------------------------
# The function used to generate this CPUE map is Emily's "plot_idw_xbyx()"
# THIS SHOULD ONLY BE USED FOR THE GOA - in the AI, the area is too narrow for a raster map of CPUE and we should instead be using the bubble plots of CPUE.
# get cpue table by station for a species
# sp <- 30060
# yr <- 2018
# dat2plot <- cpue_raw %>%
#   filter(survey == SRVY & species_code == sp & year == yr)
# colnames(dat2plot)
# cpue_res <- 0.1 # will take less time
# # example data:
# # head(akgfmaps::YFS2017)
#
# # This is a dummy figure! Doesn't mean anything because it's for GOA
# figure1 <- plot_idw_xbyx(
#   yrs = yr,
#   dat = dat2plot,
#   lat = "start_latitude",
#   lon = "start_longitude",
#   var = "cpue_kgkm2",
#   year = "year",
#   key.title = "POP (kg/km2)",
#   grid = "extrapolation.grid",
#   extrap.box = c(xmin = -180, xmax = -135, ymin = 52, ymax = 62),
#   grid.cell = c(cpue_res, cpue_res),
#   row0 = 1,
#   region = "goa"
# )
#
# list_figures <- list()
# list_figures[[1]] <- figure1
# names(list_figures)[1] <- "POP" # NOTE: NEED TO MAKE THIS THE WHOLE LIST OF SPECIES
#
# # Check that the figure list is filled out
# length(list_figures)
