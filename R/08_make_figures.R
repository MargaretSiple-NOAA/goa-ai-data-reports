# This script should load the necessary data and build PNGs of all the figures for the paper, so they can be loaded in the Markdown part.

# Figures & their name in the list ----------------------------------------
# Fig 1: district map
# Fig 2. CPUE map 1 "cpue_bubbles" - list_cpue_bubbles
# Fig 3. CPUE map 2 - raster maps of CPUE - ONLY FOR GOA
# Fig 4. size comp plot 2 - "joy_division_length" - list_joy_length
# Fig 5. Historical biomass plots - list_biomass_ts

make_biomass_timeseries <- TRUE
# 2. Catch composition plot
make_catch_comp <- TRUE
# 3. CPUE bubble maps - strata are shaded in. These were presented at GPT 2022.
make_cpue_bubbles_strata <- TRUE
# 3b. CPUE bubble maps, Emily M edition - strata not shown. Bubbles are purple. Scale bar and legend with CPUE scale are shown.
make_cpue_bubbles <- FALSE
# 5. Length frequency plots as joy division plots
make_joy_division_length <- TRUE
# 5b. Length vs. depth, faceted by district with GAM-predicted size by depth.
make_ldscatter <- TRUE
# 6. Plot of surface and bottom SST with long term avgs
make_temp_plot <- TRUE
# XX. Map of the full survey area with strata and stations
if (SRVY == "GOA") {
  make_total_surv_map <- TRUE
} else {
  make_total_surv_map <- FALSE
}

# in report settings, complexes will usually be set to TRUE

# Base maps ---------------------------------------------------------------
if (SRVY == "AI") {
  stratum_lookup <- read.csv("data/local_gap_products/area.csv") |>
    dplyr::filter(AREA_TYPE == "STRATUM") |>
    dplyr::select(AREA_ID, DEPTH_MAX_M)

  ai_east <- akgfmaps::get_base_layers(
    select.region = "ai.east",
    set.crs = "auto"
  ) |>
    add_depths() # stratum_lookup_tab = stratum_lookup_region

  ai_central <- akgfmaps::get_base_layers(
    select.region = "ai.central",
    set.crs = "auto"
  ) |>
    add_depths()

  ai_west <- akgfmaps::get_base_layers(
    select.region = "ai.west",
    set.crs = "auto"
  ) |>
    add_depths()

  # Get the number of strata
  a <- read.csv("data/goa_strata.csv")
  a <- dplyr::filter(a, SURVEY == "AI")
  nstrata <- length(unique(a$STRATUM))

  ndepths <- 4 # number of max depth intervals (for AI, it's 100,200,300,500)

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

if (SRVY == "GOA") {
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

  goa_nmfs_areas <- akgfmaps::get_nmfs_areas(
    set.crs = "auto"
  ) |>
    dplyr::filter(REP_AREA %in% c(610, 620, 630, 640, 650))
  geo_order2 <- district_order


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

# Aesthetic settings ------------------------------------------------------
# * Themes ----------------------------------------------------------------
bubbletheme <- theme(
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
  strip.text = element_text(size = 8, face = "bold"),
  legend.text = element_text(size = 8), ###
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
  plot.title = element_text(margin = margin(b = -30)),
  legend.position = "bottom"
)


linetheme <- theme_bw(base_size = 9)

bartheme <- ggpubr::theme_classic2(base_size = 14) +
  theme(strip.background = element_blank())

# * Palettes ----------------------------------------------------------------
if (SRVY == "AI") {
  stratumpal <- lengthen_pal(
    shortpal = RColorBrewer::brewer.pal(n = 9, name = "PuBu"),
    x = 1:nstrata
  ) |>
    colorspace::lighten(amount = 0.3, space = "HCL")
} else {
  stratumpal <- lengthen_pal(
    shortpal = RColorBrewer::brewer.pal(n = 9, name = "PuBu"),
    x = 1:nstrata
  )
}

depthpal <- lengthen_pal(x = unique(stratum_lookup$DEPTH_MAX_M), shortpal = RColorBrewer::brewer.pal(n = 9, name = "Blues")[1:7])


# Palette for lines
linecolor <- RColorBrewer::brewer.pal(n = 9, name = "Blues")[9]
accentline <- RColorBrewer::brewer.pal(n = 9, name = "Blues")[8]

# Palette for joy div plot
joypal <- c("#d1eeea", "#a8dbd9", "#85c4c9", "#68abb8", "#4f90a6", "#3b738f", "#2a5674") # Mint palette
joypal <- c("#d2fbd4", "#a5dbc2", "#7bbcb0", "#559c9e", "#3a7c89", "#235d72", "#123f5a") # more green palette
joypal <- lengthen_pal(shortpal = RColorBrewer::brewer.pal(n = 9, name = "Blues"), x = 1:nyears) # Light blues palette
joypal_grey <- grey.colors(n = 7)

# Palette for survey regions - 2 options
dispal <- c("#1d4497", "#5773c0", "#8cc8bc", "#b83326", "black")
dispal <- c("#441151", "#90be6d", "#de541e", "#a7a5c6", "#2d3047")

# Palette for species colors and fills
speciescolors <- lengthen_pal(
  shortpal = c("#dd7867", "#b83326", "#c8570d", "#edb144", "#8cc8bc", "#7da7ea", "#5773c0", "#1d4497"),
  x = 1:(nrow(report_species) + 1)
)

# 0. Static figure: INPFC areas ----------------------------------------------

# This figure is loaded in knit_report; it is static.

# 0b: make_total_surv_map: INPFC areas with stations sampled -----------------------------------
if (make_total_surv_map) {
  # old colors
  # palette_map <- c("#dd7867", "#8cc8bc", "#b83326", "#5773c0", "#c8570d")
  # alternate
  # palette_map <- c("#4E79A7", "#A0CBE8", "#F28E2B", "#59A14F", "#F1CE63")
  # new colors
  palette_map <- c("#4E79A7", "#A0CBE8", "#F28E2B", "#FFBE7D", "#59A14F")

  #  Base map
  # p1 <- ggplot() +
  #   geom_sf(data = goa_all$akland) +
  #   geom_sf(data = goa_inpfc, aes(fill = AREA_NAME)) + #used to be INPFC_STRATUM - 2x check this later
  #   scale_fill_manual("INPFC area", values = palette_map, breaks = geo_order) +
  #   geom_sf(data = goa_all$survey.area, fill = NA) +
  #   geom_sf_text(
  #     data = goa_inpfc, size = 4,
  #     aes(label = AREA_NAME, geometry = geometry),
  #     nudge_y = -240000
  #   ) +
  #   theme_light() +
  #   theme(legend.position = "none")

  #  Base map 2 - for new GOA survey design
  p1 <- ggplot() +
    geom_sf(data = goa_all$akland) +
    geom_sf(data = goa_nmfs_areas, aes(fill = REP_AREA)) +
    scale_fill_manual("NMFS area",
      values = palette_map,
      breaks = c(610, 620, 630, 640, 650),
      labels = geo_order2
    ) +
    geom_sf(data = goa_all$survey.area, fill = NA) +
    geom_sf_text(
      data = goa_nmfs_areas, size = 4,
      aes(label = geo_order2, geometry = geometry),
      nudge_y = -140000
    ) +
    theme_light() +
    theme(legend.position = "none")

  # Where we sampled
  thisyrshauldata <- cpue_processed |>
    dplyr::filter(year == maxyr & survey == SRVY) |>
    st_as_sf(
      coords = c("longitude_dd_start", "latitude_dd_start"),
      crs = "EPSG:4326"
    ) |>
    st_transform(crs = reg_data$crs)


  station_map1 <- p1 +
    geom_sf(data = thisyrshauldata, size = 0.5) +
    coord_sf(
      xlim = goa_all$plot.boundary$x,
      ylim = c(goa_all$plot.boundary$y[1] - 250000, goa_all$plot.boundary$y[2])
    ) +
    guides(fill = guide_legend(nrow = 2, byrow = TRUE)) +
    ggspatial::annotation_scale(location = "br") +
    theme(panel.grid = element_line(color = "#FAFAFA")) +
    xlab("Longitude") +
    ylab("Latitude")

  # inset map
  world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")
  inset_map <- ggplot(data = world) +
    geom_sf(color = "darkgrey", fill = "darkgrey") +
    xlim(-170, -50) +
    ylim(20, 80) +
    annotate(
      "rect",
      xmin = -160,
      xmax = -130,
      ymin = 55,
      ymax = 62,
      color = "darkblue", fill = NA, lwd = 0.5
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      panel.border = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),

      # border (for inset plotting)
      plot.background = element_rect(
        fill = "white", colour = "black",
        linewidth = 0.5
      )
    )

  station_map <- station_map1 +
    inset_element(inset_map, -0.78, 0.5, 1, 1, align_to = "panel")

  png(
    filename = paste0(dir_out_figures, maxyr, "_station_map.png"),
    width = 8, height = 6, units = "in", res = 150
  )
  print(station_map)
  dev.off()

  save(station_map, file = paste0(dir_out_srvy_yr, "figures/", maxyr, "_station_map.RDS"))
}

# 1. Biomass index relative to LT mean ---------------------------------------
if (make_biomass_timeseries) {
  list_biomass_ts <- list()
  list_3panel_ts <- list()

  for (i in 1:nrow(report_species)) { #
    sp <- report_species$species_code[i]
    name_bms <- report_species$spp_name_informal[i]

    dat <- biomass_total |>
      dplyr::filter(SPECIES_CODE == report_species$species_code[i]) |>
      dplyr::mutate(PERCENT_OF_STATIONS = round((N_WEIGHT / N_HAUL) * 100)) |>
      dplyr::mutate(PERCENT_CHANGE_BIOMASS = round((BIOMASS_MT - lag(BIOMASS_MT, default = first(BIOMASS_MT))) / lag(BIOMASS_MT, default = first(BIOMASS_MT)) * 100))

    dat$PERCENT_CHANGE_BIOMASS[1] <- NA # no difference calculated for first year of ts
    
    # if the species has a start year after the start of the survey, filter to after that
    if(sp %in% species_year$SPECIES_CODE){
      dat <- dat |> 
        dplyr::filter(YEAR > species_year$YEAR_STARTED[which(species_year$SPECIES_CODE == sp)])
    }

    lta_biomass <- mean(dat$BIOMASS_MT)
    lta_percent_stns <- mean(dat$PERCENT_OF_STATIONS)
    lta_percent_change <- mean(dat$PERCENT_CHANGE_BIOMASS, na.rm = TRUE)

    p1 <- dat |>
      ggplot(aes(x = YEAR, y = BIOMASS_MT)) +
      geom_hline(yintercept = lta_biomass, color = accentline, lwd = 0.7, lty = 2) +
      geom_point(color = linecolor, size = 2) +
      geom_errorbar(aes(ymin = MIN_BIOMASS, ymax = MAX_BIOMASS),
        color = linecolor, linewidth = 0.9, width = 0.7
      ) +
      ylab("Estimated total \nbiomass (mt)") +
      xlab("Year") +
      scale_y_continuous(labels = scales::label_comma()) +
      linetheme +
      scale_x_continuous(limits = c(minyr, maxyr), breaks = pretty_breaks(n = 3))
    p1

    p2 <- dat |>
      ggplot(aes(x = YEAR, y = PERCENT_OF_STATIONS)) +
      geom_point(color = linecolor, size = 2) +
      geom_hline(
        yintercept = lta_percent_stns,
        color = accentline, lwd = 0.7, lty = 2
      ) +
      xlab("Year") +
      ylab("Proportion of hauls \nwhere present (%)") +
      linetheme +
      scale_x_continuous(limits = c(minyr, maxyr), breaks = pretty_breaks(n = 3))

    p3 <- ggplot() +
      geom_hline(
        yintercept = lta_percent_change,
        color = accentline, lwd = 0.7, lty = 2
      ) +
      geom_point(
        data = subset(dat, abs(PERCENT_CHANGE_BIOMASS) < 50),
        aes(x = YEAR, y = PERCENT_CHANGE_BIOMASS, color = PERCENT_CHANGE_BIOMASS),
        size = 2
      ) +
      geom_point(
        data = subset(dat, PERCENT_CHANGE_BIOMASS > 50),
        aes(x = YEAR, y = PERCENT_CHANGE_BIOMASS),
        color = "#276419",
        size = 2
      ) +
      geom_point(
        data = subset(dat, PERCENT_CHANGE_BIOMASS < (-50)),
        aes(x = YEAR, y = PERCENT_CHANGE_BIOMASS),
        color = "#8e0152",
        size = 2
      ) +
      # scale_color_distiller(palette = "PiYG", type = "div") +
      scale_color_gradientn(colors = c("#8e0152", "#f7f7f7", "#276419")) +
      xlab("Year") +
      ylab("Percent change in biomass \nfrom previous survey (%)") +
      linetheme +
      theme(legend.position = "none") +
      scale_x_continuous(limits = c(minyr, maxyr), breaks = pretty_breaks(n = 3))

    # p1 + p2 + p3

    # If needed: make plot of CPUE distribution where present
    # dat_cpue <- cpue_processed |>
    #   dplyr::filter(species_code == sp & cpue_kgkm2>0)
    #
    # p4 <- dat_cpue |>
    #   ggplot(aes(x=year, y = cpue_kgkm2)) +
    #   geom_jitter(alpha=0.2,size=2) +
    #   linetheme +
    #   xlab("Year") +
    #   ylab(bquote(CPUE~~where~~present~~(kg / km^2)))

    # Save just time series
    list_biomass_ts[[i]] <- p1
    names(list_biomass_ts)[[i]] <- report_species$species_code[i]

    png(
      filename = paste0(dir_out_figures, maxyr, "_", name_bms, "_biomass_ts.png"),
      width = 7, height = 7, units = "in", res = 200
    )
    print(p1)
    dev.off()

    list_3panel_ts[[i]] <- p1 + p2 + p3
    names(list_3panel_ts)[[i]] <- report_species$species_code[i]

    # Save 3-panel figs
    png(
      filename = paste0(dir_out_figures, maxyr, "_", name_bms, "_biomass_3panel_ts.png"),
      width = 6.5, height = 2, units = "in", res = 200
    )
    print(p1 + p2 + p3)
    dev.off()

    ## Save 4-panel figs - not set up yet to save them all as a list in a data file
    # png(
    #   filename = paste0(dir_out_figures, name_bms, "_", YEAR, "_biomass_4panel_ts.png"),
    #   width = 8, height = 8, units = "in", res = 200
    # )
    # print(p1 + p2 + p3 + p4)
    # dev.off()
  } # /end species loop
  # names(list_biomass_ts) <- report_species$species_code
  save(list_biomass_ts, file = paste0(dir_out_figures, "biomass_ts.rdata"))
  save(list_3panel_ts, file = paste0(dir_out_figures, "list_3panel_ts.rdata"))
  print("Done making biomass time series plots.")
}


# 2. Catch composition -------------------------------------------------------
if (make_catch_comp) {
  head(biomass_total)
  biomass_total_filtered <- biomass_total |>
    left_join(report_species,
      by = c("SPECIES_CODE" = "species_code")
    ) %>%
    mutate(spp_name_informal = tidyr::replace_na(
      data = spp_name_informal,
      replace = "Other species"
    ))

  biomass_total_filtered$spp_name_informal <- factor(biomass_total_filtered$spp_name_informal,
    levels = c(report_species$spp_name_informal, "Other species")
  )

  p2 <- biomass_total_filtered |>
    ggplot(aes(fill = spp_name_informal, y = BIOMASS_MT / 10e6, x = YEAR)) +
    geom_bar(position = "stack", stat = "identity") +
    scale_fill_manual("", values = speciescolors) +
    xlab("Year") +
    ylab(expression(paste("Total estimated biomass (\u00D7 ", 10^6, " mt)"))) +
    scale_y_continuous(expand = c(0, 0)) +
    bartheme +
    theme(legend.position = "bottom")

  png(
    filename = paste0(dir_out_figures, YEAR, "_biomass_catchcomp.png"),
    width = 11, height = 6, units = "in", res = 200
  )
  print(p2)
  dev.off()
}

# 3. CPUE maps - depths colored in (presented at GPT 2022 and 2024 with strata colored in) ----------------------------------------------------------
if (make_cpue_bubbles_strata) { # / end make stratum bubble figs

  # * * COMPLEXES ----------
  list_cpue_bubbles_strata_complexes <- list()

  cpue_complexes <- cpue_processed |>
    filter(grepl(species_code, pattern = "[A-Za-z]"))

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


    thisyrshauldata <- cpue_complexes |>
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
        scale_size(bquote("CPUE" ~ (kg / km^2)),
          limits = c(1, max(thisyrshauldata$cpue_kgkm2)),
          guide = "legend"
        ) +
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

    thisyrshauldata <- cpue_processed |>
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
        scale_size(bquote("CPUE" ~ (kg / km^2)),
          limits = c(1, max(thisyrshauldata$cpue_kgkm2)),
          guide = "legend"
        ) +
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

# 5. Length frequency - joy division plots ---------------------------------
if (make_joy_division_length) {
  list_joy_length <- list()
  if (file.exists(paste0(dir_out_srvy_yr, "tables/report_pseudolengths.csv"))) {
    report_pseudolengths <- read.csv(paste0(dir_out_srvy_yr, "tables/report_pseudolengths.csv"))
  } else {
    cat("Pseudolength file not found. Return to the 06_prep_data.R file and create it again.")
  }

  # This is repeated; deal with it later
  L0 <- read.csv(here::here("data/local_racebase/length.csv"))

  L3 <- L0 |>
    dplyr::mutate(YEAR = as.numeric(gsub("(^\\d{4}).*", "\\1", CRUISE))) |> # L is the big length table from RACEBASE
    dplyr::filter(REGION == SRVY) |> # want to keep all years for this fig
    left_join(haul2, by = c(
      "HAULJOIN", "YEAR", "CRUISEJOIN",
      "VESSEL", "CRUISE", "HAUL"
    )) |>
    dplyr::select(
      VESSEL, YEAR, LENGTH, FREQUENCY, SEX,
      GEAR_DEPTH, STRATUM, SPECIES_CODE
    ) |>
    dplyr::left_join(region_lu, by = "STRATUM") |>
    mutate(Sex = case_when(
      SEX == 1 ~ "Male",
      SEX == 2 ~ "Female",
      SEX == 3 ~ "Unsexed"
    )) |>
    dplyr::select(-SEX, -MIN_DEPTH, -MAX_DEPTH)

  # get samples for individual species
  species_sample_sizes <- L3 |>
    dplyr::filter(grepl("[0-9]", SPECIES_CODE)) |> # filter to not complexes
    dplyr::filter(SPECIES_CODE %in% (report_species$species_code)) |> # filter to report species
    dplyr::filter(YEAR >= minyr) |>
    dplyr::group_by(YEAR, SPECIES_CODE, Sex) |>
    dplyr::summarize(n = sum(FREQUENCY)) |>
    ungroup() |>
    mutate(
      YEAR = as.integer(YEAR),
      SPECIES_CODE = as.character(SPECIES_CODE)
    )

  # get sample sizes for complexes
  complex_sample_sizes <- L3 |>
    dplyr::filter(SPECIES_CODE %in% complex_lookup$species_code) |>
    dplyr::filter(YEAR >= minyr) |>
    dplyr::mutate(SPECIES_CODE = case_when(SPECIES_CODE %in% complex_lookup$species_code[which(complex_lookup$complex == "OROX")] ~ "OROX",
      SPECIES_CODE %in% complex_lookup$species_code[which(complex_lookup$complex == "REBS")] ~ "REBS",
      SPECIES_CODE %in% complex_lookup$species_code[which(complex_lookup$complex == "OFLATS")] ~ "OFLATS",
      SPECIES_CODE %in% complex_lookup$species_code[which(complex_lookup$complex == "DEEPFLATS")] ~ "DEEPFLATS",
      SPECIES_CODE %in% complex_lookup$species_code[which(complex_lookup$complex == "DSROX")] ~ "DSROX",
      SPECIES_CODE %in% complex_lookup$species_code[which(complex_lookup$complex == "NRSSRS")] ~ "NRSSRS",
      SPECIES_CODE %in% complex_lookup$species_code[which(complex_lookup$complex == "SWFLATS")] ~ "SWFLATS",
      SPECIES_CODE %in% complex_lookup$species_code[which(complex_lookup$complex == "SHARKS")] ~ "SHARKS",
      SPECIES_CODE %in% complex_lookup$species_code[which(complex_lookup$complex == "SKATES")] ~ "SKATES",
      SPECIES_CODE %in% complex_lookup$species_code[which(complex_lookup$complex == "THORNYHEADS")] ~ "THORNYHEADS",
      .default = as.character(SPECIES_CODE)
    )) |>
    dplyr::mutate(SPECIES_CODE = as.character(SPECIES_CODE)) |>
    dplyr::group_by(YEAR, SPECIES_CODE, Sex) |>
    dplyr::summarize(n = sum(FREQUENCY)) |>
    ungroup() |>
    mutate(YEAR = as.integer(YEAR))

  sample_sizes <- bind_rows(species_sample_sizes, complex_sample_sizes)
  left_labels <- c(30420, 30152) # species for which you want the label on the left instead of the right!

  # Loop thru species
  for (i in 1:nrow(report_species)) { #
    len2plot <- report_pseudolengths |>
      filter(SPECIES_CODE == report_species$species_code[i])
    if (nrow(len2plot) == 0) {
      stop("pseudolength (expanded lengths) data missing for this species. Fix it!")
    }
    # Subset to years when species was confidently ID'ed
    if (report_species$species_code[i] %in% species_year$SPECIES_CODE) {
      len2plot <- len2plot |>
        filter(YEAR >= species_year$YEAR[which(species_year$SPECIES_CODE == report_species$species_code[i])])
    }

    # SSTH, YIL, and darkfin sculpin only show Unsexed; all other spps show only sexed lengths
    if (report_species$species_code[i] %in% c(30020, 21341, 21347, "THORNYHEADS")) {
      len2plot <- len2plot
    } else {
      len2plot <- len2plot |>
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
        mapping = aes(
          label = paste0("n = ", n),
          x = ifelse(report_species$species_code[i] %in% left_labels, -Inf, Inf)
        ),
        fill = "white", linewidth = NA,
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

    png(filename = paste0(
      dir_out_figures,
      maxyr, "_", report_species$spp_name_informal[i], "_joyfreqhist.png"
    ), width = 7, height = 5, units = "in", res = 200)
    print(joyplot)
    dev.off()

    list_joy_length[[i]] <- joyplot
  }
  names(list_joy_length) <- report_species$species_code

  save(list_joy_length, file = paste0(dir_out_figures, "list_joy_length.rdata"))

  rm(list = c("L0", "L3", "joyplot", "len2plot2", "len2plot"))
  print("Done with joy division plots for length comp.")
}


# 5b. Length by depth scatterplot with GAM -------------------------------------
if (make_ldscatter) {
  lscale <- 10
  dscale <- 100

  length_maxyr_ldscatter <- length_maxyr |>
    dplyr::mutate(SPECIES_CODE = as.character(SPECIES_CODE)) |>
    dplyr::bind_rows(length_maxyr_complexes)

  list_ldscatter <- list()

  for (i in 1:nrow(report_species)) {
    if (report_species$species_code[i] == 78403) {
      ldscatter <- ggplot() +
        theme_void()
    } else {
      if (SRVY == "GOA" & maxyr >= 2025) {
        ltoplot0 <- length_maxyr_ldscatter %>%
          dplyr::filter(SPECIES_CODE == report_species$species_code[i]) %>%
          dplyr::left_join(haul2, by = c(
            "CRUISEJOIN", "HAULJOIN", "HAUL",
            "REGION", "VESSEL", "CRUISE"
          )) %>%
          dplyr::left_join(stratum_lu, by = "STRATUM") %>%
          dplyr::filter(ABUNDANCE_HAUL == "Y") %>%
          dplyr::filter(HAULJOIN != -21810) # take out haul 191 from OEX 2022 which i JUST DISCOVERED has a depth of zero
        # make a new INPFC_AREA that is all of them combined
        ltoplot <- ltoplot0 %>%
          mutate(REGULATORY_AREA_NAME = "All districts") %>%
          bind_rows(ltoplot0)

        ltoplot$HAULJOIN <- as.factor(ltoplot$HAULJOIN)
        ltoplot$REGULATORY_AREA_NAME <- as.factor(ltoplot$REGULATORY_AREA_NAME)
        ltoplot$dummy_var <- 0

        nknots <- 4

        if (nrow(ltoplot) < nknots * 2) {
          ldscatter <- ggplot(ltoplot, aes(x = BOTTOM_DEPTH / dscale, y = LENGTH / lscale)) +
            geom_point(alpha = 0.2, size = 0.8) +
            xlab(paste("Bottom depth (x", dscale, "m)")) +
            ylab(ifelse(lscale == 10, "Length (cm)", "Length (mm)")) +
            facet_wrap(~REGULATORY_AREA_NAME, nrow = 1) +
            theme_light(base_size = 10) +
            theme(
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              legend.position = "none",
              strip.background = element_blank(),
              strip.text = element_text(colour = "black")
            )
          cat("NOTE: Not enough data to fit a GAM for", report_species$spp_name_informal[i], "- saving just the scatterplot \n")
        } else {
          mod1 <- mgcv::gam(data = ltoplot, formula = LENGTH ~ s(BOTTOM_DEPTH, by = REGULATORY_AREA_NAME, k = nknots) + s(HAULJOIN, bs = "re", by = dummy_var), na.action = "na.omit")

          ltoplot[c("predicted", "se")] <- stats::predict(mod1, newdata = ltoplot, se.fit = TRUE)

          ltoplot$REGULATORY_AREA_NAME <- factor(ltoplot$REGULATORY_AREA_NAME, levels = c(district_order, "All districts"))

          # color scale
          ncols <- length(unique(ltoplot$REGULATORY_AREA_NAME))
          pal <- c(rep("#FF773D", times = ncols - 1), "#809BCE")

          ldscatter <- ggplot(ltoplot, aes(x = BOTTOM_DEPTH / dscale, y = LENGTH / lscale)) +
            geom_point(alpha = 0.2, size = 0.8) +
            geom_ribbon(aes(ymin = (predicted - 1.96 * se) / lscale, ymax = (predicted + 1.96 * se) / lscale, fill = REGULATORY_AREA_NAME), alpha = 0.2) +
            geom_line(aes(y = predicted / lscale, color = REGULATORY_AREA_NAME), linewidth = 1) +
            scale_color_manual(values = pal) +
            scale_fill_manual(values = pal) +
            xlab(paste("Bottom depth (x", dscale, "m)")) +
            ylab(ifelse(lscale == 10, "Length (cm)", "Length (mm)")) +
            facet_wrap(~REGULATORY_AREA_NAME, nrow = 1) +
            theme_light(base_size = 10) +
            theme(
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              legend.position = "none",
              strip.background = element_blank(),
              strip.text = element_text(colour = "black")
            )
        }
      } else { # if survey is AI or GOA before redesign
        ltoplot0 <- length_maxyr_ldscatter %>%
          dplyr::filter(SPECIES_CODE == report_species$species_code[i]) %>%
          dplyr::left_join(haul2, by = c(
            "CRUISEJOIN", "HAULJOIN", "HAUL",
            "REGION", "VESSEL", "CRUISE"
          )) %>%
          dplyr::left_join(region_lu, by = "STRATUM") %>%
          dplyr::filter(ABUNDANCE_HAUL == "Y") %>%
          dplyr::filter(HAULJOIN != -21810) # take out haul 191 from OEX 2022 which i JUST DISCOVERED has a depth of zero
        # make a new INPFC_AREA that is all of them combined
        ltoplot <- ltoplot0 %>%
          mutate(INPFC_AREA = "All districts") %>%
          bind_rows(ltoplot0)

        ltoplot$HAULJOIN <- as.factor(ltoplot$HAULJOIN)
        ltoplot$INPFC_AREA <- as.factor(ltoplot$INPFC_AREA)
        ltoplot$dummy_var <- 0

        nknots <- 4

        if (nrow(ltoplot) < nknots * 2) {
          ldscatter <- ggplot(ltoplot, aes(x = BOTTOM_DEPTH / dscale, y = LENGTH / lscale)) +
            geom_point(alpha = 0.2, size = 0.8) +
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
          cat("NOTE: Not enough data to fit a GAM for", report_species$spp_name_informal[i], "- saving just the scatterplot \n")
        } else {
          mod1 <- mgcv::gam(data = ltoplot, formula = LENGTH ~ s(BOTTOM_DEPTH, by = INPFC_AREA, k = nknots) + s(HAULJOIN, bs = "re", by = dummy_var), na.action = "na.omit")

          ltoplot[c("predicted", "se")] <- stats::predict(mod1, newdata = ltoplot, se.fit = TRUE)

          ltoplot$INPFC_AREA <- factor(ltoplot$INPFC_AREA, levels = c(district_order, "All districts"))

          # color scale
          ncols <- length(unique(ltoplot$INPFC_AREA))
          pal <- c(rep("#FF773D", times = ncols - 1), "#809BCE")

          ldscatter <- ggplot(ltoplot, aes(x = BOTTOM_DEPTH / dscale, y = LENGTH / lscale)) +
            geom_point(alpha = 0.2, size = 0.8) +
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
        }
      } # end if statement for AI/old GOA
    }
    png(filename = paste0(
      dir_out_figures, maxyr, "_", report_species$spp_name_informal[i],
      "_ldscatter.png"
    ), width = 9, height = 2, units = "in", res = 200)
    print(ldscatter)
    dev.off()

    list_ldscatter[[i]] <- ldscatter
    print(paste("Done with length by depth scatter plot of", report_species$spp_name_informal[i]))
  }

  names(list_ldscatter) <- report_species$species_code
  save(list_ldscatter, file = paste0(dir_out_figures, "list_ldscatter.rdata"))
  print("Done with length by depth scatter plots.")
} # end ldscatter plot making

# 6. Surface and bottom temp -----------------------------------------------
if (make_temp_plot) {
  list_temperature <- list()

  sstdat <- haul |>
    mutate(YEAR = stringr::str_extract(CRUISE, "^\\d{4}")) |>
    filter(YEAR >= 1994 & REGION == SRVY & YEAR != 1997) |>
    filter(PERFORMANCE >= 0) |>
    group_by(YEAR) |>
    dplyr::summarize(
      bottom = mean(GEAR_TEMPERATURE, na.rm = TRUE),
      surface = mean(SURFACE_TEMPERATURE, na.rm = TRUE)
    ) |>
    ungroup() |>
    as.data.frame() |>
    mutate(YEAR = as.numeric(YEAR))

  if (SRVY == "GOA") {
    sstdat <- sstdat |> filter(YEAR != 2001) # They didn't finish the GOA survey in 2001
  }

  sst_summary <- sstdat |>
    mutate(
      bottom_stz = bottom - mean(bottom, na.rm = T),
      surface_stz = surface - mean(surface, na.rm = T)
    ) |>
    pivot_longer(cols = bottom:surface_stz)

  plotdat <- haul |>
    mutate(YEAR = as.numeric(stringr::str_extract(CRUISE, "^\\d{4}"))) |>
    filter(PERFORMANCE >= 0) |>
    filter(REGION == SRVY & YEAR != 1997) |> # YEAR >= 1994 &
    filter(CRUISE != 201402) |> # remove study from Makushin bay in 2014 (contains a zero BT)
    filter(HAULJOIN != -17737) # Filter out the situation with BT=0 in 2018

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

  bottom_temp_plot <- plotdat |>
    ggplot(aes(y = GEAR_TEMPERATURE, x = YEAR)) +
    ggdist::stat_interval(linewidth = 3) +
    ggdist::stat_halfeye(
      fill = "tan", alpha = 0.5,
      interval_color = "grey27", point_color = "grey27"
    ) +
    rcartocolor::scale_color_carto_d("Quantile", palette = "Peach") +
    scale_fill_ramp_discrete(na.translate = FALSE) +
    xlim(c(minyr, maxyr)) +
    labs(x = "Year", y = expression("Bottom temperature "(degree * C))) + #
    theme_light(base_size = 12) +
    geom_segment(data = bottom_temp_avgs, aes(
      y = Value, yend = Value,
      linetype = Average,
      x = Start_year, xend = maxyr
    ))

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
    # geom_point(size = 0.5, color = "gray5") +
    rcartocolor::scale_color_carto_d("Quantile", palette = "Peach") +
    scale_fill_ramp_discrete(na.translate = FALSE) +
    xlim(c(minyr, maxyr)) +
    labs(x = "Year", y = expression("Surface temperature "(degree * C))) +
    theme_light(base_size = 12) +
    geom_segment(data = surface_temp_avgs, aes(
      y = Value, yend = Value,
      linetype = Average,
      x = Start_year, xend = maxyr
    ))

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
}
