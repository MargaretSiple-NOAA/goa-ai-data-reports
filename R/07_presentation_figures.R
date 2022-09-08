
# PRESENTATION FIGURES ----------------------------------------------------
# RUN THE FIRST 70 LINES OF THE 01_START_HERE.R SCRIPT BEFORE RUNNING THE CODE BELOW
#
# Table of contents (toggle true/false to make some plots but not others):
# 1. Biomass indices relative to LT mean
make_biomass_timeseries <- FALSE
# 2. Catch composition plot
make_catch_comp <- TRUE
# 3. CPUE bubble maps
make_cpue_bubbles <- FALSE
# 4. Length frequency plots by region and depth stratum
make_length_freqs <- TRUE

# Libraries ---------------------------------------------------------------
library(akgfmaps)
library(patchwork)

# Data to plot ------------------------------------------------------------
# All the species for which we want to make plots
head(report_species)

# Total biomass data (currently taking from local copy; download/update new one in 01_start_here.R)
biomass_total <- read.csv("data/local_ai/biomass_total.csv")

# Base maps ---------------------------------------------------------------
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
bartheme <- theme_classic2(base_size = 16)

# Palettes!
# Ghibli Ponyo palette
# stratumpal <- lengthen_pal(
#   shortpal = ghibli::ghibli_palette("PonyoLight"),
#   x = 1:nstrata
# )
# ColorBrewer pastels
# stratumpal <- lengthen_pal(
#   shortpal = RColorBrewer::brewer.pal(n = 6,name = "Pastel1"),
#   x = 1:nstrata
# )
# MetBrewer (dark colors)
stratumpal <- lengthen_pal(
  shortpal = MetBrewer::met.brewer(name = "Hokusai1", type = "continuous"),
  x = 1:nstrata
)
# MetBrewer dark colors, lightened
# stratumpal <- colorspace::lighten(lengthen_pal(
#   shortpal = MetBrewer::met.brewer(name = "Hokusai1",type = "continuous"),
#                                 x = 1:nstrata),amount = 0.6) #


# Palette for lines
linecolor <- RColorBrewer::brewer.pal(n = 9, name = "Blues")[9]
accentline <- RColorBrewer::brewer.pal(n = 9, name = "Blues")[8]

# Palette for species colors and fills
speciescolors <- nmfspalette::nmfs_palette("regional web")(nrow(report_species) + 1)
speciescolors <- lengthen_pal(
  shortpal = MetBrewer::met.brewer(name = "VanGogh2", type = "discrete", direction = -1),
  x = 1:(nrow(report_species) + 1)
)
# 1. Biomass index relative to LT mean ---------------------------------------

if (make_biomass_timeseries) {
  for (i in 1:nrow(report_species)) {
    sp <- report_species$species_code[i]
    name_bms <- report_species$spp_name_informal[i]

    dat <- biomass_total %>%
      filter(SPECIES_CODE == report_species$species_code[i])
    lta <- mean(dat$TOTAL_BIOMASS)

    p1 <- dat %>%
      ggplot(aes(x = YEAR, y = TOTAL_BIOMASS)) +
      geom_hline(yintercept = lta, color = accentline, lwd = 1, lty = 2) +
      geom_point(color = linecolor, size = 2) +
      geom_line(color = linecolor, lwd = 1) +
      geom_ribbon(aes(ymin = MIN_BIOMASS, ymax = MAX_BIOMASS), alpha = 0.2, fill = linecolor, color = NA) +
      ylab("Estimated total biomass (mt)") +
      xlab("Year") +
      labs(title = paste0(name_bms)) +
      scale_y_continuous(labels = scales::label_comma()) +
      linetheme
    png(
      filename = paste0(dir_out_figures, name_bms, "_", YEAR, "_biomass_ts.png"),
      width = 7, height = 7, units = "in", res = 150
    )
    print(p1)
    dev.off()
  }
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

  p1 <- biomass_total_filtered %>%
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
  print(p1)
  dev.off()
}

# 3. CPUE bubble maps ----------------------------------------------------------
if (make_cpue_bubbles) {
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
    p1 <- ggplot() +
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
      coord_sf(
        xlim = ai_east$plot.boundary$x,
        ylim = ai_east$plot.boundary$y
      ) +
      scale_x_continuous(breaks = ai_east$lon.breaks) +
      scale_y_continuous(breaks = ai_east$lat.breaks) +
      labs(subtitle = "Eastern Aleutians") +
      bubbletheme

    p2 <- ggplot() +
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
      labs(subtitle = paste0(namebubble, " - Central Aleutians - ", YEAR)) +
      bubbletheme

    p3 <- ggplot() +
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
      labs(subtitle = "Western Aleutians") +
      bubbletheme

    final_obj <- p2 / (p3 | p1)

    png(
      filename = paste0(dir_out_figures, namebubble, "_2018_bubble_example_Hokusai.png"),
      width = 10, height = 9, units = "in", res = 150
    )
    print(final_obj)
    # ggsave(filename = paste0(dir_out_figures, namebubble, "_2018_bubble_example_Hokusai.png"), plot = final_obj,width = 10,height = 9,units = "in")
    dev.off()
  }
}
