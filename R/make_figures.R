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
# 3. CPUE bubble maps
make_cpue_bubbles <- TRUE
# 5. Length frequency plots as joy division plots
make_joy_division_length <- TRUE

# DO you want to load the whole Windows font library? This is useful if you are building all the figures for the first time.
full_font_import <- TRUE

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


linetheme <- theme_bw(base_size = 12)
bartheme <- theme_classic2(base_size = 12) +
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
#joypal <- lengthen_pal(shortpal = RColorBrewer::brewer.pal(n = 9, name = "Blues"), x = 1:nyears)
joypal <- c("#d1eeea","#a8dbd9","#85c4c9","#68abb8","#4f90a6","#3b738f","#2a5674") # Mint palette
joypal <- c("#d2fbd4", "#a5dbc2", "#7bbcb0", "#559c9e", "#3a7c89", "#235d72", "#123f5a")
 # more green palette


# Palette for species colors and fills
# speciescolors <- nmfspalette::nmfs_palette("regional web")(nrow(report_species) + 1)
speciescolors <- lengthen_pal(
  shortpal = MetBrewer::met.brewer(palette_name = "VanGogh2", type = "discrete", direction = -1),
  x = 1:(nrow(report_species) + 1)
)


# Fonts -------------------------------------------------------------------
#windowsFonts("Montserrat" = windowsFont("Montserrat"))
if(full_font_import){
  extrafont::font_import()
  loadfonts(device = "win")
}

# 0. Static figure: INPFC areas ----------------------------------------------
#**** TODO: Load png or whatever and put it here. Can I store it in a list?
# Needed to use magick pkg to convert the image to a png, for some reason the usual image reader in Windows does not actually convert the format.
# error_file = magick::image_read("img/AleutiansMap.png")
# right_png <- magick::image_convert(image = error_file, "png")
# magick::image_write(right_png, path = "img/AleutiansMap.png", format = "png")

img1_path <- "img/AleutiansMap.png"
img1 <- png::readPNG(img1_path)
# attr(img1, "info")

# 1. Biomass index relative to LT mean ---------------------------------------

if (make_biomass_timeseries) {
  list_biomass_ts <- list()
  for (i in 1: nrow(report_species)) { #
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
        color = linecolor, size = 0.9, width = 0.7
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

# 3. CPUE bubble maps - FIXXXXXXXXXXXXXXXXXX ----------------------------------------------------------
if (make_cpue_bubbles) {
   list_cpue_bubbles <- list()
  for (i in 1:nrow(report_species)) {
    spbubble <- report_species$species_code[i]
    namebubble <- report_species$spp_name_informal[i]

    # CPUE data
    # thisyrshauldata <- cpue_raw2 %>%
    #   filter(year == maxyr & srvy == SRVY & species_code == spbubble) %>%
    #   st_as_sf(
    #     coords = c("longitude_dd", "latitude_dd"), #TODO NEED TO CHANGE TO THE RIGHT COORDS
    #     crs = "EPSG:4326"
    #   ) %>%
    #   st_transform(crs = ai_east$crs)

    thisyrshauldata <- cpue_raw %>%
      filter(year == maxyr & survey == SRVY & species_code == spbubble) %>%
      st_as_sf(
        coords = c("start_longitude", "start_latitude"), #TODO NEED TO CHANGE TO THE RIGHT COORDS
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
      geom_sf(data = thisyrshauldata, aes(size = cpue_kgkm2 ), alpha = 0.5) + # USED TO BE cpue_kgha
      scale_size(limits = c(0, max(thisyrshauldata$cpue_kgkm2 ))) +
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
      geom_sf(data = thisyrshauldata, aes(size = cpue_kgkm2 ), alpha = 0.5) +
      scale_size(limits = c(0, max(thisyrshauldata$cpue_kgkm2 ))) +
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
      geom_sf(data = thisyrshauldata, aes(size = cpue_kgkm2 ), alpha = 0.5) +
      scale_size(limits = c(0, max(thisyrshauldata$cpue_kgkm2 ))) +
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

    list_cpue_bubbles[[i]] <- final_obj # save fig to list
  } # /end species loop
  names(list_cpue_bubbles) <- report_species$species_code
}


# 5. Length frequency plots - joy division plots -----------------------------

if (make_joy_division_length) {
  list_joy_length <- list()

  # length <- read.csv(here::here("data", "local_racebase", "length.csv"))

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
  for (i in 1:nrow(report_species)) {
    length3_species <- length3 %>%
      filter(SPECIES_CODE == report_species$species_code[i])

    if (report_species$species_code[i] != 30060) {
      length3_species <- length3_species %>%
        filter(Sex != "Unsexed")
    }
    length3_species <- length3_species %>%
      left_join(length3_species %>% dplyr::count(YEAR, Sex)) %>%
      left_join(length3_species %>% dplyr::group_by(Sex) %>% dplyr::summarize(yloc = median(LENGTH) * 2) %>% ungroup())

    joyplot <- length3_species %>%
      ggplot(aes(x = LENGTH, y = YEAR, group = YEAR, fill = ..x..)) +
      geom_density_ridges_gradient() +
      scale_y_discrete(limits = rev) +
      geom_text(aes(label = paste0("n = ",n),x=yloc),nudge_y = 0.5,colour = "grey35", size = 3) +
      facet_grid(~Sex) +
      xlab("Length (mm)") +
      ylab("Year") +
      theme_ridges(font_size = 11) +
      scale_fill_gradientn("Length (mm)", colours = joypal) +
      labs(title = paste(report_species$spp_name_informal[i])) +
      theme(strip.background = element_blank())
joyplot
    
    png(filename = paste0(
      dir_out_figures, maxyr, "_",
      report_species$spp_name_informal[i], "_joyfreqhist.png"
    ), width = 7, height = 7, units = "in", res = 200)
    print(joyplot)
    dev.off()

    list_joy_length[[i]] <- joyplot
  }
  names(list_joy_length) <- report_species$species_code
}

# CPUE map ----------------------------------------------------------------
# The function used to generate this CPUE map is Emily's "plot_idw_xbyx()"
# THIS SHOULD ONLY BE USED FOR THE GOA - in the AI, the area is too narrow for a raster map of CPUE and we should instead be using the bubble plots of CPUE.
# get cpue table by station for a species
sp <- 30060
yr <- 2018
dat2plot <- cpue_raw %>%
  filter(survey == SRVY & species_code == sp & year == yr)
colnames(dat2plot)
cpue_res <- 0.1 # will take less time
# example data:
# head(akgfmaps::YFS2017)

# This is a dummy figure! Doesn't mean anything because it's for GOA
figure1 <- plot_idw_xbyx(
  yrs = yr,
  dat = dat2plot,
  lat = "start_latitude",
  lon = "start_longitude",
  var = "cpue_kgkm2",
  year = "year",
  key.title = "POP (kg/km2)",
  grid = "extrapolation.grid",
  extrap.box = c(xmin = -180, xmax = -135, ymin = 52, ymax = 62),
  grid.cell = c(cpue_res, cpue_res),
  row0 = 1,
  region = "goa"
)

list_figures <- list()
list_figures[[1]] <- figure1
names(list_figures)[1] <- "POP" # NOTE: NEED TO MAKE THIS THE WHOLE LIST OF SPECIES




if (print_figs) {
  lapply(
    X = list_figures, FUN = make_png, year = YEAR, region = SRVY,
    savedir = dir_out_figures
  )
}

# Check that the figure list is filled out
length(list_figures)

# SAVE FIGURES -----------------------------------------------------------
# save(list_figures,
#   file = paste0(dir_out_figures, "report_figures.rdata")
# )
save(list_biomass_ts, file = paste0(dir_out_figures, "biomass_ts.rdata"))
save(list_cpue_bubbles, file = paste0(dir_out_figures, "cpue_bubbles.rdata"))
save(list_joy_length, file = paste0(dir_out_figures, "joy_length.rdata"))
