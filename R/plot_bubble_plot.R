# Plot bubbles

library(akgfmaps)
library(patchwork)


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

nstrata <- length(unique(floor(ai_east$survey.grid$STRATUM / 10)))

stratumpal <- lengthen_pal(
  shortpal = MetBrewer::met.brewer(name = "Hokusai1", type = "continuous"),
  x = 1:nstrata
)
# stratumpal <- colorspace::lighten(lengthen_pal(
#   shortpal = MetBrewer::met.brewer(name = "Hokusai1",type = "continuous"),
#                                 x = 1:nstrata),amount = 0.6) #


# Plots -------------------------------------------------------------------

i=17

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
    labs(subtitle = paste0(namebubble, " - Central Aleutians")) +
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

  png(filename = paste0(dir_out_figures, namebubble, "_2018_bubble_example_Hokusai.png"), 
      width = 10, height = 9, units = "in", res = 150)
  p2 / (p1 | p3)
  dev.off()



# p1 + geom_sf(data = thisyrshauldata,aes(size=cpue_kgha),alpha = 0.5) +
#   coord_sf(xlim = ai_east$plot.boundary$x,
#            ylim = ai_east$plot.boundary$y) +
#   scale_x_continuous(breaks = ai_east$lon.breaks) +
#   scale_y_continuous(breaks = ai_east$lat.breaks)
