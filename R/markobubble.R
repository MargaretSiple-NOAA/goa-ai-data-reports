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
#' library(magrittr)
#' reg_dat_ai <- akgfmaps::get_base_layers(select.region = "ai", set.crs = "EPSG:3338")
#' reg_dat_ai$survey.area <- reg_dat_ai$survey.area %>%
#'   dplyr::mutate(
#'     SRVY = "AI",
#'     color = scales::alpha(colour = "grey80", 0.7),
#'     SURVEY = "Aleutian Islands"
#'   )
#' reg_dat <- reg_dat_ai
#' # cpue_raw is generated in prep_data.R and is a summary of cpue by species and station
#' thisyrshauldata <- cpue_raw %>%
#'   mutate(cpue_kgha = cpue_kgkm2 * 100) %>%
#'   filter(year == maxyr & survey == SRVY & species_code == spcode) %>%
#'   st_as_sf(
#'     coords = c("start_longitude", "start_latitude"),
#'     crs = "EPSG:4326"
#'   ) %>%
#'   st_transform(crs = reg_dat_ai$crs)
#'   fig <- plot_pa_xbyx(spcode=21921,dat = thisyrshauldata,yrs = c(2022),key.title = "",
#'   row0 = 2,reg_dat = reg_dat_ai,dist_unit = "nm", # nautical miles
#'   col_viridis = "mako
#'   ,plot_coldpool = FALSE,plot_stratum = FALSE)
#'  png("Atka_bubble_2022.png",width = 8,height = 5.5,units = 'in',res = 200)
#'  fig 
#'  dev.off()

#############################################################################
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
  
  legendtitle <- bquote(CPUE(kg / km^2))

  f1 <- ggplot() +
    geom_sf(
      data = reg_dat$akland,
      color = NA,
      fill = "grey40"
    ) +
    geom_sf(
      data = filter(thisyrshauldata,cpue_kgkm2>0), 
      aes(size = cpue_kgkm2),
      alpha = 0.5,
      color = mako(n = 1, begin = .25, end = .75)
    ) +
    geom_sf( #x's for places where we sampled but didn't catch any of that species
      data = filter(thisyrshauldata,cpue_kgkm2==0), 
      alpha = 0.5,
      color = "grey5",
      shape = 4, 
      size = 1
    ) +
    scale_size_area(
      name = legendtitle,max_size = 10
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
    ) +
    ggsn::scalebar(
      data = reg_dat$survey.grid,
      location = "bottomleft",
      dist = 100,
      dist_unit = dist_unit,
      transform = FALSE,
      st.dist = dplyr::case_when(
        row0 == 1 & length(yrs) > 4 ~ 0.07,
        row0 == 1 ~ 0.04,
        row0 == 2 ~ 0.06,
        TRUE ~ 0.05
      ),
      height = ifelse(row0 == 1, 0.02,
        ifelse(row0 == 2, 0.04, 0.04)
      ),
      st.bottom = FALSE,
      st.size = dplyr::case_when(
        row0 == 1 & length(yrs) > 4 ~ 1.5,
        row0 == 1 & length(yrs) > 3 ~ 2,
        row0 == 1 ~ 3,
        row0 == 2 ~ 2.25,
        TRUE ~ 2
      )
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
