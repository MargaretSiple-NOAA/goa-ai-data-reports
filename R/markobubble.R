#' Make a bubble plot of the Aleutian Islands.
#' 
#' @description Map of the Aleutian Islands with bubbles indicating species CPUE. Original code by Emily Markowitz. Modified and included in data reports for AI by Megsie Siple. 
#'
#' @param yrs numeric vector of years for which you want plots. For data reports, this is `maxyr`.
#' @param dat dataframe of cpue by tow. Columns must include year, lat, long, cpue_kgha. For data reports, this is 
#' @param lat 
#' @param lon 
#' @param year 
#' @param key.title 
#' @param row0 
#' @param reg_dat sf object from calling akgfmaps::get_base_layers(). See example.
#' @param dist_unit 
#' @param col_viridis 
#' @param plot_coldpool 
#' @param plot_stratum 
#' @param plot_bubble 
#'
#' @return
#' @export
#'
#' @example
#' 
#' 
############## BELOW IS MEGSIE TESTING CODE FOR THE FUNCTION #################
library(magrittr)
reg_dat_ai <- akgfmaps::get_base_layers(select.region = "ai", set.crs = "EPSG:3338")
reg_dat_ai$survey.area <- reg_dat_ai$survey.area %>%
  dplyr::mutate(
    SRVY = "AI", 
    color = scales::alpha(colour = "grey80", 0.7), 
    SURVEY = "Aleutian Islands") 
reg_dat=reg_dat_ai
# cpue_raw is generated in prep_data.R and is a summary of cpue by species and station 
spcode <- 30060
thisyrshauldata <- cpue_raw %>%
  filter(year == maxyr & survey == SRVY & species_code == spcode) %>%
  st_as_sf(
    coords = c("start_longitude", "start_latitude"), #TODO NEED TO CHANGE TO THE RIGHT COORDS
    crs = "EPSG:4326"
  ) %>%
  st_transform(crs = reg_dat_ai$crs)
dat  <- thisyrshauldata %>%
  

##############################################################################

plot_pa_xbyx <- function(spcode, # speciescode
    yrs,
                         dat,
                         key.title = "",
                         row0 = 2,
                         reg_dat,
                         dist_unit = "nm", # nautical miles
                         col_viridis = "mako",
                         plot_coldpool = FALSE,
                         plot_stratum = FALSE,
                         plot_bubble = FALSE) {
  yrs <- as.numeric(sort(x = yrs, decreasing = T))

  if (plot_bubble) {
    dat0 <- dat %>%
      # dplyr::rename(
      #   year = as.character(year),
      #   lat = as.character(lat),
      #   lon = as.character(lon)
      # ) %>%
      dplyr::select(year, lat, lon, cpue_kgha) %>%
      dplyr::mutate(
        year = as.numeric(year),
        latdd = as.numeric(lat),
        londd = as.numeric(lon),
        cpue_kgha = as.numeric(cpue_kgha)
      )
    d <- dat0[, c("londd", "latdd", "year", "cpue_kgha")]
  } else {
    dat0 <- dat %>%
      dplyr::rename(
        year = as.character(year),
        lat = as.character(lat),
        lon = as.character(lon)
      ) %>%
      dplyr::select(year, lat, lon) %>%
      dplyr::mutate(
        year = as.numeric(year),
        latdd = as.numeric(lat),
        londd = as.numeric(lon)
      )
    d <- dat0[, c("londd", "latdd", "year")]
  }


  coordinates(d) <- c("londd", "latdd")
  sp::proj4string(d) <- CRS("+proj=longlat +datum=WGS84")
  dd <- data.frame(sp::spTransform(d, CRS(as.character(reg_dat$crs)[1])))
  # dd <- as(res, "SpatialPoints") ## For a SpatialPoints object rather than a SpatialPointsDataFrame

  f1 <- ggplot() +
    geom_sf(
      data = reg_dat$akland,
      color = NA,
      fill = "grey50"
    ) +
     geom_sf(data = thisyrshauldata, aes(size = cpue_kgkm2 ), alpha = 0.5,color = mako(n = 1, begin = .25, end = .75), show.legend = TRUE,
                  na.rm = TRUE) + scale_size_continuous(
                    name = "CPUE (kg/km^2)",
                    range = c(1, 4)
                  )

  f2 <- f1 +
    geom_sf(
      data = reg_dat$survey.area,
      mapping = aes(color = SURVEY),
      fill = NA,
      shape = NA,
      size = ifelse(row0 > 2, 0.25, 0.75),
      show.legend = TRUE
    ) +
    scale_color_manual(
      name = "", # key.title,
      values = reg_dat$survey.area$color,
      breaks = rev(reg_dat$survey.area$SURVEY),
      labels = rev((reg_dat$survey.area$SRVY))
    )

  
  
  if (plot_coldpool) {
    temp_break <- 2 # 2*C

    if (sum(dat$SRVY %in% "EBS") > 0) {
      cp <- coldpool::ebs_bottom_temperature
    } else if (unique(dat$SRVY) %in% "NBS") {
      cp <- coldpool::nbs_ebs_bottom_temperature
    }

    temp <- c()
    outline <- c()
    for (i in 1:length(yrs)) {
      #   temp <- c(temp, which(grepl(pattern = yrs[i], x = names(cp))))
      # }
      temp <- which(grepl(pattern = yrs[i], x = names(cp)))

      cp0 <- cp[[temp]] # [[which(grepl(x = names(cp), pattern = 2019))]] # cp[[temp[2]]]
      values(cp0)[values(cp0) <= temp_break] <- 1
      values(cp0)[values(cp0) > temp_break] <- NA
      pp <- rasterToPolygons(x = cp0, na.rm = TRUE, dissolve = TRUE)

      outline <- rbind(
        outline,
        pp %>%
          sp::geometry(obj = .) %>%
          sf::st_as_sf(x = .) %>%
          dplyr::mutate(new_dim = yrs[i])
      )
    }

    figure <- figure +
      geom_sf(
        data = outline %>%
          sf::st_cast(x = ., to = "MULTIPOLYGON"),
        size = 1,
        fill = NA, # alpha(colour = "red", alpha = 0.3),
        color = alpha(colour = "red", alpha = 0.3)
      )
    # fill = alpha(colour = "yellow", alpha = 0.3),
    # color = alpha(colour = "yellow", alpha = 0.3))
  }
  if (plot_stratum) {
    figure <- figure +
      geom_sf(
        data = reg_dat$survey.strata,
        color = "grey50",
        size = 0.1,
        # alpha = 0,
        fill = NA
      )
  }


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
                      ifelse(row0 == 2, 0.04, 0.04)),
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
        ),
        color = guide_legend(
          order = 2,
          label.position = "right",
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
      # legend.title = ,element_blank(),
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
    ) 


  return(figure)
}