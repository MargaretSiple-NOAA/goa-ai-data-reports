plot_pa_xbyx <- function(yrs,
                         dat,
                         lat,
                         lon,
                         year,
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
      dplyr::rename(
        year = as.character(year),
        lat = as.character(lat),
        lon = as.character(lon)
      ) %>%
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

  figure <- ggplot() +
    geom_sf(
      data = reg_dat$akland,
      color = NA,
      fill = "grey50"
    ) #+
  # geom_sf(data = reg_dat$graticule,
  #         color = "grey80",
  #         alpha = 0.2)

  # if (length(length(reg_dat$survey.area$color))>1 ) {
  if (plot_bubble) {
    figure <- figure +
      geom_point(
        data = dd,
        mapping = aes(
          x = londd, y = latdd,
          size = cpue_kgha,
          group = as.factor(year)
        ),
        color = mako(n = 1, begin = .25, end = .75),
        shape = 16,
        # size = 1.5,
        show.legend = TRUE,
        na.rm = TRUE
      ) +
      scale_size_continuous(
        name = paste0(key.title, "weight CPUE (kg/ha)"),
        range = c(1, 4)
      )
  } else {
    figure <- figure +
      geom_point(
        data = dd,
        mapping = aes(
          x = londd, y = latdd,
          # shape = key.title,
          group = as.factor(year)
        ),
        color = mako(n = 1, begin = .25, end = .75),
        shape = 16,
        size = 1.5,
        show.legend = TRUE,
        na.rm = TRUE
      )
  }

  figure <- figure +
    geom_sf(
      data = reg_dat$survey.area, # %>%
      # dplyr::filter(SRVY %in% SRVY1),
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
  # } else {
  #   figure <- figure   +
  #     geom_point(data = dd,
  #                mapping = aes(x = londd, y = latdd,
  #                              shape = key.title,
  #                              group = as.factor(year)),
  #                color = mako(n = 1, begin = .25, end = .75),
  #                # shape = 16,
  #                size = 2,
  #                show.legend = TRUE,
  #                na.rm = TRUE)
  # }

  # if (plot_coldpool) {
  #   temp_break <- 2 # 2*C
  #
  #   if (unique(dat$SRVY) %in% "EBS") {
  #     cp <- coldpool::ebs_bottom_temperature
  #   } else if (unique(dat$SRVY) %in% "NBS") {
  #     cp <- coldpool::nbs_ebs_bottom_temperature
  #   }
  #
  #   coords <- raster::coordinates(cp)
  #
  #   for(i in 1:length(yrs)) {
  #     sel_layer_df <- data.frame(x = coords[,1],
  #                                y = coords[,2],
  #                                temperature = cp@data@values[,i])
  #     sel_layer_df <- sel_layer_df[!is.na(sel_layer_df$temperature),]
  #     sel_layer_df$year <- yrs[i]
  #
  #     if(i == 1) {
  #       bt_year_df <- sel_layer_df
  #     } else{
  #       bt_year_df <- dplyr::bind_rows(bt_year_df, sel_layer_df)
  #     }
  #   }
  #
  #   figure <- figure +
  #     ggplot2::geom_tile(data = bt_year_df %>%
  #                          dplyr::filter(temperature <= temp_break), #%>%
  #                          # dplyr::rename(new_dim = year),
  #                        aes(x = x,
  #                            y = y,
  #                            group = year),
  #                        fill = "magenta",
  #                        alpha = 0.25,
  #                        show.legend = FALSE)
  #
  # }

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

  if (length(yrs) == 0) { # if there is no data to plot
    grid <- ""
    figure <- figure +
      ggplot2::geom_text(
        mapping = aes(
          x = mean(reg_dat$lon.breaks),
          y = mean(reg_dat$lat.breaks),
          label = "No data was available\nfor this species in this\nregion for this year."
        ),
        fontface = "bold"
      )
  } else if (length(yrs) > 1) { # if there is data to plot
    figure <- figure +
      facet_wrap(~year, nrow = row0) +
      coord_sf() # coord_equal()
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

  # lon_break <- reg_dat$lon.breaks
  # lat_break <- reg_dat$lat.breaks
  # lon_label <- reg_dat$lon.label
  # lat_label <- reg_dat$lat.label
  # if (length(yrs) > 6) { # are there a lot of plots on the page? In which case we'd want detail
  #   lon_break <- lon.breaks[rep_len(x = c(FALSE, TRUE), length.out = length(lon_break))]
  #   lat_break <- lat.breaks[rep_len(x = c(FALSE, TRUE), length.out = length(lat_break))]
  #   lon_label <- lon.breaks[rep_len(x = c(FALSE, TRUE), length.out = length(lon_label))]
  #   lat_label <- lat.breaks[rep_len(x = c(FALSE, TRUE), length.out = length(lat_label))]
  # }

  figure <- figure +
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
      ), # ifelse(row0 > 1, 0.08, 0.04), #ifelse(row0 == 1, 0.04, ifelse(row0 == 2, 0.06, 0.05)),  # ifelse(row0 > 1, 0.08, 0.04),
      height = ifelse(row0 == 1, 0.02, ifelse(row0 == 2, 0.04, 0.04)), # ifelse(row0 > 1, 0.04, 0.02),
      st.bottom = FALSE, # ifelse(row0 <= 2, TRUE, FALSE),
      st.size = dplyr::case_when(
        row0 == 1 & length(yrs) > 4 ~ 1.5,
        row0 == 1 & length(yrs) > 3 ~ 2,
        row0 == 1 ~ 3,
        row0 == 2 ~ 2.25,
        TRUE ~ 2
      )
    )
  if (plot_bubble) {
    figure <- figure +
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
  } else {
    figure <- figure +
      guides(
        color = guide_legend(
          title = key.title,
          title.position = "top",
          label.position = "right",
          title.hjust = 0.5,
          nrow = 1
        )
      )
  }

  figure <- figure +
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
    ) # ifelse(plot_bubble, "vertical", "horizontal"))


  return(figure)
}