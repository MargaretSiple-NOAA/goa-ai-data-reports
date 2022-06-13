plot_idw_xbyx <- function(
  yrs, 
  dat, 
  lat,
  lon,
  var,
  year,
  key.title = "", 
  grid = "extrapolation.grid",
  extrap.box, 
  set.breaks = "auto", #seq(from = -2, to = 20, by = 2),
  grid.cell = c(0.02, 0.02), 
  row0 = 2, 
  region = "bs.south",
  dist_unit = "nm", # nautical miles
  col_viridis = "mako", 
  plot_coldpool = FALSE, 
  plot_stratum = TRUE, 
  use.survey.bathymetry = FALSE) {
  
  reg_dat <- akgfmaps::get_base_layers(select.region = region)
  yrs <- as.numeric(sort(x = yrs, decreasing = T))
  figure <- ggplot()
  dat <- dat %>%
    dplyr::rename(year = as.character(year), 
                  lat = as.character(lat), 
                  lon = as.character(lon), 
                  var = as.character(var)) %>% 
    dplyr::select(year, lat, lon, var) %>% 
    dplyr::mutate(year = as.numeric(year), 
                  lat = as.numeric(lat), 
                  lon = as.numeric(lon))
  
  if (nrow(dat) != 0) {
    if (set.breaks[1] =="auto") {
      set.breaks <- set_breaks(dat = dat, var = "var")
    }
    
    # Select data and make plot
    for (ii in length(yrs):1) {
      
      # temp1 <- unique(dat$SRVY[dat$year == yrs[ii]])
      
      temp <- dat %>% 
        dplyr::filter(year == yrs[ii]) 
      
      temp1 <- akgfmaps::make_idw_map(
        LATITUDE = temp$lat,
        LONGITUDE = temp$lon,
        CPUE_KGHA = temp$var,
        use.survey.bathymetry = use.survey.bathymetry,
        region = region, 
        out.crs = as.character(crs(reg_dat$bathymetry)),
        extrap.box = extrap.box, 
        set.breaks = set.breaks,
        grid.cell = grid.cell, 
        key.title = key.title)
      
      temp0 <- temp1[grid][[1]]  
      
      if (ii == length(yrs)) {
        stars_list <- temp0
        names(stars_list)[names(stars_list) == "var1.pred"] <- paste0("y", yrs[ii])  
      } else {
        stars_list$temp <- temp0$var1.pred
        names(stars_list)[names(stars_list) == "temp"] <- paste0("y", yrs[ii])   
      }
    }
    
    
    # stars_list0<-stars_list
    
    # https://rpubs.com/michaeldorman/646276
    stars_list <- stars_list %>% 
      dplyr::select(names(stars_list)[substr(start = 1, stop = 1, x = names(stars_list)) == "y"])
    names(stars_list)<-gsub(pattern = "y", replacement = "", x = names(stars_list))
    stars_list = stars::st_redimension(stars_list)
    names(stars_list) = "value"
    
    
    figure <- figure +
      geom_stars(data = stars_list, na.rm = TRUE) 
  }
  
  
  
  if (plot_coldpool) {
    
    temp_break <- 2 # 2*C
    
    coords <- raster::coordinates(coldpool:::nbs_ebs_bottom_temperature)
    
    for(i in 1:length(yrs)) {
      sel_layer_df <- data.frame(x = coords[,1],
                                 y = coords[,2],
                                 temperature = coldpool:::nbs_ebs_bottom_temperature@data@values[,i])
      sel_layer_df <- sel_layer_df[!is.na(sel_layer_df$temperature),]
      sel_layer_df$year <- yrs[i]
      
      if(i == 1) {
        bt_year_df <- sel_layer_df
      } else{
        bt_year_df <- dplyr::bind_rows(bt_year_df, sel_layer_df)
      }
    }
    
    figure <- figure +
      ggplot2::geom_tile(data = bt_year_df %>%
                           dplyr::filter(temperature <= temp_break) %>% 
                           dplyr::rename(new_dim = year),
                         aes(x = x,
                             y = y, 
                             group = new_dim),
                         fill = "magenta",#"gray80",
                         alpha = 0.5)
    
  }  
  
  if (length(yrs) == 0) {
    
    grid <- ""
    figure <- figure +
      ggplot2::geom_text(mapping = aes(x = mean(reg_dat$lon.breaks), 
                                       y = mean(reg_dat$lat.breaks), 
                                       label = "No data was available\nfor this species in this\nregion for this year."), 
                         fontface="bold")
  }   else if (length(yrs)>1) {
    figure <- figure +
      facet_wrap( ~ new_dim, nrow = row0) +
      coord_equal() 
  }
  
  
  if (plot_stratum) {
    figure <- figure +
      geom_sf(data = reg_dat$survey.strata,
              color = "grey50",
              size = 0.1,
              alpha = 0,
              fill = NA)
  }
  
  lon_break <- reg_dat$lon.breaks
  lat_break <- reg_dat$lat.breaks
  if (length(yrs) > 6) {
    lon_break <- reg_dat$lon.breaks[rep_len(x = c(FALSE, TRUE), length.out = length(lon_break))]
    lat_break <- reg_dat$lat.breaks[rep_len(x = c(FALSE, TRUE), length.out = length(lat_break))]
  }
  
  figure <- figure +
    geom_sf(data = reg_dat$graticule,
            color = "grey80",
            alpha = 0.2) +
    geom_sf(data = reg_dat$akland, 
            color = NA, 
            fill = "grey50") +
    scale_y_continuous(name = "", # "Latitude",,
                       # labels = lat_break, 
                       # labels = reg_dat$lat.breaks, 
                       limits = reg_dat$plot.boundary$y,
                       breaks = lat_break) +
    scale_x_continuous(name = "", # "Longitude"#,
                       # labels = reg_dat$lon.breaks,
                       limits = reg_dat$plot.boundary$x,
                       breaks = lon_break) +
    # coord_sf(xlim = reg_dat$plot.boundary$x, 
    #          ylim = reg_dat$plot.boundary$y)  +
    ggsn::scalebar(data = reg_dat$survey.grid,
                   location = "bottomleft",
                   dist = 150,
                   dist_unit = dist_unit,
                   transform = FALSE,
                   st.dist = ifelse(row0 > 2, 0.08, 0.04),
                   height = ifelse(row0 > 2, 0.04, 0.02),
                   st.bottom = FALSE, #ifelse(row0 <= 2, TRUE, FALSE),
                   st.size = ifelse(row0 > 2, 2.5, 3), # 2.5
                   model = reg_dat$crs) 
  
  
  if (grid == "continuous.grid") {
    figure <- figure + 
      scale_fill_viridis_c(option = col_viridis, 
                           #limits = range(set.breaks),
                           na.value = "transparent", 
                           breaks = set.breaks,
                           labels = set.breaks)  + 
      guides(fill=guide_colourbar(title=key.title, 
                                  title.position="top", 
                                  title.hjust = 0.5))
    
  } else if (grid == "extrapolation.grid") {
    # temp <- factor(x = temp0$var1.pred, levels = levels(temp0$var1.pred), labels = levels(temp0$var1.pred), ordered = T)
    figure <- figure +
      scale_fill_manual(
        values=c("gray90", 
                 viridis::mako(
                   direction = -1, 
                   n = temp1$n.breaks,
                   begin = 0,
                   end = 0.80)), 
        name = key.title,
        na.value = "transparent", 
        breaks = levels(temp0$var1.pred), 
        labels = levels(temp0$var1.pred))      
  }
  
  figure <- figure +
    guides(
      fill = guide_legend(title.position = "top", 
                          label.position = "bottom",
                          title.hjust = 0.5,
                          nrow = 1
      )) +
    
    #set legend position and vertical arrangement
    theme( 
      # axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), 
      panel.background = element_rect(fill = "white", 
                                      colour = NA), 
      panel.border = element_rect(fill = NA, 
                                  colour = "grey20"), 
      axis.text = element_text(size = 8),
      
      strip.background = element_blank(), 
      strip.text = element_text(size = 10, face = "bold"), 
      # legend.title = element_text(size = 12), #, vjust = .5, hjust = .3),
      legend.text = element_text(size = 10),
      legend.background = element_rect(colour = "transparent", 
                                       fill = "transparent"),
      legend.key = element_rect(colour = "transparent", 
                                fill = "transparent"),
      # legend.title.align = 0,#.1, 
      legend.position = "bottom",
      # legend.box.just = "center",
      # legend.key.width = unit(.5, "in"), 
      legend.box = "horizontal")
  
  return(figure)
  
}
