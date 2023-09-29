
# Get base layers to show GOA strata --------------------------------------
lengthen_pal <- function(x = 1:10, shortpal) {
  ncolours <- length(unique(x))
  newpal <- colorRampPalette(shortpal)(ncolours)
  return(newpal)
}

library(MetBrewer)
## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

## -----------------------------------------------------------------------------
library(akgfmaps)

# SEBS <- akgfmaps::get_base_layers(select.region = "bs.south", set.crs = "auto")
# class(SEBS)
# names(SEBS)
GOA <- akgfmaps::get_base_layers(select.region = "goa", set.crs = "auto")
class(GOA)
names(GOA)

## ----fig.width=8, fig.height = 6----------------------------------------------
nstrata <- length(unique(GOA$survey.strata$STRATUM))

newpal <- lengthen_pal(x = 1:nstrata,
                       shortpal = MetBrewer::met.brewer(palette_name = "Renoir", n=10, type="discrete"))

GOA_plot <- ggplot() +
   geom_sf(data = GOA$akland) +
   geom_sf(data = filter(GOA$survey.strata, STRATUM!=0), aes(fill = factor(STRATUM))) +
   #geom_sf(data = GOA$bathymetry) +
   geom_sf(data = GOA$survey.area, fill = NA) +
   geom_sf(data = GOA$graticule, color = "grey70", alpha = 0.5) +
   coord_sf(xlim = GOA$plot.boundary$x, 
            ylim = GOA$plot.boundary$y) +
  scale_fill_manual(values = newpal) +
   scale_x_continuous(name = "Longitude", 
                      breaks = GOA$lon.breaks) + 
   scale_y_continuous(name = "Latitude", 
                      breaks = GOA$lat.breaks) + 
   theme_bw() +
  theme(legend.position = "none")

png("output/GOA_with_strata.png",width = 10,height = 6,res = 200,units = 'in')
GOA_plot
dev.off()

GOA_plot_bathy <- ggplot() +
  geom_sf(data = GOA$akland) +
  #geom_sf(data = filter(GOA$survey.strata, STRATUM!=0), aes(fill = factor(STRATUM))) +
  geom_sf(data = GOA$bathymetry,aes(color=METERS),lwd=0.2) +
  geom_sf(data = GOA$survey.area, fill = NA,color="red",lwd=.5) +
  geom_sf(data = GOA$graticule, color = "grey70", alpha = 0.5) +
  coord_sf(xlim = GOA$plot.boundary$x, 
           ylim = GOA$plot.boundary$y) +
  scale_color_distiller(palette = "PuBu") +
  scale_x_continuous(name = "Longitude", 
                     breaks = GOA$lon.breaks) + 
  scale_y_continuous(name = "Latitude", 
                     breaks = GOA$lat.breaks) + 
  theme_bw() +
  theme(legend.position = "none")

png("output/GOA_with_bathy.png",width = 10,height = 6,res = 200,units = 'in')
GOA_plot_bathy
dev.off()
