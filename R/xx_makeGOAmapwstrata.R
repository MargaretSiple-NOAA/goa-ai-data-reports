
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

GOA <- akgfmaps::get_base_layers(select.region = "goa", set.crs = "auto")
class(GOA)
names(GOA)

AI <- akgfmaps::get_base_layers(select.region = "ai", set.crs = "auto")
class(AI)
names(AI)

## ----Map of GOA survey area with strata shaded--------------------------------
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


# Make AI map with strata -------------------------------------------------

nstrata <- length(unique(AI$survey.strata$STRATUM))
newpal <- lengthen_pal(x = 1:nstrata,
                       shortpal = MetBrewer::met.brewer(palette_name = "Nizami", n=8, type="discrete"))
newpal <- sample(newpal, size = length(newpal),replace = FALSE)

AI_plot <- ggplot() +
  geom_sf(data = AI$akland) +
  geom_sf(data = filter(AI$survey.strata, STRATUM!=0), aes(fill = factor(STRATUM))) +
  #geom_sf(data = GOA$bathymetry) +
  geom_sf(data = AI$survey.area, fill = NA) +
  geom_sf(data = AI$graticule, color = "grey70", alpha = 0.5) +
  coord_sf(xlim = AI$plot.boundary$x, 
           ylim = AI$plot.boundary$y) +
  scale_fill_manual(values = newpal) +
  scale_x_continuous(name = "Longitude", 
                     breaks = AI$lon.breaks) + 
  scale_y_continuous(name = "Latitude", 
                     breaks = AI$lat.breaks) + 
  theme_bw() +
  theme(legend.position = "none")

png("output/AI_with_strata.png",width = 10,height = 6,res = 200,units = 'in')
AI_plot
dev.off()

AI_plot_bathy <- ggplot() +
  geom_sf(data = AI$akland) +
  #geom_sf(data = filter(GOA$survey.strata, STRATUM!=0), aes(fill = factor(STRATUM))) +
  geom_sf(data = AI$bathymetry,aes(color=METERS),lwd=0.2) +
  geom_sf(data = AI$survey.area, fill = NA,color="red",lwd=.5) +
  geom_sf(data = AI$graticule, color = "grey70", alpha = 0.5) +
  coord_sf(xlim = AI$plot.boundary$x, 
           ylim = AI$plot.boundary$y) +
  scale_color_distiller(palette = "PuBu") +
  scale_x_continuous(name = "Longitude", 
                     breaks = AI$lon.breaks) + 
  scale_y_continuous(name = "Latitude", 
                     breaks = AI$lat.breaks) + 
  theme_bw() +
  theme(legend.position = "none")

png("output/GOA_with_bathy.png",width = 10,height = 6,res = 200,units = 'in')
GOA_plot_bathy
dev.off()


# Make map with INPFC and NMFS regions ------------------------------------
goa_all <- akgfmaps::get_base_layers(select.region = "goa", set.crs = "auto")

goa_inpfc <- akgfmaps::get_base_layers(select.region = "inpfc.goa", set.crs = "auto")
goa_nmfs <- akgfmaps::get_base_layers(select.region = "nmfs", set.crs = "auto")

geo_order <- c( "Shumagin","Chirikof", "Kodiak", "Yakutat", "Southeastern")    
palette_map <- MetBrewer::met.brewer(palette_name = "Nizami",n = 6,type = "discrete",direction = 1)[c(1,4,2,5,3)]

p1 <- ggplot() +
  geom_sf(data = goa_all$akland) +
  geom_sf(data = goa_inpfc, aes(fill = INPFC_STRATUM)) +
  scale_fill_manual("INPFC area", values = palette_map, breaks = geo_order)  +
  #geom_sf(data = GOA$bathymetry) +
  geom_sf(data = goa_all$survey.area, fill = NA) +
  #geom_sf(data = goa_all$graticule, color = "grey70", alpha = 0.5) +
  coord_sf(xlim = goa_all$plot.boundary$x, 
           ylim = goa_all$plot.boundary$y) +
 # labs(title = "INPFC") +
  theme_light() +
  theme(legend.position = "bottom")

png("INPFC_areas_GOA.png",width = 8,height = 4,units = 'in',res = 200)
p1 
dev.off()

p2 <- ggplot() +
  geom_sf(data = goa_all$akland) +
  geom_sf(data = goa_nmfs, aes(fill = REP_AREA)) +
  geom_sf(data = goa_all$survey.area, fill = NA) +
  coord_sf(xlim = goa_all$plot.boundary$x, 
           ylim = goa_all$plot.boundary$y) +
  labs(title = "NMFS")

library(patchwork)
p1 + p2 + plot_layout(ncol = 1)

class(GOA)
names(GOA)