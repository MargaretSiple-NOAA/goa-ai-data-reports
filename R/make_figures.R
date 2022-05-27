# This script should load the necessary data and build PNGs of all the figures for the paper, so they can be loaded in the Markdown part.

# Figures -----------------------------------------------------------------
# Fig 1: district map
# Fig 2. CPUE map 1
# Fig 3. size comp plot
# Fig 4. CPUE map 2

# Static figure: INPFC areas ----------------------------------------------
# 

# CPUE map ----------------------------------------------------------------
# The function used to generate this CPUE map is Emily's "plot_idw_xbyx()"
# get cpue table by station for a species
sp <- 30060
yr <- 2017
POP2017 <- cpue_raw %>% 
  filter(srvy=="GOA" & species_code == sp & year == yr) 
colnames(POP2017)
# example data:
# head(akgfmaps:::YFS2017)



 figure <- plot_idw_xbyx(
   yrs = yr, 
   dat = POP2017, 
   lat = "latitude_dd",
   lon = "longitude_dd",
   var = "cpue_kgha",
   year = "year",
   key.title = "POP (kg/ha)", 
   grid = "extrapolation.grid",
   extrap.box = c(xmn = -180, xmx = -156, ymn = 54, ymx = 62), 
   grid.cell = c(1.5, 1.5), # will take less time
   row0 = 1, 
   region = "goa") 

 
 # test
 akgfmaps::get_base_layers(select.region = "goa")

 
 figure <- plot_idw_xbyx(yrs = c(2017))

 

 # *** *** Save --------------------------------------------
 # save(list_figures,
 #      file = paste0(dir_out_figures, "/report_figures.rdata")
 # )
 