# This script should load the necessary data and build PNGs of all the figures for the paper, so they can be loaded in the Markdown part.

# Figures -----------------------------------------------------------------
# Fig 1: district map
# Fig 2. CPUE map 1
# Fig 3. size comp plot 1
# Fig 4. CPUE map 2
# Fig 5. size comp plot 2

# Static figure: INPFC areas ----------------------------------------------
# 

# CPUE map ----------------------------------------------------------------
# The function used to generate this CPUE map is Emily's "plot_idw_xbyx()"
# get cpue table by station for a species
sp <- 30060
yr <- 2018
dat2plot <- cpue_raw %>% 
  filter(srvy==SRVY & species_code == sp & year == yr) 
colnames(dat2plot)
cpue_res <- 0.1 # will take less time
# example data:
# head(akgfmaps:::YFS2017)


 figure1 <- plot_idw_xbyx(
   yrs = yr, 
   dat = dat2plot, 
   lat = "latitude_dd",
   lon = "longitude_dd",
   var = "cpue_kgha",
   year = "year",
   key.title = "POP (kg/ha)", 
   grid = "extrapolation.grid",
   extrap.box = c(xmin = -180, xmax = -135, ymin = 52, ymax = 62), 
   grid.cell = c(cpue_res,cpue_res), 
   row0 = 1, 
   region = "goa") 
 
 list_figures <- list()
 list_figures[[1]] <- figure1 
  names(list_figures)[1] <- "POP" # NOTE: NEED TO MAKE THIS THE WHOLE LIST OF SPECIES
 

# test
# png(filename = paste0(dir_out_figures,"POP2017.png"), width = 8, height = 4.5,units = 'in',res = 200)
# figure1
# dev.off()


 
 if(print_figs){
 lapply(X = list_figures, FUN = make_png, year = YEAR, region = SRVY,
        savedir = dir_out_figures)
 }


 # *** *** Save --------------------------------------------
 save(list_figures,
      file = paste0(dir_out_figures, "report_figures.rdata")
 )
 

# (optional) print out figures --------------------------------------------

 