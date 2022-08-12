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
yr <- 2017
POP2017 <- cpue_raw %>% 
  filter(srvy=="GOA" & species_code == sp & year == yr) 
colnames(POP2017)
cpue_res <- 0.1 # will take less time
# example data:
# head(akgfmaps:::YFS2017)


 figure1 <- plot_idw_xbyx(
   yrs = yr, 
   dat = POP2017, 
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
 list_figures[[1]] <- figure1 #want to give these figures names so I can save them by filename - how do I do it?
 
 

 
# test
# png(filename = paste0(dir_out_figures,"POP2017.png"), width = 8, height = 4.5,units = 'in',res = 200)
# figure1
# dev.off()
 


 # *** *** Save --------------------------------------------
 save(list_figures,
      file = paste0(dir_out_figures, "report_figures.rdata")
 )
 

# (optional) print out figures --------------------------------------------
 # ******find out a way to print all figures in a list into a directory with a certain image file type
# if(print_figs){
#   for(i in 1:length(list_figures)){
#     png(filename = paste0(dir_out_figures,names(list_figures)[i],".png"),)
#   }
#} 
 