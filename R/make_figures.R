# This script should load the necessary data and build PNGs of all the figures for the paper, so they can be loaded in the Markdown part.

# Make figures


# Static figure: INPFC areas ----------------------------------------------
# 

# CPUE map ----------------------------------------------------------------
# The function used to generate this CPUE map is Emily's "plot_idw_xbyx()"
# get cpue table by station for a species

cpuetab <- 
figure <- plot_idw_xbyx(yrs = c(2017), dat )
