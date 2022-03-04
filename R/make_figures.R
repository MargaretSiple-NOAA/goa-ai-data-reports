# Make figures
# This script should load the necessary data and build PNGs of all the figures for the paper, so they can be loaded in the Markdown part.


# *** *** Save --------------------------------------------
save(list_figures,
     file = paste0(dir_out_figures, "/report_figures.rdata")
)

save(list_tables,
     file = paste0(dir_out_tables, "/report_tables.rdata")
)

