# This script should load the necessary data and build PNGs of all the figures for the paper, so they can be loaded in the Markdown part.


# Tables ------------------------------------------------------------------
table1 <- data.frame(Species = c("POP","ATF"), Abundance = c(1000, 2000))

list_tables <- list()
list_tables[[1]] <- table1



# Figures -----------------------------------------------------------------
# Fig 1 is district map
# Fig 2 is the start of the CPUE maps

# *** *** Save --------------------------------------------
# save(list_figures,
#      file = paste0(dir_out_figures, "/report_figures.rdata")
# )

save(list_tables,
     file = "figtab/report_tables.rdata"
)

