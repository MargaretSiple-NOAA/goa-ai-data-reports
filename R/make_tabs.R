# This script should load the necessary data and build PNGs of all the figures for the paper, so they can be loaded in the Markdown part.


# Tables ------------------------------------------------------------------
# Table 1 is the target sample sizes for different species categories
# Table 2 is the number of stations allocated, attempted, and successfully completed.
# Table 3 onwards are specific to each species.
targetn <- read.csv("data/TargetN.csv") # for now, it's a csv
sampled_stations <- data.frame(INPFC_area = c("Shumagin","Chirikof"), 
                               Stations_allocated = c(74, 32)) # there is sql code for this in sql/
top_CPUE <- make_top_cpue()

list_tables <- list()
list_tables[[1]] <- targetn
list_tables[[2]] <- sampled_stations
list_tables[[3]] <- 


save(list_tables,
     file = "figtab/report_tables.rdata"
)

