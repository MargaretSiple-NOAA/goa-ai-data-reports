# 11_knit_report
# MAKE SURE YOU'VE SET THE DATES YOU WANT TO GET THE DATA FROM IN 00_report_settings.R

# Get basic report info
source("R/00_report_settings.R")
source("R/01_directories.R")
source("R/02_load_packages.R")
source("R/03_functions.R")

# Load tables  ------------------------------------------------------------
load(file = paste0(dir_in_tables, "report_tables.rdata")) # object: list_tables
load(file = paste0(dir_in_tables, "table3s_list.rdata")) # object: table3s_list
load(file = paste0(dir_in_tables, "table4s_list.rdata")) # object: table4s_list

top_CPUE <- read.csv(
  file =
    paste0(dir_in_tables, "top_CPUE_", maxyr, ".csv")
) # /topcpue

compare_tab <- read.csv(paste0(dir_in_tables, maxyr, "_", "comparison_w_previous_survey.csv"))

if (!exists("sizecomp")) {
  sizecomp <- read.csv(file = paste0(dir_out_srvy_yr, "tables/sizecomp_all.csv"))
}

# Load figures ------------------------------------------------------------
# Static map of region
if (SRVY == "AI") {
  img1_path <- "img/AleutiansMap.png"
}
if (SRVY == "GOA") {
  img1_path <- "img/INPFC_areas_GOA.png"
}

img1 <- png::readPNG(img1_path)

# Station map
if (SRVY == "GOA") {
  load(file = paste0(
    dir_out_srvy_yr, "figures/", maxyr, "_station_map.RDS"
  )) # object: station_map
}


# Diagram of net
net_img <- magick::image_read(path = here::here("img/Poly_NorE_Bottom Trawl.png"))
net_asp <- magick::image_info(net_img)$height / magick::image_info(net_img)$width # calculate the figure's aspect ratio

# Maps with CPUE
# update: I have removed this from loading because it is a HUGE rdata file. Instead, the code takes raw pngs that have already been generated and inserts them in the Word doc.
# load(file = paste0(
#   dir_in_figures, "list_cpue_bubbles_strata.rdata"
# )) # object: list_cpue_bubbles

# Calculate aspect ratio of CPUE maps (should be same aspect ratio for all species and complexes):
cpue_img <- magick::image_read(path = here::here(paste0("output/", SRVY, "_", maxyr, "/","figures/",maxyr,"_","Pacific ocean perch","_bubble.png"))) # just as an example - and POP is in both regions
cpue_asp <- magick::image_info(cpue_img)$height / magick::image_info(cpue_img)$width

# Aspect ratio for biomass time series plots
ts_img <- magick::image_read(path = here::here(paste0("output/", SRVY, "_", maxyr, "/","figures/",maxyr,"_","Pacific ocean perch","_biomass_3panel_ts.png")))
ts_asp <- magick::image_info(ts_img)$height / magick::image_info(ts_img)$width 

# Aspect ratio for "joy division plots" of length composition
lengthcomp_img <- magick::image_read(path = here::here(paste0("output/", SRVY, "_", maxyr, "/","figures/",maxyr,"_","Pacific ocean perch","_joyfreqhist.png"))) 
lengthcomp_asp <- magick::image_info(lengthcomp_img)$height / magick::image_info(lengthcomp_img)$width

# Aspect ratio for length-depth scatter plot
ldscatter_img <- magick::image_read(path = here::here(paste0("output/", SRVY, "_", maxyr, "/","figures/",maxyr,"_","Pacific ocean perch","_ldscatter.png"))) 
ldscatter_asp <- magick::image_info(ldscatter_img)$height / magick::image_info(ldscatter_img)$width 


# 3-panel time series plots
load(file = paste0(
  dir_in_figures, "list_3panel_ts.rdata"
)) # object: list_3panel_ts

# Length comps
load(file = paste0(
  dir_in_figures, "list_joy_length.rdata"
)) # object: list_joy_length

# Temperature plots
load(file = paste0(
  dir_in_figures, "list_temperature.rdata"
)) # object: list_temperature

# Length by depth plots
load(file = paste0(
  dir_in_figures, "list_ldscatter.rdata"
)) # object: list_ldscatter


# Load the individual values ----------------------------------------------
load(file = paste0(dir_in_reportvalues, "/reportvalues.rdata"))
load(file = paste0(dir_out_tables, "list_samplingdensities.rdata")) # object: list_samplingdensities.rdata

# Render the markdown doc! -----------------------------------------------------

# Free unused memory
gc()

# Render
starttime <- Sys.time()
rmarkdown::render(paste0(dir_markdown, "/DATA_REPORT.Rmd"),
  output_dir = dir_out_chapters,
  output_file = "DATA_REPORT.docx"
)

Sys.time() - starttime

#  time for 4 species: about 40-50 seconds
#  for all species: about 5 mins


# Make the appendices -----------------------------------------------------
# These will print to the output/[date]/chapters/ directory
source("R/12_make_appendices.R")

# Append the appendices using officer -------------------------------------
# gc() # clean up unused memory again (helps for giant data objects)
# 
# maindoc <- read_docx(path = here::here(paste0(dir_out_chapters, "DATA_REPORT.docx"))) %>%
#   body_add_break()
# 
# fullreport <- body_add_docx(
#   x = maindoc,
#   src = paste0(appendix_dir, "Appendix A/Appendix A 2023.docx")
# ) %>%
#   body_add_break()
# 
# # Make Appendix B
# source(here::here("R", "12_make_appendices.R"))
# 
# fullreport <- body_add_docx(fullreport,
#   src = (paste0(dir_out_chapters, "AppendixB.docx"))
# ) %>%
#   body_add_break()
# 
# # Add Appendix C
# fullreport <- body_add_docx(fullreport,
#   src = paste0(appendix_dir, "Appendix C/APPENDIX C_2023.docx")
# ) %>%
#   body_add_break()
# 
# print(fullreport, target = paste0(dir_out_chapters, "Report&Appendices.docx"))
