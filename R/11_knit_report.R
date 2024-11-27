# 11_knit_report
# MAKE SURE YOU'VE SET THE DATES YOU WANT TO GET THE DATA FROM IN 00_report_settings.R

# Get basic report info
source("R/00_report_settings.R")
source("R/01_directories.R")
source("R/02_load_packages.R")
source("R/03_functions.R")

cat(
  "Using report data from", tabledate, "for tables. \n",
  "Using report data from", figuredate, "for figures. \n",
  "Using report data from", reportvaluesdate, "for report values (mostly in-text values)."
)


# Load tables  ------------------------------------------------------------
load(file = paste0(dir_in_tables, "report_tables.rdata")) # object: list_tables
load(file = paste0(dir_in_tables, "table3s_list.rdata")) # object: table3s_list
load(file = paste0(dir_in_tables, "table4s_list.rdata")) # object: table4s_list
top_CPUE <- read.csv(
  file =
    paste0(dir_in_tables, "top_CPUE_", maxyr, ".csv")
) # /topcpue

compare_tab <- read.csv(paste0(dir_in_tables, maxyr, "_","comparison_w_previous_survey.csv"))


# Load figures ------------------------------------------------------------
# Static map of region - already loaded?
if (SRVY == "AI") {
  img1_path <- "img/AleutiansMap.png"
  img1 <- png::readPNG(img1_path)
}

load(file = paste0(
  dir_in_figures, "list_cpue_bubbles_strata.rdata"
)) # object: list_cpue_bubbles

load(file = paste0(
  dir_in_figures, "list_joy_length.rdata"
)) # object: list_joy_length

load(file = paste0(
  dir_in_figures, "list_temperature.rdata"
)) # object: list_temperature

load(file = paste0(
  dir_in_figures, "list_ldscatter.rdata"
)) # object: list_ldscatter

if(SRVY=="GOA"){
load(file = paste0(
  dir_in_figures, maxyr, "_station_map.RDS"
)) # object: station_map
}


# Load the individual values ----------------------------------------------
load(file = paste0(dir_in_reportvalues, "/reportvalues.rdata"))

# Render the markdown doc! -----------------------------------------------------

starttime <- Sys.time()
rmarkdown::render(paste0(dir_markdown, "/DATA_REPORT.Rmd"),
  output_dir = dir_out_chapters,
  output_file = "DATA_REPORT.docx"
)
Sys.time() - starttime

#  time for 4 species: about 40-50 seconds
#  for all species: about 12 mins


# Append the appendices using officer -------------------------------------

maindoc <- read_docx(path = here::here(paste0(dir_out_chapters, "DATA_REPORT.docx"))) %>%
  body_add_break()

fullreport <- body_add_docx(
  x = maindoc,
  src = paste0(appendix_dir, "Appendix A/Appendix A 2023.docx")
) %>%
  body_add_break()

# Make Appendix B
source(here::here("R", "12_make_appendices.R"))

fullreport <- body_add_docx(fullreport,
  src = (paste0(dir_out_chapters, "AppendixB.docx"))
) %>%
  body_add_break()

# Add Appendix C
fullreport <- body_add_docx(fullreport,
  src = paste0(appendix_dir, "Appendix C/APPENDIX C_2023.docx")
) %>%
  body_add_break()

print(fullreport, target = paste0(dir_out_chapters, "Report&Appendices.docx"))
