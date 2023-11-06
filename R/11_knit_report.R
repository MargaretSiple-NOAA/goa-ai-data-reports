# 10_knit_report
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


# Load figures ------------------------------------------------------------
# Static map of region - already loaded?
# img1_path <- "img/AleutiansMap.png"
# img1 <- png::readPNG(img1_path)

load(file = paste0(
  dir_in_figures, "cpue_bubbles.rdata"
)) # object: list_cpue_bubbles

load(file = paste0(
  dir_in_figures, "joy_length.rdata"
)) # object: list_joy_length

load(file = paste0(
  dir_in_figures, "list_temperature.rdata"
)) # object: list_temperature

load(file = paste0(
  dir_in_figures, "list_ldscatter.rdata"
)) # object: list_ldscatter


# Figures used for presentation
# load(file = paste0(
#   dir_in_figures,"biomass_ts.rdata")) # object: list_biomass_ts
# load(file = paste0(
#   dir_in_figures,"cpue_bubbles_strata.rdata")) # object: list_cpue_bubbles_strata


# Load the individual values ----------------------------------------------
load(file = paste0(dir_in_reportvalues, "/reportvalues.rdata"))

# Render the markdown doc! -----------------------------------------------------

starttime <- Sys.time()
rmarkdown::render(paste0(dir_markdown, "/PARENT.Rmd"),
  output_dir = dir_out_chapters,
  output_file = "PARENT.docx"
)
Sys.time() - starttime

#  time for 4 species: about 40-50 seconds
#  for all species: about 12 mins


# Append the appendices using officer -------------------------------------

maindoc <- read_docx(path = here::here(paste0(dir_out_chapters, "PARENT.docx"))) %>%
  body_add_break()

fullreport <- body_add_docx(
  x = maindoc,
  src = paste0(appendix_dir, "AppendixA/Appendix Figure A-1.docx")
) %>%
  body_add_break()

fullreport <- body_add_docx(fullreport,
  src = paste0(appendix_dir, "AppendixA/Appendix Figure A-2.docx")
) %>%
  body_add_break()

fullreport <- body_add_docx(fullreport,
  src = paste0(appendix_dir, "AppendixA/Appendix Figure A-3.docx")
) %>%
  body_add_break()

fullreport <- body_add_docx(fullreport,
  src = paste0(appendix_dir, "AppendixA/Appendix Figure A-4.docx")
) %>%
  body_add_break()

# Make appendix B
source(here::here("R", "12_make_appendices.R"))

fullreport <- body_add_docx(fullreport,
  src = (paste0(dir_out_chapters, "AppendixB.docx"))
) %>%
  body_add_break()

# Add Appendix C
fullreport <- body_add_docx(fullreport,
  src = paste0(appendix_dir, "Appendix C/APPENDIX C_2022.docx")
) %>%
  body_add_break()

# Add Appendix D
fullreport <- body_add_docx(fullreport,
  src = paste0(appendix_dir, "AppendixD/Figure D1 AI2022 BottomTemp.docx")
) %>%
  body_add_break()

fullreport <- body_add_docx(fullreport,
  src = paste0(appendix_dir, "AppendixD/Figure D2 AI2022 Surface Temp.docx")
) %>%
  body_add_break()



print(fullreport, target = paste0(dir_out_chapters, "Parent&Appendices.docx"))
