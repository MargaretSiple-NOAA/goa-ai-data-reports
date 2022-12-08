# Make appendices
# Depauperate for now but will probably be populated with scripts to create the appendices

# Appendix A --------------------------------------------------------------
# Maps of study area with stratum boundaries (I think)
# These files are created by Paul and live at G:\ALEUTIAN\AI 2022\DataReport\Appendices\Appendix A
# We append them to the main doc using {officer}

# Appendix B --------------------------------------------------------------
# Species encountered
# This appendix comes from a csv table. This year it was made by Nate, but in the future we want to automate the checking process. See issue https://github.com/MargaretSiple-NOAA/goa-ai-data-reports/issues/11
fish <- read.csv(file = paste0(appendix_dir, "Appendix B/2022DataReportAppendixB_fish.csv"))
# fish <- fish %>%
#   rename(
#     Family = FAMILY,
#     `Species name` = SPECIES_NAME,
#     `Common name` = COMMON_NAME
#   )
# 

inverts <- read.csv(file = paste0(appendix_dir, "Appendix B/2022DataReportAppendixB_inverts.csv"))
# inverts <- inverts %>%
#   rename(
#     Phylum = PHYLUM,
#     `Species name` = SPECIES_NAME,
#     `Common name` = COMMON_NAME
#   )

fish2 <- prep_appendix_b(fish)
inverts2 <- prep_appendix_b(inverts)

# Now just put in the flextable so that species_2 is in italics and new_suffix is not in italics. It should work for pretty much all the cases!
rmarkdown::render(paste0(dir_markdown, "/APPENDIXB.Rmd"),
                  output_dir = dir_out_chapters,
                  output_file = "AppendixB.docx"
)

# Appendix C --------------------------------------------------------------
# Length-weight parameters
# This file is created by Paul in 2022; eventually we can automate


# Appendix D --------------------------------------------------------------
# Bottom and surface temps
# This file is created by Paul in 2022; will take more work to automate because it contains materials from ArcGIS and we'll have to turn them into maps in R.
