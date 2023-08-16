# Make appendices
# Depauperate for now but will probably be populated with scripts to create the appendices

# Appendix A --------------------------------------------------------------
# Maps of study area with stratum boundaries (I think)
# These files are created by Paul and live at G:\ALEUTIAN\AI 2022\DataReport\Appendices\Appendix A
# We append them to the main doc using {officer}

# Appendix B --------------------------------------------------------------
# Species encountered
source("R/06_prep_data.R")
source("R/06a_appendix_b.R") #makes the table appB

# Now just put in the flextable so that species_2 is in italics and new_suffix is not in italics. It should work for pretty much all the cases!
rmarkdown::render(paste0(dir_markdown, "/APPENDIXB.Rmd"),
                  output_dir = dir_out_chapters,
                  output_file = "AppendixB.docx"
)

# Appendix C --------------------------------------------------------------
# Length-weight parameters
# This file is created by Paul in 2022; eventually we can automate
# All 2022 apendices can be found in G:\ALEUTIAN\AI 2022\DataReport\Appendices\

# Appendix D --------------------------------------------------------------
# Bottom and surface temps
# This file is created by Paul in 2022; will take more work to automate because it contains materials from ArcGIS and we'll have to turn them into maps in R.


# Appendix E --------------------------------------------------------------
# Ops manual
# Get Word doc from G:/ALEUTIAN/AI 2022/Ops Plan/2022 AI Scientific Operations Plan.docx
file.copy(from = "G:/ALEUTIAN/AI 2022/Ops Plan/2022 AI Scientific Operations Plan.docx",
          to = paste0(appendix_dir,"Appendix E.docx"))
