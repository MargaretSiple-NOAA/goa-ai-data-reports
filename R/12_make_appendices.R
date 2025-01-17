# Make appendices
# We append them to the main doc using {officer}
# Depauperate now but will probably be populated with scripts to create the appendices

# Appendix A --------------------------------------------------------------
source("R/06a_appendix_a.R")
rmarkdown::render(paste0(dir_markdown, "/APPENDIXA.Rmd"),
                  output_dir = dir_out_chapters,
                  output_file = "AppendixA.docx"
)

# Appendix B --------------------------------------------------------------
# Species encountered
# Appendix B is already created because the object is used in the report. So here we just knit it.
# source("R/06b_appendix_b.R") #makes the table appB

# Now just put in the flextable so that species_2 is in italics and new_suffix is not in italics. It should work for pretty much all the cases!
rmarkdown::render(paste0(dir_markdown, "/APPENDIXB.Rmd"),
                  output_dir = dir_out_chapters,
                  output_file = "AppendixB.docx"
)

# Appendix C --------------------------------------------------------------
# Length-weight parameters
# This file was created by Paul in 2022; now it is automated
# All 2022 appendices can be found in G:\ALEUTIAN\AI 2022\DataReport\Appendices\
source("R/06c_appendix_c.R")
rmarkdown::render(paste0(dir_markdown, "/APPENDIXC.Rmd"),
                  output_dir = dir_out_chapters,
                  output_file = "AppendixC.docx"
)


# Additional Appendices --------------------------------------------------------------
# Appendix D: Bottom and surface temp maps
# This file is created by Paul in 2022; will take more work to automate because it contains materials from ArcGIS and we'll have to turn them into maps in R. NOTE: THIS IS DEPRECATED AS OF 2023; WE ARE NO LONGER PRODUCING IT.