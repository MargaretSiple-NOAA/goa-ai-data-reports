# GOA/AI DATA REPORT

# Report settings -------------------------------------------------------------
usePNGPDF <- "png"
maxyr <- 2018 # Change this for the year!
compareyr <- 2016 # Change this for the year!
print_figs <- FALSE # Do you want to print out PNGs of each figure?

# When did you save the last version of the figures and tables you want to use?
tabledate <- "2022-08-30"
figuredate <- "2022-08-30"

# Survey information ------------------------------------------------------
survnumber <- "49th" # Change this for the year!
dates_conducted <- "June 6th through August 14th, 2018" # Change this for the year!
SRVY <- "AI" # Options: "GOA", "AI"
YEAR <- maxyr
vessel1 <- "FV Ocean Explorer"
vessel2 <- "FV Alaska Provider"
captain1 <- "Dan Carney"
captain2 <- "Loren Reynolds"

ref_compareyr <- "@von_szalay_data_2017" # should be included in cite/bibliography.bib

if (SRVY == "GOA"){dir_googledrive <- "1UAQKChSuKohsRJ5enOloHPk3qFtk5kVC"} 
# Link to folder:  https://drive.google.com/drive/folders/1UAQKChSuKohsRJ5enOloHPk3qFtk5kVC 
# This is where all the text files live and are edited.

if(SRVY =="AI"){dir_googledrive <- "11RBHMEQtkq4BsuzY7AeNdX8IQPr5bv_J"} 
# Link to folder: https://drive.google.com/drive/folders/11RBHMEQtkq4BsuzY7AeNdX8IQPr5bv_J

# Report info -------------------------------------------------------------
report_title <- paste0(
  "Data Report: ", maxyr, " ", NMFSReports::TitleCase(SRVY),
  " Bottom Trawl Survey"
)
report_authors <- "P. von Szalay, N. Raring, W. Palsson, B. Riggle, M. Siple"
report_yr <- maxyr

# Functions, packages, directories ---------------------------------------------
source("R/01_directories.R")
source("R/02_load_packages.R") 

# Get data from RACEBASE and AI/GOA schemas ------------------------------------
x <- askYesNo(msg = "Do you want to download local versions of RACEBASE tables now?")
if (x) {
  source("R/05_download_data_from_oracle.R")
}

# Get text from Google Drive ----------------------------------------------
y <- askYesNo(msg = "Do you want to re-download Google Drive files now?")
if (y) {
  source("R/06_get_gdrive_chapters.R")
}

# Functions and data prep -------------------------------------------------
source("R/04_functions.R") # May not need all these functions.
source("R/03_prep_data.R") # Make all the tables and stuff needed for report and pres.


################################################################################
################################################################################
################################################################################
# EVERYTHING BELOW THIS LINE IS FOR REPORTS
################################################################################
################################################################################
################################################################################

# NOTE: MAY CHANGE TO DRAW FROM ORACLE

# Get functions from Emily's data reports pkg -----------------------------
#devtools::source_url("https://github.com/MargaretSiple-NOAA/AFSCDataReport/blob/main/code/functions.R?raw=TRUE")
source("R/plot_idx_xbyx.R") # *** move to functions script when ready


# Create tables and figures -----------------------------------------------

z <- askYesNo(msg = "Do you want to build all the tables right now? NOTE: This may take a while.")
if (z) {
  source(here::here("R", "make_tabs.R"))
}else{
  load(file = 
    paste0(dir_in_tables,"report_tables.rdata")) # object: list_tables
}


aa <- askYesNo(msg = "Do you want to build all the figures right now? NOTE: This may take a while.")
if (aa) {
  source(here::here("R", "make_figures.R"))
}else{
  load(file = paste0(
    dir_in_figures,"report_figures.rdata")) # object: list_figures
}


# Load figures and tables -------------------------------------------------

rmarkdown::render(paste0(dir_markdown, "/PARENT.Rmd"),
                  output_dir = dir_out_chapters,
                  output_file = paste0("PARENT.docx")
)


# *** 06 - Results_spp ------------------------
report_spp1 <- add_report_spp(
  spp_info = spp_info,
  spp_info_codes = "species_code",
  report_spp = report_spp,
  report_spp_col = "order",
  report_spp_codes = "species_code",
  lang = TRUE
)

for (jj in 1:length(unique(report_spp1$file_name)[!is.na(unique(report_spp1$file_name))])) {
  print(paste0(jj, " of ", length(unique(report_spp1$file_name)[!is.na(unique(report_spp1$file_name))]), ": ", unique(report_spp1$file_name)[jj]))

  cnt_chapt_content <- auto_counter(cnt_chapt_content)
  filename00 <- paste0(cnt_chapt, "_spp_")
  rmarkdown::render(paste0(dir_code, "/06_results_spp.Rmd"),
    output_dir = dir_out_chapters,
    output_file = paste0(
      filename00, cnt_chapt_content, "_",
      unique(report_spp1$file_name)[jj], ".docx"
    )
  )
}

# *** 09 - Appendix ------------------------
cnt_chapt <- NMFSReports::auto_counter(cnt_chapt)
cnt_chapt_content <- "001"
filename0 <- paste0(cnt_chapt, "_appendix_")
rmarkdown::render(paste0(dir_code, "/09_appendix.Rmd"),
  output_dir = dir_out_chapters,
  output_file = paste0(filename0, cnt_chapt_content, ".docx")
)


# *** 10 - Endmatter ------------------------
cnt_chapt <- NMFSReports::auto_counter(cnt_chapt)
cnt_chapt_content <- "001"
filename0 <- paste0(cnt_chapt, "_endmatter_")
rmarkdown::render(paste0(dir_code, "/10_endmatter.Rmd"),
  output_dir = dir_out_chapters,
  output_file = paste0(filename0, cnt_chapt_content, ".docx")
)


# *** *** - Figures and Tables ------------------------
# - run figures and tables before each chapter so everything works smoothly
if (FALSE) {
  report_spp1 <- add_report_spp(
    spp_info = spp_info,
    spp_info_codes = "species_code",
    report_spp = report_spp,
    report_spp_col = "order",
    report_spp_codes = "species_code",
    lang = TRUE
  )

  # cnt_chapt<-auto_counter(cnt_chapt)
  cnt_chapt_content <- "001"
  filename0 <- paste0(cnt_chapt, "_")
  rmarkdown::render(paste0(dir_code, "/figtab_pres.Rmd"),
    output_dir = dir_out_ref,
    output_file = paste0(filename0, cnt_chapt_content, ".docx")
  )




  for (jj in 1:length(unique(report_spp1$file_name))) {
    print(paste0(jj, " of ", length(unique(report_spp1$file_name))))
    start_time <- Sys.time()
    # cnt_chapt<-auto_counter(cnt_chapt)
    # cnt_chapt_content<-"001"
    filename00 <- paste0(cnt_chapt, "_spp_")
    rmarkdown::render(paste0(dir_code, "/figtab_spp_pres.Rmd"),
      output_dir = dir_out_ref,
      output_file = paste0(
        filename00, cnt_chapt_content, "_",
        unique(report_spp1$file_name)[jj], ".docx"
      )
    )
    end_time <- Sys.time()
    print(paste0(end_time - start_time))
  }

  save(list_figures,
    file = paste0(dir_out_figures, "/report_figures_pres.rdata")
  )

  save(list_tables,
    file = paste0(dir_out_tables, "/report_tables_pres.rdata")
  )
}

load(file = paste0(dir_out_tables, "/report_figures_pres.rdata"))
load(file = paste0(dir_out_tables, "/report_tables_pres.rdata"))

# subtitle
cruises_maxyr0 <- haul_cruises_vess_maxyr %>%
  dplyr::filter(SRVY %in% c("NBS", "EBS")) %>%
  dplyr::select(
    "year", "survey_name", "vessel", "vessel_name",
    "vessel_ital", "SRVY", "SRVY_long",
    "start_date_cruise", "end_date_cruise",
    "start_date_haul", "end_date_haul"
  ) %>%
  unique() %>%
  group_by(
    year, survey_name, vessel, vessel_name,
    vessel_ital, SRVY, SRVY_long
  ) %>%
  dplyr::summarise(
    start_date_cruise = min(start_date_cruise),
    end_date_cruise = max(end_date_cruise),
    start_date_haul = min(start_date_haul),
    end_date_haul = max(end_date_haul)
  ) %>%
  dplyr::arrange(start_date_cruise)

str <- paste0(
  format(min(cruises_maxyr0$start_date_haul), format = "%B %d"),
  " to ",
  format(max(cruises_maxyr0$end_date_haul), format = "%B %d, %Y")
)

cnt_chapt <- auto_counter(cnt_chapt)
cnt_chapt_content <- "001"
filename0 <- paste0(cnt_chapt, "_presentation_")
rmarkdown::render(paste0(dir_code, "/11_presentation.Rmd"),
  output_dir = dir_out_chapters,
  output_file = paste0(filename0, cnt_chapt_content, ".pptx")
)



# SAVE OTHER OUTPUTS -----------------------------------------------------------

# save(list_figures,
#      file=paste0(dir_out_figures, "/report_figures.rdata"))
#
# save(list_tables,
#      file=paste0(dir_out_tables, "/report_tables.rdata"))

save(list_equations,
  file = paste0(dir_out_tables, "/report_equations.rdata")
)

# SAVE METADATA ----------------------------------------------------------------

con <- file(paste0(dir_out_todaysrun, "metadata.log"))
sink(con, append = TRUE)
sessionInfo()
sink() # Restore output to console
# cat(readLines("notes.log"), sep="\n") # Look at the log
