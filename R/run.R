# GOA DATA REPORT
# Report settings -------------------------------------------------------------
usePNGPDF <- "png"
maxyr <- 2019
compareyr <- 2017

# Survey information ------------------------------------------------------
survnumber <- "49th" #I have no idea if this is right
dates_conducted <- "May 15th and July 31st, 2021"
SRVY <- "GOA"
YEAR <- 2021
vessel1 <- "FV Ocean Explorer"
vessel2 <- "FV Alaska Provider"
ref_compareyr <- "@von_szalay_data_2017" 
dir_googledrive <- "1UAQKChSuKohsRJ5enOloHPk3qFtk5kVC" # https://drive.google.com/drive/folders/1UAQKChSuKohsRJ5enOloHPk3qFtk5kVC This is where all the text files live and are edited


# Report info -------------------------------------------------------------
# devtools::install_github("EmilyMarkowitz-NOAA/NMFSReports")
report_title <- paste0(
  "Data Report: ", maxyr, " ", NMFSReports::TitleCase(SRVY),
  " Bottom Trawl Survey"
)
report_authors <- "P. von Szalay, N. Raring, W. Palsson, B. Riggle, M. Siple"
report_yr <- maxyr

nfish <- 360 #UPDATE THESE
ninverts <- 151
nstations <- 500
highest_total_catch <- c("Pacific cod (Gadus chalcogrammus), Arrowtooth flounder (Atherestes stomias)") # character vector. FIX and make list of species

# Functions, packages, directories ---------------------------------------------
source("R/directories.R")
source("R/load_packages.R")
source("R/functions.R") # May not need all these functions.


# Get data from RACEBASE --------------------------------------------------
x <- askYesNo(msg = "Do you want to download local versions of Oracle tables now?")
if (x) {
  dir.create("data/local_racebase", recursive = TRUE)
  source_url("https://github.com/afsc-gap-products/design-based-indices/blob/master/R/00_download_data_from_oracle.R")
}

# Get text from Google Drive ----------------------------------------------
y <- askYesNo(msg = "Do you want to re-download Google Drive files now?")
if (y) {
  source("R/get_gdrive_chapters.R")
}


# Get functions from Emily's data reports pkg -----------------------------
#devtools::source_url("https://github.com/MargaretSiple-NOAA/AFSCDataReport/blob/main/code/functions.R?raw=TRUE")
source("R/plot_idx_xbyx.R")


# Get CPUE tables from Emily's public-facing data pkg ---------------------
# Update this directory if you need to; grabs a time-stamped snapshot of the CPUE tables used in the data reports
cpue_raw <- read.csv(here::here("../../gap_public_data","output/2022-05-25/cpue_station.csv"))
head(cpue_raw)

# Create tables and figures -----------------------------------------------

z <- askYesNo(msg = "Do you want to build all the tables right now? NOTE: This may take a while.")
if (z) {
  source("R/make_tables.R")
}


aa <- askYesNo(msg = "Do you want to build all the figures right now? NOTE: This may take a while.")
if (aa) {
  source("R/make_figures.R")
}


# Load figures and tables -------------------------------------------------
load(file = paste0(dir_out_figures, "figtab/report_figures.rdata")) # object: list_figures
load(file = paste0(dir_out_tables, "figtab/report_tables.rdata")) # object: list_tables


# *** 01 - Abstract ------------------------
cnt_chapt <- NMFSReports::auto_counter(cnt_chapt)
cnt_chapt_content <- "001"
filename0 <- paste0(cnt_chapt, "_abstract_")
rmarkdown::render(
  input = paste0(dir_code, "/01_abstract.Rmd"),
  output_dir = dir_out_chapters,
  output_file = paste0(filename0, cnt_chapt_content, ".docx")
)


# *** 02 - Introduction ------------------------
cnt_chapt <- NMFSReports::auto_counter(cnt_chapt)
cnt_chapt_content <- "001"
filename0 <- paste0(cnt_chapt, "_introduction_")
rmarkdown::render(paste0(dir_code, "/02_introduction.Rmd"),
  output_dir = dir_out_chapters,
  output_file = paste0(filename0, cnt_chapt_content, ".docx")
)


# *** 04 - Methods ------------------------
cnt_chapt <- NMFSReports::auto_counter(cnt_chapt)
cnt_chapt_content <- "001"
filename0 <- paste0(cnt_chapt, "_methods_")
rmarkdown::render(paste0(dir_code, "/04_methods.Rmd"),
  output_dir = dir_out_chapters,
  output_file = paste0(filename0, cnt_chapt_content, ".docx")
)


# *** 05 - Results ------------------------
cnt_chapt <- NMFSReports::auto_counter(cnt_chapt)
cnt_chapt_content <- "001"
filename0 <- paste0(cnt_chapt, "_results_")
rmarkdown::render(paste0(dir_code, "/05_results.Rmd"),
  output_dir = dir_out_chapters,
  output_file = paste0(filename0, cnt_chapt_content, ".docx")
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


# *** 11 - Presentation ------------------------


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

# MAKE MASTER DOCX -------------------------------------------------------------

# USE GUIDENCE FROM THIS LINK
# https://support.microsoft.com/en-us/help/2665750/how-to-merge-multiple-word-documents-into-one

# SAVE METADATA ----------------------------------------------------------------

con <- file(paste0(dir_out_todaysrun, "metadata.log"))
sink(con, append = TRUE)
sessionInfo()
sink() # Restore output to console
# cat(readLines("notes.log"), sep="\n") # Look at the log
