# RUN

eport_title <- "Data Report"
workfaster <- TRUE # an attempt to satisfy limited patience
refcontent <- TRUE # produce extra summary text and tables for each spp to help with writing
googledrive_dl <- TRUE # redownload google drive tables and docs?
indesign_flowin <- FALSE
pres_img <- FALSE
usePNGPDF <- "png"

maxyr <- 2021
compareyr <- 2019
SRVY<-"NEBS"
ref_compareyr <- "@2019NEBSStevenson2022" # CHANGE
ref_maxyr_npfmc <- "@NPFMC2019" # temp
dir_googledrive <- "1i3NRmaAPpIYfMI35fpJCa-8AjefJ7J7X" # https://drive.google.com/drive/folders/1i3NRmaAPpIYfMI35fpJCa-8AjefJ7J7X?usp=sharing

# *** SIGN INTO GOOGLE DRIVE----------------------------------------------------
googledrive::drive_deauth()
googledrive::drive_auth()


# *** SOURCE SUPPORT SCRIPTS ---------------------------------------------------

source('./R/directories.R')

source('./R/functions.R')

# source('./R/get_data.R') # used to be datadl.R in Emily code

source('./R/data.R') # contains tables and objects to be used in text, including design-based estimates and static values


# *** REPORT TITLE -------------------------------------------------------------
report_title <- paste0('Data Report: ',maxyr,' ', NMFSReports::TitleCase(SURVEY),
                       ' continental shelf Bottom Trawl Survey of Groundfish and Invertebrate Fauna')
report_authors <- 'P. von Szalay, N. Raring, W. Palsson'
# report_yr <- maxyr 


# RUN EACH REPORT SECTION ------------------------------------------------------

# TOLEDO
# create file that checks for errors in RMDs
# https://github.com/NOAA-EDAB/esp_data_aggregation/blob/main/R-scripts/test_rmds.R
# https://github.com/NOAA-EDAB/esp_data_aggregation/blob/main/R-scripts/render%20dev%20report%20with%20errors.R

# *** Figures and Tables ------------------------
# - run figures and tables before each chapter so everything works smoothly

report_spp1 <- add_report_spp(spp_info = spp_info, 
                              spp_info_codes = "species_code", 
                              report_spp = report_spp, 
                              report_spp_col = "order", 
                              report_spp_codes = "species_code", 
                              lang = FALSE)

cnt_chapt_content<-"001"

if (FALSE) {
  # *** *** General figures --------------------------------------------
  filename0<-paste0(cnt_chapt, "_")
  rmarkdown::render(paste0(dir_code, "/figtab.Rmd"),
                    output_dir = dir_out_ref,
                    output_file = paste0(filename0, cnt_chapt_content, ".docx"))
  
  # *** *** Species figures --------------------------------------------
  for (jj in 1:length(unique(report_spp1$file_name)[!is.na(unique(report_spp1$file_name))])) {
    
    print(paste0(jj, " of ", length(unique(report_spp1$file_name))))
    start_time <- Sys.time()
    filename00<-paste0(cnt_chapt, "_spp_")
    rmarkdown::render(paste0(dir_code, "/figtab_spp.Rmd"),
                      output_dir = dir_out_ref,
                      output_file = paste0(filename00, cnt_chapt_content, "_", 
                                           unique(report_spp1$file_name)[jj],".docx"))
    end_time <- Sys.time()
    print(paste0(end_time - start_time))
  }
  
  # *** *** Appendix --------------------------------------------
  filename0<-paste0(cnt_chapt, "_")
  rmarkdown::render(paste0(dir_code, "/figtab_appendix.Rmd"),
                    output_dir = dir_out_ref,
                    output_file = paste0(filename0, cnt_chapt_content, ".docx"))
  
  # *** *** Save --------------------------------------------
  save(list_figures,
       file=paste0(dir_out_figures, "/report_figures.rdata"))
  
  save(list_tables,
       file=paste0(dir_out_tables, "/report_tables.rdata"))
  
}

load(file = paste0(dir_out_figures, "/report_figures.rdata"))
load(file = paste0(dir_out_tables, "/report_tables.rdata"))

# rmarkdown::render(input = "./notforgit/test.Rmd",
#                   output_dir = dir_out_chapters,
#                   output_file = "test.docx")

# *** 01 - Abstract ------------------------
cnt_chapt<-auto_counter(cnt_chapt)
cnt_chapt_content<-"001"
filename0<-paste0(cnt_chapt, "_abstract_")
rmarkdown::render(input = paste0(dir_code, "/01_abstract.Rmd"),
                  output_dir = dir_out_chapters,
                  output_file = paste0(filename0, cnt_chapt_content, ".docx"))


# *** 02 - Introduction ------------------------
cnt_chapt<-auto_counter(cnt_chapt)
cnt_chapt_content<-"001"
filename0<-paste0(cnt_chapt, "_introduction_")
rmarkdown::render(paste0(dir_code, "/02_introduction.Rmd"),
                  output_dir = dir_out_chapters,
                  output_file = paste0(filename0, cnt_chapt_content, ".docx"))


# *** 04 - Methods ------------------------
cnt_chapt<-auto_counter(cnt_chapt)
cnt_chapt_content<-"001"
filename0<-paste0(cnt_chapt, "_methods_")
rmarkdown::render(paste0(dir_code, "/04_methods.Rmd"),
                  output_dir = dir_out_chapters,
                  output_file = paste0(filename0, cnt_chapt_content, ".docx"))


# *** 05 - Results ------------------------
cnt_chapt<-auto_counter(cnt_chapt)
cnt_chapt_content<-"001"
filename0<-paste0(cnt_chapt, "_results_")
rmarkdown::render(paste0(dir_code, "/05_results.Rmd"),
                  output_dir = dir_out_chapters,
                  output_file = paste0(filename0, cnt_chapt_content, ".docx"))


# *** 06 - Results_spp ------------------------
report_spp1 <- add_report_spp(spp_info = spp_info, 
                              spp_info_codes = "species_code", 
                              report_spp = report_spp, 
                              report_spp_col = "order", 
                              report_spp_codes = "species_code", 
                              lang = TRUE)

for (jj in 1:length(unique(report_spp1$file_name)[!is.na(unique(report_spp1$file_name))])) {
  
  print(paste0(jj, " of ", length(unique(report_spp1$file_name)[!is.na(unique(report_spp1$file_name))]), ": ", unique(report_spp1$file_name)[jj]))
  
  cnt_chapt_content<-auto_counter(cnt_chapt_content)
  filename00<-paste0(cnt_chapt, "_spp_")
  rmarkdown::render(paste0(dir_code, "/06_results_spp.Rmd"),
                    output_dir = dir_out_chapters,
                    output_file = paste0(filename00, cnt_chapt_content, "_", 
                                         unique(report_spp1$file_name)[jj],".docx"))
}

# *** 07 - Results_crabretow ------------------------
# cnt_chapt<-auto_counter(cnt_chapt)
# cnt_chapt_content<-"001"
# filename0<-paste0(cnt_chapt, "_results_crabretow_")
# rmarkdown::render(paste0(dir_code, "/07_results_crabretow.Rmd"),
#                   output_dir = dir_out_chapters,
#                   output_file = paste0(filename0, cnt_chapt_content, ".docx"))


# *** 09 - Appendix ------------------------
cnt_chapt<-auto_counter(cnt_chapt)
cnt_chapt_content<-"001"
filename0<-paste0(cnt_chapt, "_appendix_")
rmarkdown::render(paste0(dir_code, "/09_appendix.Rmd"),
                  output_dir = dir_out_chapters,
                  output_file = paste0(filename0, cnt_chapt_content, ".docx"))


# *** 10 - Endmatter ------------------------
cnt_chapt<-auto_counter(cnt_chapt)
cnt_chapt_content<-"001"
filename0<-paste0(cnt_chapt, "_endmatter_")
rmarkdown::render(paste0(dir_code, "/10_endmatter.Rmd"),
                  output_dir = dir_out_chapters,
                  output_file = paste0(filename0, cnt_chapt_content, ".docx"))


# *** 11 - Presentation ------------------------


# *** *** - Figures and Tables ------------------------
# - run figures and tables before each chapter so everything works smoothly
if (FALSE) {
  
  report_spp1 <- add_report_spp(spp_info = spp_info, 
                                spp_info_codes = "species_code", 
                                report_spp = report_spp, 
                                report_spp_col = "order", 
                                report_spp_codes = "species_code", 
                                lang = TRUE)
  
  # cnt_chapt<-auto_counter(cnt_chapt)
  cnt_chapt_content<-"001"
  filename0<-paste0(cnt_chapt, "_")
  rmarkdown::render(paste0(dir_code, "/figtab_pres.Rmd"),
                    output_dir = dir_out_ref,
                    output_file = paste0(filename0, cnt_chapt_content, ".docx"))
  
  
  
  
  for (jj in 1:length(unique(report_spp1$file_name))) {
    
    print(paste0(jj, " of ", length(unique(report_spp1$file_name))))
    start_time <- Sys.time()
    # cnt_chapt<-auto_counter(cnt_chapt)
    # cnt_chapt_content<-"001"
    filename00<-paste0(cnt_chapt, "_spp_")
    rmarkdown::render(paste0(dir_code, "/figtab_spp_pres.Rmd"),
                      output_dir = dir_out_ref,
                      output_file = paste0(filename00, cnt_chapt_content, "_", 
                                           unique(report_spp1$file_name)[jj],".docx"))
    end_time <- Sys.time()
    print(paste0(end_time - start_time))
  }
  
  save(list_figures,
       file=paste0(dir_out_figures, "/report_figures_pres.rdata"))
  
  save(list_tables,
       file=paste0(dir_out_tables, "/report_tables_pres.rdata"))
  
}

load(file = paste0(dir_out_tables, "/report_figures_pres.rdata"))
load(file = paste0(dir_out_tables, "/report_tables_pres.rdata"))

# subtitle
cruises_maxyr0  <- haul_cruises_vess_maxyr %>% 
  dplyr::filter(SRVY %in% c("NBS", "EBS")) %>% 
  dplyr::select("year", "survey_name", "vessel", "vessel_name", 
                "vessel_ital", "SRVY", "SRVY_long", 
                "start_date_cruise", "end_date_cruise", 
                "start_date_haul", "end_date_haul") %>% 
  unique() %>% 
  group_by(year, survey_name, vessel, vessel_name, 
           vessel_ital, SRVY, SRVY_long) %>% 
  dplyr::summarise(start_date_cruise = min(start_date_cruise), 
                   end_date_cruise = max(end_date_cruise), 
                   start_date_haul = min(start_date_haul), 
                   end_date_haul = max(end_date_haul)) %>% 
  dplyr::arrange(start_date_cruise)

str <- paste0(
  format(min(cruises_maxyr0$start_date_haul), format = "%B %d"), 
  " to ", 
  format(max(cruises_maxyr0$end_date_haul), format = "%B %d, %Y") ) 

cnt_chapt<-auto_counter(cnt_chapt)
cnt_chapt_content<-"001"
filename0<-paste0(cnt_chapt, "_presentation_")
rmarkdown::render(paste0(dir_code, "/11_presentation.Rmd"),
                  output_dir = dir_out_chapters,
                  output_file = paste0(filename0, cnt_chapt_content, ".pptx"))



# SAVE OTHER OUTPUTS -----------------------------------------------------------

# save(list_figures,
#      file=paste0(dir_out_figures, "/report_figures.rdata"))
# 
# save(list_tables,
#      file=paste0(dir_out_tables, "/report_tables.rdata"))

save(list_equations,
     file=paste0(dir_out_tables, "/report_equations.rdata"))

# MAKE MASTER DOCX -------------------------------------------------------------

#USE GUIDENCE FROM THIS LINK
#https://support.microsoft.com/en-us/help/2665750/how-to-merge-multiple-word-documents-into-one

# SAVE METADATA ----------------------------------------------------------------

con <- file(paste0(dir_out_todaysrun, "metadata.log"))
sink(con, append=TRUE)
sessionInfo()
sink() # Restore output to console
# cat(readLines("notes.log"), sep="\n") # Look at the log