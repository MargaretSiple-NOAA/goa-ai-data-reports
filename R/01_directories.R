# SAVE FILE LOCATIONS ----------------------------------------------------------
# Just in case you change the base name for any reason, it will change for anytime you load the files inside the folder, too! (e.g., if you have something against "scripts" being the name of the folder, just let the script know in one place aka right here).

# Directories to all of your current folders
dir_in <- paste0(getwd(), "/")
dirs <- list.dirs(path = getwd(), full.names = FALSE)
dirs <- dirs[!grepl(pattern = "\\/", x = dirs)]
dirs <- dirs[!grepl(pattern = "\\..", x = dirs)]
dirs <- dirs[dirs != ""]
for (i in 1:length(dirs)) {
  assign(
    x = paste0("dir_", dirs[i]),
    value = paste0(dir_in, (dirs[i]), "/")
  )
}

# Data folder (for storing db tables) -------------------------------------
dir.create(paste0(dir_in,"data/"))
dir.create(paste0(dir_in,"data/local_gap_products/"))
dir.create(paste0(dir_in,"data/local_ai/"))
dir.create(paste0(dir_in,"data/local_goa/"))
dir.create(paste0(dir_in,"data/local_racebase/"))

# Folder with tables produced by other people -----------------------------
if (SRVY == "AI") {
  dir_in_premadetabs <- "G:/ALEUTIAN/AI 2022/DataReport/"
}
if (SRVY == "GOA") {
  dir_in_premadetabs <- "G:/GOA/GOA 2023/DataReport/"
}
# Folder containing appendices --------------------------------------------
if (SRVY == "AI") {
  appendix_dir <- "G:/ALEUTIAN/AI 2022/DataReport/Appendices/"
}
if (SRVY == "GOA") {
  appendix_dir <- "G:/GOA/GOA 2023/DataReport/Appendices/"
}

# Output folder -----------------------------------------------------------
dir.output <- paste0(dir_in, "output/")
dir.create(dir.output)

dir_out_todaysrun <- paste0(dir.output, Sys.Date(), "/")
dir.create(dir_out_todaysrun)

dirs <- c("chapters", 
          "figures", 
          "markdown", 
          "tables", 
          "cite")
for (i in 1:length(dirs)) {
  if (dir.exists(paste0(dir_out_todaysrun, dirs[i])) == FALSE) {
    dir.create(paste0(dir_out_todaysrun, "/", dirs[i]))
  }
  assign(x = paste0("dir_out_", dirs[i]), value = paste0(dir_out_todaysrun, "/", dirs[i], "/"))
}


# Get tables and figures you already made ---------------------------------

if(is.na(tabledate) | is.na(figuredate)){
  print("Please choose the date when you last saved figures or tables.")
}

dir_in_tables <- paste0(dir.output,tabledate,"/tables/")
dir_in_figures <- paste0(dir.output,figuredate, "/figures/")
dir_in_reportvalues <- paste0(dir.output,reportvaluesdate)

if(!file.exists(paste0(dir_in_tables,"report_tables.rdata"))){
  print(paste("Report tables file does not exist. If you haven't made tables for the report yet, ignore this message. If you are trying to build a report, make sure you generated tables on", tabledate, "or select a new date."))
}


# Folder with google drive snippets ---------------------------------------
dir_out_gdrive <- "gdrive"
dir.create(dir_out_gdrive)

# CITATION STYLE ---------------------------------------------------------------
options("citation_format" = "pandoc")

