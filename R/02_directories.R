# LIBRARIES

#library(magrittr)



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


# Code folder (where markdown and scripts are stored) ---------------------
dir_code <- paste0(dir_in,"R/")

# Data folder (for storing db tables) -------------------------------------
dir.create(paste0(dir_in,"data/"))


# output folder -----------------------------------------------------------
dir.output <- paste0(dir_in, "output/")
dir.create(dir.output)

dir_out_todaysrun <- paste0(dir.output, Sys.Date(), "/")
dir.create(dir_out_todaysrun)

dirs <- c("chapters", "rawdata", 
          "documentation", 
          "figures", 
          "markdown", "tables", 
          "cite", "ref")
for (i in 1:length(dirs)) {
  if (dir.exists(paste0(dir_out_todaysrun, dirs[i])) == FALSE) {
    dir.create(paste0(dir_out_todaysrun, "/", dirs[i]))
  }
  assign(x = paste0("dir_out_", dirs[i]), value = paste0(dir_out_todaysrun, "/", dirs[i], "/"))
}

# Folder with google drive snippets ---------------------------------------

dir_out_gdrive <- "gdrive"
dir.create(dir_out_gdrive)

# CITATION STYLE ---------------------------------------------------------------
options("citation_format" = "pandoc")
