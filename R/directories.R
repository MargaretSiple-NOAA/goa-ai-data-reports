
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

# Where we save everything
dir.output <- paste0(dir_in, "output/")
dir.create(dir.output)
dir_out_todaysrun <- paste0(dir.output, Sys.Date(), "/")
dir.create(dir_out_todaysrun)
dir_out_todaysrun <- paste0(dir_out_todaysrun, "/", maxyr, "/")
dir.create(dir_out_todaysrun)

dirs <- c("chapters", "rawdata", "documentation", "figures", "tables", "cite", "ref")
for (i in 1:length(dirs)) {
  if (dir.exists(paste0(dir_out_todaysrun, dirs[i])) == FALSE) {
    dir.create(paste0(dir_out_todaysrun, "/", dirs[i]))
  }
  assign(x = paste0("dir_out_", dirs[i]), value = paste0(dir_out_todaysrun, "/", dirs[i], "/"))
}

# If loading in InDesign, table and figure headers need to be their own .docx. Here's a file that will do that for you.
# TableFigureHeader<-system.file("rmd", "TableFigureHeader.Rmd", package = "RMarkReports")

#TableFigureHeader <- paste0(dir_R, "TableFigureHeader.Rmd")

# CITATION STYLE ---------------------------------------------------------------
options("citation_format" = "pandoc")
