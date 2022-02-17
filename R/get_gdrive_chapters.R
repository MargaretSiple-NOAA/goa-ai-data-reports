# *** SIGN INTO GOOGLE DRIVE----------------------------------------------------
googledrive::drive_deauth()
googledrive::drive_auth()

# Download the chapters from google drive ---------------------------------
id_googledrive <- googledrive::as_id(dir_googledrive)

# List all files in the google drive
chaps <- googledrive::drive_ls(path = id_googledrive, type = "document")

# Download chunks of text from google drive  
for (i in 1:length(chaps)){
  googledrive::drive_download(file = googledrive::as_id(chaps$id[i]),type = 'txt', 
                              overwrite = TRUE, 
                              path = paste0(dir_out_gdrive, "/", chaps$name[i]))
}


# Convert text files in gdrive directory into Rmd files -------------------

txtfiles <- list.files(path = paste0(dir_out_gdrive,"/"),pattern = ".txt")

for (i in 1:length(txtfiles)){
  print(txtfiles[i])
  pandoc_convert(here::here("gdrive",paste(txtfiles[i])),
                 to = "markdown",
                 output = here::here("gdrive",
                                     gsub(txtfiles[i],pattern = ".txt",replacement = ".Rmd")))
}

# Now to insert google drive chunks into a markdown file, you can use, for example,:
# ```{r, child = "gdrive/Preface.txt"}
# 
# ```