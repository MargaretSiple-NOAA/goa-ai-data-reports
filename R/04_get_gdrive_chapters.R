# *** SIGN INTO GOOGLE DRIVE----------------------------------------------------
googledrive::drive_deauth()
googledrive::drive_auth()

# Download the chapters from google drive ---------------------------------
id_googledrive <- googledrive::as_id(dir_googledrive)
id_googledrive_otos <- googledrive::as_id(dir_googledrive_otos)

# List all files in the google drive
chaps <- googledrive::drive_ls(path = id_googledrive, type = "document")

# Download chunks of text from google drive as txt
# The google drive is here:  https://drive.google.com/drive/folders/1UAQKChSuKohsRJ5enOloHPk3qFtk5kVC
for (i in 1:nrow(chaps)) {
  googledrive::drive_download(
    file = googledrive::as_id(chaps$id[i]), type = "txt",
    overwrite = TRUE,
    path = paste0(dir_out_gdrive, "/", chaps$name[i])
  )
}


# Download table with the oto target and collection numbers ---------------
# This target and collection goal form only started in 2023 and beyond.
if(maxyr >= 2023){
otosheet <- googledrive::drive_ls(path = id_googledrive_otos,
                                  pattern = paste0(SRVY, maxyr, "_otolith_targets"),
                                  type = "spreadsheet")

googledrive::drive_download(
  file = googledrive::as_id(otosheet$id),
  type = "csv",
  overwrite = TRUE,
  path = paste0("data/", otosheet$name)
)
} 

# Convert text files in gdrive directory into Rmd files -------------------

txtfiles <- list.files(path = paste0(dir_out_gdrive, "/"), pattern = ".txt")

for (i in 1:length(txtfiles)) {
  print(txtfiles[i])
  pandoc_convert(input = here::here("gdrive", paste(txtfiles[i])),
    to = "markdown",
    output = here::here(
      "gdrive",
      gsub(txtfiles[i], pattern = ".txt", replacement = ".Rmd")
    ),
    citeproc = TRUE # not sure if this is needed
  )
}
