# Load packages
# Install packages --------------------------------------------------------
# Packages in development (require devtools) -----------------------------------

if(!"nmfspalette" %in% installed.packages()){
  devtools::install_github("nmfs-general-modeling-tools/nmfspalette")
}

if(!"coldpool" %in% installed.packages()){
  devtools::install_github("afsc-gap-products/coldpool")
}

if(!"akgfmaps" %in% installed.packages()){
  devtools::install_github("afsc-gap-products/akgfmaps", build_vignettes = TRUE)
}

if(!"knitcitations" %in% installed.packages()){
  devtools::install_github("cboettig/knitcitations")
}

if(!"PNWColors" %in% installed.packages()){
  devtools::install_github("jakelawlor/PNWColors")
}

# List and load required packages -----------------------------------------

PKG <- c(
  "devtools",
  
  # Markdown
  "knitr", 
  "rmarkdown", 
  
  # Graphics
  "ggplot2", 
  "patchwork",
  "png",
  "broom",
  "ggpubr",
  #"nmfspalette", #add back in if needed
  "ggridges",
  "RColorBrewer",
  "MetBrewer",
  "scales",
  "ggridges",
  "rcartocolor", 
  "ggdist",
  "qrcode",
  
  # Citations
  "knitcitations", # devtools::install_github("cboettig/knitcitations")
  
  # other tidyverse
  "dplyr",
  "googledrive",
  "readr",
  "tidyr",
  "purrr",
  
  # Text Management
  "stringr",
  "readtext",
  "officedown",
  "janitor",
  
  # RACE-GAP Specific
  "akgfmaps", 
  "coldpool", 
  
  # Spatial
  "sf",
  "rlist", 
  "jsonlite", 
  "prettymapr",
  "rosm", 
  "shadowtext", 
  "ggspatial", 
  "digest", 
  "ps", 
  "backports", 
  "callr", 
  "labeling", 
  "gstat", 
  "magrittr", 
  "raster", 
  "reshape", 
  "stars",
  "grid", 
  "dbscan",
  
  # check website links
  "pingr",
  "httr",
  
  # Tables
  "officer", 
  "flextable", 
  "fuzzyjoin",
  "rlang",
  # "kableExtra",
  # "formattable",
  
  # For editing XML files
  "XML", 
  
  # Oracle
  "RODBC",
  "getPass")


PKG <- unique(PKG)
for (p in PKG) {
  if(!require(p,character.only = TRUE)) {
    install.packages(p)
    require(p,character.only = TRUE)}
}



#loadfonts(device = "win")

# Cite R Packages --------------------------------------------------------

knitr::write_bib(
  x = PKG,
  file = paste0(dir_cite, "bibliography_RPack.bib")
)

