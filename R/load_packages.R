# Load packages
# Install packages --------------------------------------------------------

# Packages in development (require devtools) -----------------------------------

if(!"nmfspalettes" %in% installed.packages()){
  devtools::install_github("nmfs-general-modeling-tools/nmfspalette")
}

if(!"coldpool" %in% installed.packages()){
  devtools::install_github("afsc-gap-products/coldpool")
}

if(!"akgfmaps" %in% installed.packages()){
  devtools::install_github("sean-rohan-noaa/akgfmaps", build_vignettes = TRUE)
}

if(!"knitcitations" %in% installed.packages()){
  devtools::install_github("cboettig/knitcitations")
}



# List and load required packages -----------------------------------------

PKG <- c(
  "devtools",
  
  # Markdown
  "knitr", # A general-purpose tool for dynamic report generation in R
  "rmarkdown", 
  
  # Graphics
  "ggplot2", 
  "cowplot",
  "png",
  "extrafont",
  "ggpubr",
  "nmfspalette",
  "ggridges",
  
  # Citations
  "knitcitations", # devtools::install_github("cboettig/knitcitations")
  
  # other tidyverse
  "dplyr",
  "googledrive",
  "readr",
  "tidyr",
  
  # Text Management
  "stringr",
  "readtext",
  
  # RACE-GAP Specific
  "akgfmaps", # devtools::install_github("sean-rohan-noaa/akgfmaps", build_vignettes = TRUE)
  "coldpool", # devtools::install_github("afsc-gap-products/coldpool")
  
  # Spatial
  "sf",
  "rlist", 
  "jsonlite", 
  "prettymapr",
  "rgdal", 
  "rosm", 
  "shadowtext", 
  "ggspatial", 
  "digest", 
  "ggsn",
  "rgdal", 
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
  
  # check website links
  "pingr",
  "httr",
  
  # Tables
  "officer", 
  "flextable", 
  
  # For editing XML files
  "XML", 
  
  # Oracle
  "RODBC")


PKG <- unique(PKG)
for (p in PKG) {
  if(!require(p,character.only = TRUE)) {
    install.packages(p)
    require(p,character.only = TRUE)}
}





loadfonts(device = "win")
