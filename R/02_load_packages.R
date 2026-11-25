# Load packages
# Install packages --------------------------------------------------------
# List and load required packages -----------------------------------------

PKG <- c(
  "devtools",
  "git2r",

  # Markdown
  "knitr",
  "rmarkdown",
  "magick",

  # Graphics
  "ggplot2",
  "patchwork",
  "png",
  "broom",
  "ggpubr",
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
  "rnaturalearth",
  "rnaturalearthdata",

  # check website links
  "pingr",
  "httr",

  # Tables
  "officer",
  "flextable",
  "fuzzyjoin",
  "rlang",
  "kableExtra",
  "forcats", # for the % change table

  # For editing XML files
  "XML",

  # Oracle
  "RODBC",
  "getPass"
)


PKG <- unique(PKG)
for (p in PKG) {
  if (!require(p, character.only = TRUE)) {
    install.packages(p)
    require(p, character.only = TRUE)
  }
}


# Packages in development (require devtools) ------------------------------
if (!"coldpool" %in% installed.packages()) {
  remotes::install_github("afsc-gap-products/coldpool")
}

if (!"akgfmaps" %in% installed.packages()) {
  message("Installing akgfmaps to 4.2.1 or higher...")
  remotes::install_github("afsc-gap-products/akgfmaps", build_vignettes = TRUE)
}

if (packageVersion("akgfmaps") < "4.2.1") {
  message("Updating akgfmaps to 4.2.1 or higher...")
  remotes::install_github("afsc-gap-products/akgfmaps", build_vignettes = TRUE)
}

if (!"knitcitations" %in% installed.packages()) {
  remotes::install_github("cboettig/knitcitations")
}

if (!"gapindex" %in% installed.packages()) {
  remotes::install_github("afsc-gap-products/gapindex")
}

# Confirm updated version of gapindex
if (packageVersion("gapindex") < "3.1.0") {
  message("Updating gapindex to 3.1.0 or higher...")
  pak::pak("afsc-gap-products/gapindex")
}

# Check install and load bespoke GAP packages
# RACE-GAP Specific
PKG2 <- c(
  "akgfmaps",
  "coldpool",
  "gapindex"
)

for (p in PKG2) {
  if (!require(p, character.only = TRUE)) {
    install.packages(p)
    require(p, character.only = TRUE)
  }
}

# Cite R Packages --------------------------------------------------------
knitr::write_bib(
  x = c(PKG, PKG2),
  file = paste0(dir_cite, "bibliography_RPack.bib")
)
