data <- structure(
  list(id = c(
    "Brill", "Cod", "Cod", "Dab", "Flounder", "Flounder"
  ), SpeciesScientificName = c(
    "Scophthalmus rhombus",
    "Gadus morhua", "Gadus morhua", "Limanda limanda", "Platichthys flesus",
    "Platichthys flesus"
  ), subdivision = c(
    "22-32", "22-24, western Baltic stock",
    "25-32, eastern Baltic stock", "22-32", "22 and 23", "24 and 25"
  ), location = c(
    "Baltic Sea", "western Baltic Sea", "eastern Baltic Sea",
    "Baltic Sea", "Belt Seas and the Sound", "west of Bornholm and southwestern central Baltic"
  )),
  class = "data.frame", .Names = c("id", "SpeciesScientificName", "subdivision", "location"),
  row.names = c(NA, -6L)
)

ft <- data %>%
  flextable(col_keys = c("dummy_col", "SpeciesScientificName")) %>%
  flextable::mk_par(
    j = "dummy_col",
    value = as_paragraph(
      id,
      " (", as_i(SpeciesScientificName), ") in ",
      location,
      " (",
      as_chunk(location, props = fp_text_default(color = "red")), ")"
    )
  ) %>%
  set_header_labels(
    dummy_col = "whatever",
    SpeciesScientificName = "SpeciesScientificName"
  ) %>%
  set_table_properties(layout = "autofit")


print(ft, preview = "docx")
