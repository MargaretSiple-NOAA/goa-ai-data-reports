# 06a_appendix_a

# Load strata info --------------------------------------------------------

area <- read.csv("data/local_gap_products/area.csv")

# Confirm correct design year and survey ID
if (SRVY == "GOA") {
  area <- area |>
    filter(DESIGN_YEAR == ifelse(maxyr < 2025, 1984, 2025) & SURVEY_DEFINITION_ID == 47)
} else {
  area <- area |>
    filter(DESIGN_YEAR == 1980 & SURVEY_DEFINITION_ID == 52)
}

depth_totals <- area |>
  dplyr::filter(AREA_TYPE %in% c("STRATUM")) |>
  dplyr::mutate(DEPTH_RANGE = paste(DEPTH_MIN_M, "-", DEPTH_MAX_M)) |>
  dplyr::filter(DEPTH_RANGE != "701 - 1000 m") |>
  group_by(DEPTH_RANGE) |>
  summarize(AREA_KM2 = sum(AREA_KM2)) |>
  ungroup() |>
  mutate(AREA_NAME = "Subtotal")

grand_total <- data.frame(
  DEPTH_RANGE = paste0("1 - ", max(area$DEPTH_MAX_M, na.rm = T)),
  AREA_KM2 = sum(depth_totals$AREA_KM2),
  AREA_NAME = "Grand total"
)

a1 <- area |>
  dplyr::filter(AREA_TYPE %in% c("STRATUM", "DEPTH")) |>
  dplyr::mutate(DEPTH_RANGE = paste(DEPTH_MIN_M, "-", DEPTH_MAX_M)) |>
  dplyr::arrange(desc(AREA_TYPE), DEPTH_MIN_M, DEPTH_MAX_M, AREA_ID) |>
  dplyr::select(DEPTH_RANGE, AREA_ID, AREA_NAME, AREA_KM2) |>
  dplyr::filter(!grepl("Combined", AREA_NAME)) |>
  dplyr::filter(!grepl("All", AREA_NAME)) |>
  dplyr::filter(DEPTH_RANGE != "701 - 1000") |>
  bind_rows(grand_total) |>
  dplyr::mutate_at("AREA_KM2", function(x) format(round(x), big.mark = ",")) |>
  dplyr::rename(
    "Depth range (m)" = "DEPTH_RANGE",
    "Stratum number" = "AREA_ID",
    "Stratum name" = "AREA_NAME"
  )


# Table A2: summary codes for strata.  ------------------------------------
# THis is static, so I'm just loading it from saved data.
# if(SRVY=="GOA" & maxyr < 2025){
#   a2 <- read.csv("data/GOA_alllyrs_appendix_a2.csv")
# }
#
# if(SRVY=="AI"){
#   stratum_summaries <- area |>
#     dplyr::filter(AREA_TYPE %in% c("STRATUM")) |>
#     dplyr::mutate(DEPTH_RANGE = paste(DEPTH_MIN_M, "-", DEPTH_MAX_M)) |>
#     dplyr::select(AREA_ID, DEPTH_RANGE, AREA_NAME, AREA_KM2)
#
#   a2 <- area |>
#     dplyr::filter(AREA_TYPE %in% c("STRATUM")) |>
#     dplyr::mutate(DEPTH_RANGE = paste(DEPTH_MIN_M, "-", DEPTH_MAX_M)) |>
#     dplyr::select(AREA_ID, DEPTH_RANGE, AREA_NAME, AREA_KM2)
# }
#
