# 06c_appendix_c
# This borrows from Cecilia's length-weight-params script to fit LW params just for the species in this report


# Setup -------------------------------------------------------------------
# Same min number observed from the length-weight-params code
MinNumberObserved <- 2


# Haul table --------------------------------------------------------------
# This script uses specimen_maxyr and haul_maxyr from the prep_data.R script
nrow(haul_maxyr)
abundance_hauls <- subset(haul_maxyr, ABUNDANCE_HAUL == "Y")
nrow(abundance_hauls)

# Filter and rename cols in specimen data from RACEBASE -------------------
specimen_maxyr2 <- specimen_maxyr |>
  dplyr::rename(
    "LENGTH_MM" = "LENGTH", # units, people!!
    "WEIGHT_G" = "WEIGHT"
  ) |>
  dplyr::filter(SPECIES_CODE %in% report_species$species_code &
    LENGTH_MM > 0 & WEIGHT_G > 0)

# Want to only use data from abundance_haul==Y
specimen_maxyr_fit <- specimen_maxyr2 |>
  left_join(haul_maxyr, by = c("CRUISEJOIN", "HAULJOIN", "REGION", "VESSEL", "CRUISE", "HAUL", "YEAR")) |>
  dplyr::filter(ABUNDANCE_HAUL == "Y") |>
  dplyr::select(CRUISE, HAUL, SPECIMENID, SPECIES_CODE, LENGTH_MM, SEX, WEIGHT_G)

nrow(specimen_maxyr_fit)

# Empty table for storing params
length_weight_df <- predictions_by_species <- data.frame()


# Loop through species and fit lm to length-weight relationship -----------------

for (i in 1:nrow(report_species)) {
  sp_i <- report_species$species_code[i]
  df_sp <- subset(x = specimen_maxyr_fit, SPECIES_CODE == sp_i)
  n_obs <- nrow(df_sp)
  for (s in 1:3) {
    df_sp_sx <- subset(df_sp, SEX == s)
    n_obs_sx <- nrow(df_sp_sx)
    if (n_obs_sx > MinNumberObserved) {
      ## Fit length-weight relationship for each sex
      t_model <- lm(log(WEIGHT_G) ~ log(LENGTH_MM), data = df_sp_sx)
      t_summary <- summary(t_model)
      t_intercept <- exp(t_summary$coefficients[1])
      t_slope <- t_summary$coefficients[2]

      if (is.nan(t_slope)) t_slope <- 0
      t_r_squared <- t_summary$r.squared
      t_max.length <- max(df_sp_sx$LENGTH_MM)
      t_sex <- switch(s,
        `1` = "Male",
        `2` = "Female",
        `3` = "Unsexed"
      )

      length_weight_df <-
        rbind(
          length_weight_df,
          data.frame(
            species = sp_i,
            sex = t_sex,
            count = n_obs_sx,
            alpha.kg = t_intercept * 1000,
            beta = t_slope,
            r_squared = t_r_squared,
            max.length.mm = t_max.length,
            alpha.grams = t_intercept
          )
        )
    }

    print(ifelse(test = n_obs > MinNumberObserved,
      yes = paste0(
        sp_i, " completed (", i, " out of ",
        nrow(report_species), " species completed)"
      ),
      no = paste0("skipped ", sp_i, " predictions")
    ))
  } # /end sex loop

  ## Fit length-weight relationship for all individuals of species i
  if (n_obs > MinNumberObserved) {
    t_model <- lm(log(WEIGHT_G) ~ log(LENGTH_MM), data = df_sp)
    t_summary <- summary(t_model)
    t_intercept <- exp(t_summary$coefficients[1])
    t_slope <- t_summary$coefficients[2]

    if (is.nan(t_slope)) t_slope <- 0
    t_r_squared <- t_summary$r.squared
    t_max.length <- max(df_sp$LENGTH_MM)
    length_weight_df <-
      rbind(
        length_weight_df,
        data.frame(
          species = sp_i,
          sex = "All",
          count = n_obs,
          alpha.kg = t_intercept * 1000,
          beta = t_slope,
          r_squared = t_r_squared,
          max.length.mm = t_max.length,
          alpha.grams = t_intercept
        )
      )
  }
} # /end species loop

length_weight_report <- length_weight_df |>
  left_join(report_species, by = c("species" = "species_code")) |>
  dplyr::mutate(alpha.grams = round(alpha.grams,digits = 9)) |>
  dplyr::mutate_at(.vars = 'sex', .funs = tolower) |>
  dplyr::select(spp_name_informal, spp_name_scientific, sex, count, beta, r_squared, max.length.mm, alpha.grams)
