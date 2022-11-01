# 2018 CPUE test
source("R/setup_channel.R")

# Download 0-filled cpue tables from RACEBASE_FOSS
a <- RODBC::sqlQuery(channel, "SELECT * FROM RACEBASE_FOSS.FOSS_ZEROFILLED")
write.csv(x = a, "./data/local_ai/foss_cpue.csv", row.names = FALSE)

x <- read.csv(here::here("data","local_ai","foss_cpue.csv"))
ai <- filter(x, SRVY=="AI")
head(ai)

ai2 <- ai %>% 
  filter(YEAR==2018) %>%
  left_join(haul,by="HAULJOIN") %>%
  dplyr::select(SRVY, SPECIES_CODE, WEIGHT, NUMBER_FISH, AREA_SWEPT_HA, CPUE_KGHA, CPUE_NOHA, HAUL ,HAUL_TYPE ,STRATUM, START_LATITUDE, END_LATITUDE) %>%
  mutate_at("CPUE_KGHA", ~replace(., is.na(.), 0))

head(ai2)

cpue_by_district_foss <- ai2  %>%
      dplyr::group_by(SPECIES_CODE) %>%
      dplyr::summarize(meancpue_kgha = mean(CPUE_KGHA, na.rm=T)) %>%
      ungroup() %>%
      arrange(-meancpue_kgha)
