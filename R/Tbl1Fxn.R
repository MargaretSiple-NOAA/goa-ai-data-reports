source(here::here("R/setup_channel.R"))

GetTbl1Data <- function(){

Survey <- readline("Enter Survey Designation (e.g., GOA or AI):  "); Survey <- toupper(Survey)
Year <- readline("Enter Survey Year (e.g., 2013):  ")

# ERROR HANDLING in case wrong survey designation is entered
	while(!(Survey %in% c("AI","GOA","EBSSLOPE"))){
		cat("\nWrong Survey Designation\nTry again...\n", sep = "")
		Survey <- readline("Enter Survey Designation (e.g., GOA or AI):  ")
		Survey <- toupper(Survey)
		}

#library(RODBC)

#channel <- odbcConnect(dsn = "AFSC")

Cruise <- as.numeric(paste0(Year,"01"))

vessels <- RODBC::sqlQuery(query = paste0("select distinct vessel from racebase.haul where REGION = '", Survey, "' and CRUISE = ", Cruise ," and VESSEL not in (21,157)"), channel = channel, believeNRows = FALSE)

tbl.1.data <- sqlQuery("select a.INPFC_AREA,
         a.DEPTH_RANGE,
         STATIONS_ALLOCATED,
         STATION_COUNT STATIONS_ATTEMPTED,
         HAUL_COUNT STATIONS_COMPLETED,
         AREA,
         round(((HAUL_COUNT/AREA)*1000),2) SAMPLING_DENSITY,
         A.SUMMARY_AREA_DEPTH
from   (
          select    INPFC_AREA,
                      DEPTH_RANGE,
                      round(sum(AREA),0) AREA,      -- sum by stratum within a fishing area and depth range
                      SUMMARY_AREA_DEPTH
          from (
                  select    INPFC_AREA, 
                              MIN_DEPTH|| ' - ' ||MAX_DEPTH DEPTH_RANGE, 
                              AREA, 
                              SUMMARY_AREA_DEPTH
                  from      GOA.GOA_STRATA
                  where     SURVEY = 'AI'
                  )
          group by INPFC_AREA,DEPTH_RANGE,SUMMARY_AREA_DEPTH
          ) a,
          (
          select    INPFC_AREA,
                     DEPTH_RANGE,
                     SUMMARY_AREA_DEPTH,
                     count(*) HAUL_COUNT        -- count of hauls successfully completed during survey
          from (
                    select  INPFC_AREA, 
                              MIN_DEPTH|| ' - ' ||MAX_DEPTH DEPTH_RANGE,
                              SUMMARY_AREA_DEPTH,
                              VESSEL,
                              CRUISE,
                              HAUL
                    from    GOA.GOA_STRATA A, 
                              RACEBASE.HAUL B
                    where   B.REGION = 'AI' and 
                               B.REGION = A.SURVEY and
                               CRUISE = 202201 and 
                               VESSEL in (148,176) and
                               A.STRATUM = B.STRATUM and 
                               B.PERFORMANCE >= 0 and 
                               B.HAUL_TYPE = 3
            )
            group by INPFC_AREA,DEPTH_RANGE,SUMMARY_AREA_DEPTH
            ) b,
            (
            select  INPFC_AREA,
                     SUMMARY_AREA_DEPTH,
                     DEPTH_RANGE,
                     count(*) STATION_COUNT     --count of unique stationid/stratum identities attempted
            from (
                    select  INPFC_AREA,
                             SUMMARY_AREA_DEPTH,
                             MIN_DEPTH|| ' - ' ||MAX_DEPTH DEPTH_RANGE
                    from   GOA.GOA_STRATA a, (
                                                            select distinct REGION,     -- term distinct here returns unique stationid/stratum combos
                                                                               VESSEL,
                                                                               CRUISE,
                                                                               STATIONID,
                                                                               STRATUM
                                                            from    RACEBASE.HAUL
                                                            where  REGION = 'AI' and 
                                                                      CRUISE = 202201 and 
                                                                      VESSEL in (148,176) and 
                                                                      HAUL_TYPE = 3
                                                            ) b
                      where  B.REGION = A.SURVEY and 
                                A.STRATUM = B.STRATUM
                      )
                      group by INPFC_AREA,DEPTH_RANGE,SUMMARY_AREA_DEPTH
            ) c,
            (
            select  INPFC_AREA,
                      SUMMARY_AREA_DEPTH,
                      DEPTH_RANGE,
                      count(*) STATIONS_ALLOCATED       -- from initial station allocation undertaken during survey planning phase
            from (
                    select  INPFC_AREA,
                              SUMMARY_AREA_DEPTH,
                              MIN_DEPTH|| ' - ' ||MAX_DEPTH DEPTH_RANGE
                    from    GOA.GOA_STRATA a, 
                               AI.STATION_ALLOCATION b
                    where   A.SURVEY = 'AI' and 
                               A.SURVEY = B.SURVEY 
                               and A.STRATUM = B.STRATUM
                               and B.YEAR = 2022
            ) 
            group by INPFC_AREA,DEPTH_RANGE,SUMMARY_AREA_DEPTH
            ) d
where   A.INPFC_AREA = B.INPFC_AREA and B.INPFC_AREA = C.INPFC_AREA and C.INPFC_AREA = D.INPFC_AREA and
           A.DEPTH_RANGE = B.DEPTH_RANGE and B.DEPTH_RANGE = C.DEPTH_RANGE and C.DEPTH_RANGE = D.DEPTH_RANGE and
           A.SUMMARY_AREA_DEPTH = B.SUMMARY_AREA_DEPTH and B.SUMMARY_AREA_DEPTH = C.SUMMARY_AREA_DEPTH and
           C.SUMMARY_AREA_DEPTH = D.SUMMARY_AREA_DEPTH
order by A.SUMMARY_AREA_DEPTH,A.DEPTH_RANGE", channel = channel, believeNRows = FALSE)

return(list(tbl1=tbl.1.data,vessels=vessels))
# close(channel)
# 
# write.csv()

}
