/*****************************************************************************************
 * Supporting queries are found below.  Instantiate the following view after changing the Cruise, Year,
 * and Vessels used.  The resulting view will compile the Cruise-of-interest data for Table 1 of the Data Report
 * roughly in the format needed for the final table.  Then user must go to Excel and exercise Microsoft 
 * Query to bring the data into a work-sheet and paste results into the preformatted table 
 * (e.g., see G:\GOA\GOA 2011\Data Report\Table 1\
 * Table_1.xlsx).
 *
 * Now what should really happen is that the SQL below should be ported into an R function so that it is 
 * more flexible (e.g., prompts user for Cruise and Vessels) and so that the station allocation CSV can be
 * directly read into R rather than SQLLDR'd into Oracle.  I.E., use portion of view-building query below to 
 * generate all but the station allocation data.  Read that into a separate object from the CSV.  Massage
 * those data into shape so it can be merged with the Oracle-retrieved data.  Then play around with creating
 * an output table in the format required for the final data report.
 *
 ******************************************************************************************/
 
create or replace view DATA_REPORT_TABLE_1 as 
select  a.INPFC_AREA,
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
                  where     SURVEY = 'GOA'
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
                    where   B.REGION = 'GOA' and 
                               B.REGION = A.SURVEY and
                               CRUISE = 201701 and 
                               VESSEL in (143,148) and
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
                     count(*) STATION_COUNT     --count of all stationid/strata identities attempted
            from (
                    select  INPFC_AREA,
                             SUMMARY_AREA_DEPTH,
                             MIN_DEPTH|| ' - ' ||MAX_DEPTH DEPTH_RANGE
                    from   GOA.GOA_STRATA a, (
                                                            select            REGION,     -- returns all stations sampled regardless of success
                                                                               -- VESSEL,
                                                                               CRUISE,
                                                                               STATIONID,
                                                                               STRATUM
                                                            from    RACEBASE.HAUL
                                                            where  REGION = 'GOA' and 
                                                                      CRUISE = 201701 and 
                                                                      VESSEL in (143,148) and 
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
                    where   B.SURVEY = 'GOA'  
                               and B.SURVEY = A.SURVEY
                               and A.STRATUM = B.STRATUM
                               and B.YEAR = 2017
            ) 
            group by INPFC_AREA,DEPTH_RANGE,SUMMARY_AREA_DEPTH
            ) d
where   A.INPFC_AREA = B.INPFC_AREA and B.INPFC_AREA = C.INPFC_AREA and C.INPFC_AREA = D.INPFC_AREA and
           A.DEPTH_RANGE = B.DEPTH_RANGE and B.DEPTH_RANGE = C.DEPTH_RANGE and C.DEPTH_RANGE = D.DEPTH_RANGE and
           A.SUMMARY_AREA_DEPTH = B.SUMMARY_AREA_DEPTH and B.SUMMARY_AREA_DEPTH = C.SUMMARY_AREA_DEPTH and
           C.SUMMARY_AREA_DEPTH = D.SUMMARY_AREA_DEPTH
order by A.SUMMARY_AREA_DEPTH,A.DEPTH_RANGE
/

-- Testing scripts below this line; not necessary for creating Table 1 data.

/* calculate total area (km2) in inpfc area stratum */

select inpfc_area,depth_range,round(sum(area),0) area,summary_area_depth
from (
select inpfc_area, min_depth|| ' - ' ||max_depth depth_range, area, summary_area_depth
from goa.goa_strata
where survey = 'AI'
)
group by inpfc_area,depth_range,summary_area_depth
order by summary_area_depth,depth_range
/

/* count up total number of successful stations */

select inpfc_area,depth_range,summary_area_depth,count(*) HAUL_COUNT
from (
select inpfc_area, min_depth|| ' - ' ||max_depth depth_range,
summary_area_depth,vessel,cruise,haul
from goa.goa_strata a, racebase.haul b
where b.region = 'AI' and 
b.region = a.survey and
cruise = 200901 and vessel in (143,94,159) and
a.stratum = b.stratum and b.performance >= 0 and b.haul_type = 3
)
group by inpfc_area,depth_range,summary_area_depth
order by summary_area_depth,depth_range
/
-- total successful stations in GOA 2009 = 823

/* count up number of stations attempted */

select inpfc_area,summary_area_depth,depth_range,count(*) STATION_COUNT
from (
select inpfc_area,summary_area_depth,min_depth|| ' - ' ||max_depth depth_range
from goa.goa_strata a, (
select distinct region,vessel,cruise,stationid,stratum
from racebase.haul
where region = 'AI' and cruise = 200901 and vessel in (143,94,159) and haul_type = 3
) b
where b.region = a.survey and a.stratum = b.stratum
)
group by inpfc_area,depth_range,summary_area_depth
order by summary_area_depth,depth_range
/
-- total stations attempted in GOA 2009 = 851


/* to get station allocation into Oracle to get last component of table
 * used SQLLDR to upload a CSV to the following Oracle table
 */

create table GOA09_STATION_ALLOCATION (
VESSEL NUMBER(2),
STATIONID VARCHAR2(10),
STRATUM NUMBER(3),
LATITUDE NUMBER,
LONGITUDE NUMBER)
/

/* count of number of stations allocated within fishing area and stratum depth */

select inpfc_area,summary_area_depth,depth_range,count(*) STATIONS_ALLOCATED
from (
select inpfc_area,summary_area_depth,min_depth|| ' - ' ||max_depth depth_range
from goa.goa_strata a, goa.goa09_station_allocation b
where survey = 'AI' and 
a.stratum = b.stratum
) 
group by inpfc_area,depth_range,summary_area_depth
order by summary_area_depth,depth_range
/
-- total stations allocated by area and stratum in GOA 09 = 825