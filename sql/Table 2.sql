drop table DataReportTable2;
create table DataReportTable2 as
SELECT * FROM (Select l.inpfc_area,i.summary_area,i.species_code,s.common_name,(i.mean_wgt_cpue/100) CPUE
from goa.biomass_inpfc i, racebase.species s, aigoa_work_data.goa_inpfc_lookup l
where i.species_code=s.species_code and i.summary_area=919 and l.min_depth=101 and i.summary_area=l.summary_area and year=2019 
order by i.summary_area,i.mean_wgt_cpue DESC)
where rownum<=20;

insert into DataReportTable2
SELECT * FROM (Select l.inpfc_area,i.summary_area,i.species_code,s.common_name,(i.mean_wgt_cpue/100) CPUE
from goa.biomass_inpfc i, racebase.species s, aigoa_work_data.goa_inpfc_lookup l
where i.species_code=s.species_code and i.summary_area=929 and l.min_depth=101 and i.summary_area=l.summary_area and year=2019 
order by i.summary_area,i.mean_wgt_cpue DESC)
where rownum<=20;

insert into DataReportTable2
SELECT * FROM (Select l.inpfc_area,i.summary_area,i.species_code,s.common_name,(i.mean_wgt_cpue/100) CPUE
from goa.biomass_inpfc i, racebase.species s, aigoa_work_data.goa_inpfc_lookup l
where i.species_code=s.species_code and i.summary_area=939 and l.min_depth=101 and i.summary_area=l.summary_area and year=2019 
order by i.summary_area,i.mean_wgt_cpue DESC)
where rownum<=20;

insert into DataReportTable2
SELECT * FROM (Select l.inpfc_area,i.summary_area,i.species_code,s.common_name,(i.mean_wgt_cpue/100) CPUE
from goa.biomass_inpfc i, racebase.species s, aigoa_work_data.goa_inpfc_lookup l
where i.species_code=s.species_code and i.summary_area=949 and l.min_depth=101 and i.summary_area=l.summary_area and year=2019 
order by i.summary_area,i.mean_wgt_cpue DESC)
where rownum<=20;

insert into DataReportTable2
SELECT * FROM (Select l.inpfc_area,i.summary_area,i.species_code,s.common_name,(i.mean_wgt_cpue/100) CPUE
from goa.biomass_inpfc i, racebase.species s, aigoa_work_data.goa_inpfc_lookup l
where i.species_code=s.species_code and i.summary_area=959 and l.min_depth=101 and i.summary_area=l.summary_area and year=2019 
order by i.summary_area,i.mean_wgt_cpue DESC)
where rownum<=20;

insert into DataReportTable2
SELECT * FROM (Select l.inpfc_area,i.summary_area,i.species_code,s.common_name,(i.mean_wgt_cpue/100) CPUE
from goa.biomass_total i, racebase.species s, aigoa_work_data.goa_inpfc_lookup l
where i.species_code=s.species_code and i.summary_area=929 and l.min_depth=101 and i.summary_area=l.summary_area and year=2019 
order by i.summary_area,i.mean_wgt_cpue DESC)
where rownum<=20;
