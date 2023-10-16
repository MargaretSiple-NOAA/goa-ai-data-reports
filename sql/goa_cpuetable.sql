define xsurvey_area = 'GOA'

define b = 'goa.biomass_'
define sz = 'goa.sizecomp_'
define in = 'inpfc'
define in2 = 'inpfc_'
define t = 'total'
define d = 'depth'
define cpue = 'goa.cpue'
define bindy = &b&in2&d
define bdy = &b&d
define biny = &b&in
define bty = &b&t
define szindy = &sz&in2&d
define szdy = &sz&d
define sziny = &sz&in
define szty = &sz&t
define cpuey = &cpue

drop table summary_areas1;

create table summary_areas1 as select distinct summary_area_depth summary_area 
	from goa.goa_strata where survey = upper('&xsurvey_area') and 
	stratum in (select distinct stratum from &cpuey);

insert into summary_areas1 select distinct summary_area from goa.goa_strata
	where survey = upper('&xsurvey_area') and 
	stratum in (select distinct stratum from &cpuey);

insert into summary_areas1 select distinct summary_depth from goa.goa_strata 
	where survey = upper('&xsurvey_area') and 
	stratum in (select distinct stratum from &cpuey);
	
insert into summary_areas1 values (999);

drop table summary_areas;

create table summary_areas as select summary_area, species_code 
	from summary_areas1 s, goa.analysis_species g
	where biomass_flag = upper('&xsurvey_area') or biomass_flag = 'BOTH';
	
drop table biomass_tab;

create table biomass_tab as
select b.species_code, b.summary_area_depth summary_area, haul_count, catch_count, round(b.mean_wgt_cpue, 0) mean_wgt_cpue,
	area_biomass, min_biomass, max_biomass, round((area_biomass*1000)/area_pop,3) weight
	from &bindy b, &szindy s
	where b.year = &xyear and s.year = &xyear
	and b.summary_area_depth = s.summary_area_depth(+)
	and b.species_code = s.species_code(+) and area_pop > 0
	group by b.species_code, b.summary_area_depth, haul_count, catch_count, mean_wgt_cpue,
	area_biomass, min_biomass, max_biomass, area_pop;

insert into biomass_tab select b.species_code, b.summary_depth, haul_count, catch_count, 
	round(b.mean_wgt_cpue, 0) mean_wgt_cpue, area_biomass, min_biomass, max_biomass, 
  round((area_biomass*1000)/area_pop,3) weight
	from &bdy b, &szdy s
	where b.year = &xyear and s.year = &xyear
     and b.summary_depth = s.summary_depth(+)
	and b.species_code = s.species_code(+) and area_pop > 0
	group by b.species_code, b.summary_depth, haul_count, catch_count, mean_wgt_cpue,
	area_biomass, min_biomass, max_biomass, area_pop;

insert into biomass_tab select b.species_code, b.summary_area, haul_count, catch_count, 
	round(b.mean_wgt_cpue, 0) mean_wgt_cpue, area_biomass, min_biomass, max_biomass, 
  round((area_biomass*1000)/area_pop,3) weight
	from &biny b, &sziny s
	where b.year = &xyear and s.year = &xyear
     and b.summary_area = s.summary_area(+)
	and b.species_code = s.species_code(+) and area_pop > 0
	group by b.species_code, b.summary_area, haul_count, catch_count, mean_wgt_cpue,
	area_biomass, min_biomass, max_biomass, area_pop;

drop table bty;

create table bty as select * from &bty;

update bty set total_pop = 1 where total_pop = 0;

insert into biomass_tab select b.species_code, 999 total_area, 
	haul_count, catch_count, 
	round(b.mean_wgt_cpue, 0) mean_wgt_cpue, total_biomass, min_biomass, max_biomass, 
	round((total_biomass*1000)/total_pop,3) weight
	from bty b, &szty s
	where b.year = &xyear and s.year = &xyear
	and b.species_code = s.species_code(+)
	group by b.species_code, haul_count, catch_count, mean_wgt_cpue,
	total_biomass, min_biomass, max_biomass, total_pop;
	
drop table haul_count;

create table haul_count as select distinct summary_area, haul_count from biomass_tab;

drop table table_biomass;

create table table_biomass as select s.summary_area, s.species_code, 
	ltrim(to_char(h.haul_count,'990')) haul_count,
	ltrim(to_char(catch_count,'990')) catch_count, 
	ltrim(to_char(mean_wgt_cpue, '999,999,990'), ' ') mean_wgt_cpue, 
	ltrim(to_char(area_biomass, '999,999,990'), ' ') area_biomass,
  ltrim(to_char(min_biomass, '999,999,990'), ' ') min_biomass, 
  ltrim(to_char(max_biomass, '999,999,990'), ' ') max_biomass, 
  ltrim(to_char(weight, '990.000'), ' ') weight
	from biomass_tab b, summary_areas s, haul_count h
	where s.summary_area = b.summary_area(+)
	and s.species_code = b.species_code(+)
	and h.summary_area(+) = s.summary_area;
	
update table_biomass set haul_count = '0' where haul_count is null;
update table_biomass set catch_count = '0' where catch_count is null;
update table_biomass set mean_wgt_cpue = '---' where mean_wgt_cpue is null;
update table_biomass set area_biomass = '---' where area_biomass is null;
update table_biomass set min_biomass = '---' where min_biomass is null;
update table_biomass set max_biomass = '---' where max_biomass is null;
update table_biomass set weight = '---' where weight is null;
update table_biomass set mean_wgt_cpue = '<1' where mean_wgt_cpue = '0';
update table_biomass set area_biomass = '<1' where area_biomass = '0';
update table_biomass set weight = '<0.001' where weight = '0.000';

drop table biomass_tab;

drop table bty;

drop table summary_areas1;

drop table summary_areas;
