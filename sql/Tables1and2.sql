select * from table_1_data;

select distinct 
    c.inpfc_area, 
    b.summary_area, 
    common_name, 
    round(b.mean_wgt_cpue/100,2) cpue
from 
    racebase.species s, 
    ai.biomass_inpfc b, 
    goa.goa_strata c
where 
    s.species_code = b.species_code and
    b.year = 2014 and 
    b.summary_area in (select distinct summary_area from goa.goa_strata where survey = 'AI') and 
    b.summary_area = c.summary_area
order by b.summary_area,-round(b.mean_wgt_cpue/100,2)
/

select common_name,
round(mean_wgt_cpue/100,2) cpue
from ai.biomass_total a, 
racebase.species b
where a.survey = 'AI' 
and a.year = 2014
and a.species_code = b.species_code
order by -a.mean_wgt_cpue
/

select distinct 
    b.regulatory_area_name, 
    common_name, 
    round(b.mean_wgt_cpue/100,2) cpue
from 
    racebase.species s, 
    ai.biomass_area b
where 
    s.species_code = b.species_code and
    b.year = 2014 and 
    b.regulatory_area_name = 'ALEUTIANS'
order by -round(b.mean_wgt_cpue/100,2)
/
