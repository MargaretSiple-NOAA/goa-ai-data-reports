--to help figure out count of multi species survey
select distinct cruise, gear, accessories
from racebase.haul
where region = 'GOA'
and abundance_haul = 'Y'
order by cruise, accessories
/

select distinct cruise, gear, accessories
from racebase.haul
where region = 'GOA'
and abundance_haul = 'Y'
and gear = 172
and accessories in (64, 129)
order by cruise, accessories
/