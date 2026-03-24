select floor(cruise/100) year, avg(gear_temperature) mean_temp
from racebase.haul
where region = 'GOA'
and abundance_haul = 'Y'
group by floor(cruise/100)
order by floor(cruise/100)
/