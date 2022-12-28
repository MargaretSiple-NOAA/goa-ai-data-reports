# This script requires an oracle connection

# Setup channel to connect to Oracle --------------------------------------

source("R/setup_channel.R")

# The setup_channel.R script sets up a channel using your Oracle username and pw.

# Which survey number is it?
(survnumber)
survnumber_sql <- RODBC::sqlQuery(channel, "select count(distinct cruise) survnumber
from race_data.cruises a, race_data.survey_definitions b, race_data.surveys c
where b.survey_name = 'Aleutian Islands Bottom Trawl Survey'
and a.survey_id = c.survey_id
and b.survey_definition_id = c.survey_definition_id")
