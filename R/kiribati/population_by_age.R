#Directory path
repository <- file.path(dirname(rstudioapi::getSourceEditorContext()$path))
setwd(repository)

# Load the general R functions
source("../functions/setup.R")
source("kiribati_census2020.R")

#Division population
population_div <- PP_KIR20 |> 
  group_by(divID, urbrur, household_type, citizen_type, age_grp5yr, sex) |> 
  summarise(totpop = n())

#Convert table to data table format
population_div <- as.data.table(population_div)

#Create cube table
population_div_cube <- cube(population_div, j = sum(totpop), by = c("divID", "urbrur", "household_type", "citizen_type",  "age_grp5yr", "sex"), id = FALSE)

population_div_cube <- population_div_cube |>
  mutate(divID = ifelse(is.na(divID), "KI", divID),
         urbrur = ifelse(is.na(urbrur), "_T", urbrur),
         household_type = ifelse(is.na(household_type), "_T", household_type),
         citizen_type = ifelse(is.na(citizen_type), "_T", citizen_type),
         age_grp5yr = ifelse(is.na(age_grp5yr), "_T", age_grp5yr),
         sex = ifelse(is.na(sex), "_T", sex),
         INDICATOR = "PPCNT") |>
rename(GEO_PICT = divID, SEX = sex, URBANIZATION = urbrur, HOUSEHOLD_TYPE = household_type, CITIZEN_TYPE = citizen_type, AGE = age_grp5yr, OBS_VALUE = V1)


#Island population
population_isl <- PP_KIR20 |> 
  group_by(islID, urbrur, household_type, citizen_type, age_grp5yr, sex) |> 
  summarise(totpop = n())

#Convert table to data table format
population_isl <- as.data.table(population_isl)

#Create cube table
population_isl_cube <- cube(population_isl, j = sum(totpop), by = c("islID", "urbrur", "household_type", "citizen_type",  "age_grp5yr", "sex"), id = FALSE)
population_isl_cube <- population_isl_cube[!is.na(population_isl_cube$islID)]

population_isl_cube <- population_isl_cube |>
  mutate(urbrur = ifelse(is.na(urbrur), "_T", urbrur),
         household_type = ifelse(is.na(household_type), "_T", household_type),
         citizen_type = ifelse(is.na(citizen_type), "_T", citizen_type),
         age_grp5yr = ifelse(is.na(age_grp5yr), "_T", age_grp5yr),
         sex = ifelse(is.na(sex), "_T", sex),
         INDICATOR = "PPCNT") |>
  rename(GEO_PICT = islID, SEX = sex, URBANIZATION = urbrur, HOUSEHOLD_TYPE = household_type, CITIZEN_TYPE = citizen_type, AGE = age_grp5yr, OBS_VALUE = V1)

combine_table <- rbind(population_div_cube, population_isl_cube)

#Add other fields and re-arrange the columns in proper order
combine_table <- combine_table |>
  mutate(
    FREQ = "A", TIME_PERIOD = 2020, UNIT_MEASUREE = "N", UNIT_MULT = "",
    DATA_SOURCE = "Population and Housing Census", OBS_STATUS = "", OBS_COMMENT = "", CONF_STATUS ="")

combine_table <- combine_table |>
  select(FREQ, TIME_PERIOD, GEO_PICT, INDICATOR, URBANIZATION, HOUSEHOLD_TYPE, CITIZEN_TYPE,  AGE, SEX, OBS_VALUE, UNIT_MEASUREE, UNIT_MULT, DATA_SOURCE, OBS_STATUS, OBS_COMMENT, CONF_STATUS)

#Write final table to csv file
write.csv(combine_table, "../../output/population_by_age.csv", row.names = FALSE)
