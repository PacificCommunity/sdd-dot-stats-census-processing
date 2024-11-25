#Load libraries

#library(dplyr)
#library(data.table)

#Map working directories

repository <- file.path(dirname(rstudioapi::getSourceEditorContext()$path))
setwd(repository)

# Load the general R functions
source("../functions/setup.R")
source("kiribati_census2020.R")


# ******************************* Population processing *****************************************************

#Population by country
pop <- PP_KIR20 |>
  group_by(country, age, sex) |>
  summarise(OBS_VALUE = n())

pop_dat <- as.data.table(pop)
pop_dat_cube <- cube(pop_dat, j = round(sum(OBS_VALUE),0), by = c("country", "sex", "age"), id = FALSE)

pop_dat_cube <- pop_dat_cube[!is.na(pop_dat_cube$country)]

pop_dat_cube <- pop_dat_cube |>
  mutate(sex = ifelse(is.na(sex), "_T", sex),
         age = ifelse(is.na(age), "_T", age),
         INDICATOR = "POPCNT"
         ) |>
  rename(OBS_VALUE = V1,
         AGE = age,
         SEX = sex,
         GEO_PICT = country
         )

#Population by province

pop_prov <- PP_KIR20 |>
  group_by(divID, age, sex) |>
  summarise(OBS_VALUE = n())

pop_prov_dat <- as.data.table(pop_prov)  

pop_prov_dat_cube <- cube(pop_prov_dat, j = round(sum(OBS_VALUE),0), by = c("divID", "sex", "age"), id = FALSE)  
pop_prov_dat_cube <- pop_prov_dat_cube[!is.na(pop_prov_dat_cube$divID)]

pop_prov_dat_cube <- pop_prov_dat_cube |>
  mutate(sex = ifelse(is.na(sex), "_T", sex),
         age = ifelse(is.na(age), "_T", age),
         INDICATOR = "POPCNT"
  ) |>
  rename(OBS_VALUE = V1,
         AGE = age,
         SEX = sex,
         GEO_PICT = divID
  )

#Population by island

pop_isl <- PP_KIR20 |>
  group_by(islID, age, sex) |>
  summarise(OBS_VALUE = n())

pop_isl_dat <- as.data.table(pop_isl)  

pop_isl_dat_cube <- cube(pop_isl_dat, j = round(sum(OBS_VALUE),0), by = c("islID", "sex", "age"), id = FALSE)  
pop_isl_dat_cube <- pop_isl_dat_cube[!is.na(pop_isl_dat_cube$islID)]

pop_isl_dat_cube <- pop_isl_dat_cube |>
  mutate(sex = ifelse(is.na(sex), "_T", sex),
         age = ifelse(is.na(age), "_T", age),
         INDICATOR = "POPCNT"
  ) |>
  rename(OBS_VALUE = V1,
         AGE = age,
         SEX = sex,
         GEO_PICT = islID
  )


#Rbind population tables

pop_final <- rbind(pop_dat_cube, pop_prov_dat_cube, pop_isl_dat_cube)


# ******************************* Household processing *****************************************************

hh_country <- PP_KIR20 |>
  filter(relat == "Head") |>
  group_by(country) |>
  rename(GEO_PICT = country) |>
  summarise(OBS_VALUE = n())

hh_prov <- PP_KIR20 |>
  filter(relat == "Head") |>
  group_by(divID) |>
  rename(GEO_PICT = divID) |>
  summarise(OBS_VALUE = n())


hh_isl <- PP_KIR20 |>
  filter(relat == "Head") |>
  group_by(islID) |>
  rename(GEO_PICT = islID) |>
  summarise(OBS_VALUE = n())

hh_final <- rbind(hh_country, hh_prov, hh_isl)

hh_final <- hh_final |>
  mutate(SEX = "_T", AGE = "_T", INDICATOR = "HHCNT")

# **************************** Combine both Population and Household **************************************

population_structure <- rbind(pop_final, hh_final)

#Add the rest of the fields
population_structure <- population_structure |>
  mutate(FREQ = "A", 
         TIME_PERIOD = "2020", 
         UNIT_MEASURE = "N", 
         UNIT_MULT = "", 
         DATA_SOURCE = "Population and Housing Census", 
         OBS_STATUS = "", 
         OBS_COMMENT = "",
         CONF_STATUS = ""
         )


#Reorder the fields in the proper order

population_structure <- population_structure |>
  select(FREQ,
         TIME_PERIOD,
         GEO_PICT,
         AGE,
         SEX,
         INDICATOR,
         OBS_VALUE,
         UNIT_MEASURE,
         UNIT_MULT,
         DATA_SOURCE,
         OBS_STATUS,
         OBS_COMMENT,
         CONF_STATUS
         
         )

#Write the table to csv file in output folder

write.csv(population_structure, "../../output/population_structure.csv", row.names = FALSE)


























