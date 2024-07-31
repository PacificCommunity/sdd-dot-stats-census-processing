#Directory path
repository <- file.path(dirname(rstudioapi::getSourceEditorContext()$path))
setwd(repository)

source("../functions/setup.R")

#### ************************** Household tables *********************************************** ####

#Division households
household_div <- PP_KIR20 |> 
  filter(relat == 'Head') |> 
  group_by(divID, sex) |> 
  summarise(tothh = n())

household_div <- as.data.table(household_div)

household_div_cube <- cube(household_div, j = sum(tothh), by = c("divID", "sex"), id = FALSE)
household_div_cube <- household_div_cube[!is.na(household_div_cube$divID)]

household_div_cube <- household_div_cube |>
  mutate(sex = ifelse(is.na(sex), "_T", sex),
         INDICATOR = "HHCNT") |>
  rename(GEO_PICT = divID, SEX = sex, OBS_VALUE = V1)

#Island households
household_isl <- PP_KIR20 |> 
  filter(relat == 'Head') |> 
  group_by(islID, sex) |> 
  summarise(tothh = n())

household_isl <- as.data.table(household_isl)

household_isl_cube <- cube(household_isl, j = sum(tothh), by = c("islID", "sex"), id = FALSE)

household_isl_cube <- household_isl_cube |>
  mutate(
    islID = ifelse(is.na(islID), "KI", islID),
    sex = ifelse(is.na(sex), "_T", sex),
    INDICATOR = "HHCNT") |>
  rename(GEO_PICT = islID, SEX = sex, OBS_VALUE = V1)

household_cube <- rbind(household_div_cube, household_isl_cube)


#### ************************* Population tables ************************************ ####

#Division Overall population
persons_div <- PP_KIR20 |> 
  group_by(divID, sex) |> 
  summarise(totpop = n())

persons_div <- as.data.table(persons_div)

persons_div_cube <- cube(persons_div, j = round(sum(totpop),0), by = c("divID", "sex"), id = FALSE)
persons_div_cube <- persons_div_cube[!is.na(persons_div_cube$divID)]
persons_div_cube <- persons_div_cube |>
  mutate(
    sex = ifelse(is.na(sex), "_T", sex),
    INDICATOR = "PPCNT") |>
  rename(GEO_PICT = divID, SEX = sex, OBS_VALUE = V1)

#Island Overall population
persons_ils <- PP_KIR20 |> 
  group_by(islID, sex) |> 
  summarise(totpop = n())

persons_ils <- as.data.table(persons_ils)

persons_ils_cube <- cube(persons_ils, j = round(sum(totpop),0), by = c("islID", "sex"), id = FALSE)

persons_ils_cube <- persons_ils_cube |>
  mutate(
    islID = ifelse(is.na(islID), "KI", islID),
    sex = ifelse(is.na(sex), "_T", sex),
    INDICATOR = "PPCNT") |>
  rename(GEO_PICT = islID, SEX = sex, OBS_VALUE = V1)

persons_cube <- rbind(persons_div_cube, persons_ils_cube)

#Population living in Private households in divisions

persons_ph_div <- PP_KIR20 |> 
  filter(housing_type == 'Private household') |>
  group_by(divID, sex) |> 
  summarise(totpop = n())

persons_ph_div <- as.data.table(persons_ph_div)

persons_ph_div_cube <- cube(persons_ph_div, j = round(sum(totpop),0), by = c("divID", "sex"), id = FALSE)
persons_ph_div_cube <- persons_ph_div_cube[!is.na(persons_ph_div_cube$divID)]

persons_ph_div_cube <- persons_ph_div_cube |>
  mutate(
    sex = ifelse(is.na(sex), "_T", sex),
    INDICATOR = "PPRVCNT") |>
  rename(GEO_PICT = divID, SEX = sex, OBS_VALUE = V1)

#Population living in Private households in islands

persons_ph_isl <- PP_KIR20 |> 
  filter(housing_type == 'Private household') |>
  group_by(islID, sex) |> 
  summarise(totpop = n())

persons_ph_isl <- as.data.table(persons_ph_isl)

persons_ph_isl_cube <- cube(persons_ph_isl, j = round(sum(totpop),0), by = c("islID", "sex"), id = FALSE)

persons_ph_isl_cube <- persons_ph_isl_cube |>
  mutate(
    islID = ifelse(is.na(islID), "KI", islID),
    sex = ifelse(is.na(sex), "_T", sex),
    INDICATOR = "PPRVCNT") |>
  rename(GEO_PICT = islID, SEX = sex, OBS_VALUE = V1)


persons_ph_cube <- rbind(persons_ph_div_cube, persons_ph_isl_cube)

#Population 15+ years living in private HHS in division

persons_15plus_div <- PP_KIR20 |> 
  filter(housing_type == 'Private household' & age >= 15) |>
  group_by(divID, sex) |> 
  summarise(totpop = n())

persons_15plus_div <- as.data.table(persons_15plus_div)

persons_15plus_div_cube <- cube(persons_15plus_div, j = round(sum(totpop),0), by = c("divID", "sex"), id = FALSE)
persons_15plus_div_cube <- persons_15plus_div_cube[!is.na(persons_15plus_div_cube$divID)]

persons_15plus_div_cube <- persons_15plus_div_cube |>
  mutate(
    sex = ifelse(is.na(sex), "_T", sex),
    INDICATOR = "15PLSCNT") |>
  rename(GEO_PICT = divID, SEX = sex, OBS_VALUE = V1)

#Population 15+ years living in private HHS in islands

persons_15plus_isl <- PP_KIR20 |> 
  filter(housing_type == 'Private household' & age >= 15) |>
  group_by(islID, sex) |> 
  summarise(totpop = n())

persons_15plus_isl <- as.data.table(persons_15plus_isl)

persons_15plus_isl_cube <- cube(persons_15plus_isl, j = round(sum(totpop),0), by = c("islID", "sex"), id = FALSE)

persons_15plus_isl_cube <- persons_15plus_isl_cube |>
  mutate(
    islID = ifelse(is.na(islID), "KI", islID),
    sex = ifelse(is.na(sex), "_T", sex),
    INDICATOR = "15PLSCNT") |>
  rename(GEO_PICT = islID, SEX = sex, OBS_VALUE = V1)

persons_15plus_cube <- rbind(persons_15plus_div_cube, persons_15plus_isl_cube)

#Population of children less than 15 years in divisions

persons_less15_div <- PP_KIR20 |> 
  filter(age < 15) |>
  group_by(divID, sex) |> 
  summarise(totpop = n())

persons_less15_div <- as.data.table(persons_less15_div)

persons_less15_div_cube <- cube(persons_less15_div, j = round(sum(totpop),0), by = c("divID", "sex"), id = FALSE)
persons_less15_div_cube <- persons_less15_div_cube[!is.na(persons_less15_div_cube$divID)]

persons_less15_div_cube <- persons_less15_div_cube |>
  mutate(
    sex = ifelse(is.na(sex), "_T", sex),
    INDICATOR = "LSS15CNT") |>
  rename(GEO_PICT = divID, SEX = sex, OBS_VALUE = V1)

#Population of children less than 15 years in islands

persons_less15_isl <- PP_KIR20 |> 
  filter(age < 15) |>
  group_by(islID, sex) |> 
  summarise(totpop = n())

persons_less15_isl <- as.data.table(persons_less15_isl)

persons_less15_isl_cube <- cube(persons_less15_isl, j = round(sum(totpop),0), by = c("islID", "sex"), id = FALSE)

persons_less15_isl_cube <- persons_less15_isl_cube |>
  mutate(
    islID = ifelse(is.na(islID), "KI", islID),
    sex = ifelse(is.na(sex), "_T", sex),
    INDICATOR = "LSS15CNT") |>
  rename(GEO_PICT = islID, SEX = sex, OBS_VALUE = V1)

persons_less15_cube <- rbind(persons_less15_div_cube, persons_less15_isl_cube)

#Population of children 15-24 years in divisions

persons_15TO24_div <- PP_KIR20 |> 
  filter(age >= 15 & age <= 24) |>
  group_by(divID, sex) |> 
  summarise(totpop = n())

persons_15TO24_div <- as.data.table(persons_15TO24_div)

persons_15TO24_div_cube <- cube(persons_15TO24_div, j = round(sum(totpop),0), by = c("divID", "sex"), id = FALSE)
persons_15TO24_div_cube <- persons_15TO24_div_cube[!is.na(persons_15TO24_div_cube$divID)]

persons_15TO24_div_cube <- persons_15TO24_div_cube |>
  mutate(
    sex = ifelse(is.na(sex), "_T", sex),
    INDICATOR = "15TO24CNT") |>
  rename(GEO_PICT = divID, SEX = sex, OBS_VALUE = V1)

#Population of children 15-24 years in islands

persons_15TO24_isl <- PP_KIR20 |> 
  filter(age >= 15 & age <= 24) |>
  group_by(islID, sex) |> 
  summarise(totpop = n())

persons_15TO24_isl <- as.data.table(persons_15TO24_isl)

persons_15TO24_isl_cube <- cube(persons_15TO24_isl, j = round(sum(totpop),0), by = c("islID", "sex"), id = FALSE)

persons_15TO24_isl_cube <- persons_15TO24_isl_cube |>
  mutate(
    islID = ifelse(is.na(islID), "KI", islID),
    sex = ifelse(is.na(sex), "_T", sex),
    INDICATOR = "15TO24CNT") |>
  rename(GEO_PICT = islID, SEX = sex, OBS_VALUE = V1)

persons_15TO24_cube <- rbind(persons_15TO24_div_cube, persons_15TO24_isl_cube)  


#Population of children 25-59 years in divisions

persons_25TO59_div <- PP_KIR20 |> 
  filter(age >= 25 & age <= 59) |>
  group_by(divID, sex) |> 
  summarise(totpop = n())

persons_25TO59_div <- as.data.table(persons_25TO59_div)

persons_25TO59_div_cube <- cube(persons_25TO59_div, j = round(sum(totpop),0), by = c("divID", "sex"), id = FALSE)
persons_25TO59_div_cube <- persons_25TO59_div_cube[!is.na(persons_25TO59_div_cube$divID)]

persons_25TO59_div_cube <- persons_25TO59_div_cube |>
  mutate(
    sex = ifelse(is.na(sex), "_T", sex),
    INDICATOR = "25TO59CNT") |>
  rename(GEO_PICT = divID, SEX = sex, OBS_VALUE = V1)

#Population of children 25-59 years in islands

persons_25TO59_isl <- PP_KIR20 |> 
  filter(age >= 25 & age <= 59) |>
  group_by(islID, sex) |> 
  summarise(totpop = n())

persons_25TO59_isl <- as.data.table(persons_25TO59_isl)

persons_25TO59_isl_cube <- cube(persons_25TO59_isl, j = round(sum(totpop),0), by = c("islID", "sex"), id = FALSE)

persons_25TO59_isl_cube <- persons_25TO59_isl_cube |>
  mutate(
    islID = ifelse(is.na(islID), "KI", islID),
    sex = ifelse(is.na(sex), "_T", sex),
    INDICATOR = "25TO59CNT") |>
  rename(GEO_PICT = islID, SEX = sex, OBS_VALUE = V1)

persons_25TO59_cube <- rbind(persons_25TO59_div_cube, persons_25TO59_isl_cube)


#Population of children 25-59 years in divisions

persons_60pluss_div <- PP_KIR20 |> 
  filter(age >= 60) |>
  group_by(divID, sex) |> 
  summarise(totpop = n())

persons_60pluss_div <- as.data.table(persons_60pluss_div)

persons_60pluss_div_cube <- cube(persons_60pluss_div, j = round(sum(totpop),0), by = c("divID", "sex"), id = FALSE)
persons_60pluss_div_cube <- persons_60pluss_div_cube[!is.na(persons_60pluss_div_cube$divID)]

persons_60pluss_div_cube <- persons_60pluss_div_cube |>
  mutate(
    sex = ifelse(is.na(sex), "_T", sex),
    INDICATOR = "60PLSCNT") |>
  rename(GEO_PICT = divID, SEX = sex, OBS_VALUE = V1)

#Population of children 25-59 years in islands

persons_60pluss_isl <- PP_KIR20 |> 
  filter(age >= 60) |>
  group_by(islID, sex) |> 
  summarise(totpop = n())

persons_60pluss_isl <- as.data.table(persons_60pluss_isl)

persons_60pluss_isl_cube <- cube(persons_60pluss_isl, j = round(sum(totpop),0), by = c("islID", "sex"), id = FALSE)

persons_60pluss_isl_cube <- persons_60pluss_isl_cube |>
  mutate(
    islID = ifelse(is.na(islID), "KI", islID),
    sex = ifelse(is.na(sex), "_T", sex),
    INDICATOR = "60PLSCNT") |>
  rename(GEO_PICT = islID, SEX = sex, OBS_VALUE = V1)

persons_60pluss_cube <- rbind(persons_60pluss_div_cube, persons_60pluss_isl_cube)


#Employment population by sex in divisions

employment_div <- PP_KIR20 |>
  filter(lf1 == 'Working for someone else for pay' | 
         lf1 == 'Working in own farming, raising animals or fishing' |
         lf1 == 'Working in any other kind of business activity' |
         lf1 == 'Doing an unpaid apprenticeship, internship' |
         lf1 == 'Doing unpaid voluntary, community, charity work'
         ) |>
  group_by(divID, sex) |>
  summarise(totpop = n())

employment_div <- as.data.table(employment_div)

employment_div_cube <- cube(employment_div, j = round(sum(totpop),0), by = c("divID", "sex"), id = FALSE)
employment_div_cube <- employment_div_cube[!is.na(employment_div_cube$divID)]

employment_div_cube <- employment_div_cube |>
  mutate(
    sex = ifelse(is.na(sex), "_T", sex),
    INDICATOR = "EMPCNT") |>
  rename(GEO_PICT = divID, SEX = sex, OBS_VALUE = V1)
  
#Employment population by sex in islands

employment_isl <- PP_KIR20 |>
  filter(lf1 == 'Working for someone else for pay' | 
           lf1 == 'Working in own farming, raising animals or fishing' |
           lf1 == 'Working in any other kind of business activity' |
           lf1 == 'Doing an unpaid apprenticeship, internship' |
           lf1 == 'Doing unpaid voluntary, community, charity work'
  ) |>
  group_by(islID, sex) |>
  summarise(totpop = n())

employment_isl <- as.data.table(employment_isl)

employment_isl_cube <- cube(employment_isl, j = round(sum(totpop),0), by = c("islID", "sex"), id = FALSE)

employment_isl_cube <- employment_isl_cube |>
  mutate(
    islID = ifelse(is.na(islID), "KI", islID),
    sex = ifelse(is.na(sex), "_T", sex),
    INDICATOR = "EMPCNT") |>
  rename(GEO_PICT = islID, SEX = sex, OBS_VALUE = V1)


employment_cube <- rbind(employment_div_cube, employment_isl_cube)


#Subsistence employment by sex in divisions

employment_subsistance_div <- PP_KIR20 |>
  filter(  lf1 == 'Working in own farming, raising animals or fishing') |>
  group_by(divID, sex) |>
  summarise(totpop = n())

employment_subsistance_div <- as.data.table(employment_subsistance_div)

employment_subsistance_div_cube <- cube(employment_subsistance_div, j = round(sum(totpop),0), by = c("divID", "sex"), id = FALSE)
employment_subsistance_div_cube <- employment_subsistance_div_cube[!is.na(employment_subsistance_div_cube$divID)]

employment_subsistance_div_cube <- employment_subsistance_div_cube |>
  mutate(
    sex = ifelse(is.na(sex), "_T", sex),
    INDICATOR = "SUBEMPCNT") |>
  rename(GEO_PICT = divID, SEX = sex, OBS_VALUE = V1)


#Subsistence employment by sex is islands

employment_subsistance_isl <- PP_KIR20 |>
  filter(  lf1 == 'Working in own farming, raising animals or fishing') |>
  group_by(islID, sex) |>
  summarise(totpop = n())

employment_subsistance_isl <- as.data.table(employment_subsistance_isl)

employment_subsistance_isl_cube <- cube(employment_subsistance_isl, j = round(sum(totpop),0), by = c("islID", "sex"), id = FALSE)

employment_subsistance_isl_cube <- employment_subsistance_isl_cube |>
  mutate(
    islID = ifelse(is.na(islID), "KI", islID),
    sex = ifelse(is.na(sex), "_T", sex),
    INDICATOR = "SUBEMPCNT") |>
  rename(GEO_PICT = islID, SEX = sex, OBS_VALUE = V1)

employment_subsistance_cube <- rbind(employment_subsistance_div_cube, employment_subsistance_isl_cube)

#unemployment by sex in divisions

unemployment_div <- PP_KIR20 |>
  filter(  lf1 == 'Looking for work') |>
  group_by(divID, sex) |>
  summarise(totpop = n())

unemployment_div <- as.data.table(unemployment_div)

unemployment_div_cube <- cube(unemployment_div, j = round(sum(totpop),0), by = c("divID", "sex"), id = FALSE)
unemployment_div_cube <- unemployment_div_cube[!is.na(unemployment_div_cube$divID)]

unemployment_div_cube <- unemployment_div_cube |>
  mutate(
    sex = ifelse(is.na(sex), "_T", sex),
    INDICATOR = "UNEMPCNT") |>
  rename(GEO_PICT = divID, SEX = sex, OBS_VALUE = V1)

#unemployment by sex in islands

unemployment_isl <- PP_KIR20 |>
  filter(  lf1 == 'Looking for work') |>
  group_by(islID, sex) |>
  summarise(totpop = n())

unemployment_isl <- as.data.table(unemployment_isl)

unemployment_isl_cube <- cube(unemployment_isl, j = round(sum(totpop),0), by = c("islID", "sex"), id = FALSE)

unemployment_isl_cube <- unemployment_isl_cube |>
  mutate(
    islID = ifelse(is.na(islID), "KI", islID),
    sex = ifelse(is.na(sex), "_T", sex),
    INDICATOR = "UNEMPCNT") |>
  rename(GEO_PICT = islID, SEX = sex, OBS_VALUE = V1)

unemployment_cube <- rbind(unemployment_div_cube, unemployment_isl_cube)


#Combine all the tables and finalise the table
combine_summary <- rbind(persons_cube, 
                         persons_ph_cube, 
                         persons_15plus_cube,
                         persons_less15_cube,
                         persons_15TO24_cube,
                         persons_25TO59_cube,
                         persons_60pluss_cube,
                         household_cube,
                         employment_cube,
                         employment_subsistance_cube,
                         unemployment_cube
)

combine_summary <- combine_summary |>
  mutate(
    FREQ = "A", TIME_PERIOD = 2020, UNIT_MEASUREE = "N", UNIT_MULT = "",
    DATA_SOURCE = "Population and Housing Census", OBS_STATUS = "", OBS_COMMENT = "", CONF_STATUS ="")

combine_summary <- combine_summary |>
  select(FREQ, TIME_PERIOD, GEO_PICT, INDICATOR, SEX, OBS_VALUE, UNIT_MEASUREE, UNIT_MULT, DATA_SOURCE, OBS_STATUS, OBS_COMMENT, CONF_STATUS)

#Write final data to output folder

write.csv(combine_summary, "../../output/population_by_sex.csv", row.names = FALSE)


