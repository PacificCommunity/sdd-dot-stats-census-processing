#Directory path
repository <- file.path(dirname(rstudioapi::getSourceEditorContext()$path))
setwd(repository)

source("setup.R")

#### ************************** Household tables *********************************************** ####

#households
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
    INDICATOR = "HHCNT")


#### ************************* Population tables ************************************ ####

#Overall population
persons <- PP_KIR20 |> 
  group_by(islID, sex) |> 
  summarise(totpop = n())

persons <- as.data.table(persons)

persons_cube <- cube(persons, j = round(sum(totpop),0), by = c("islID", "sex"), id = FALSE)

persons_cube <- persons_cube |>
  mutate(
    islID = ifelse(is.na(islID), "KI", islID),
    sex = ifelse(is.na(sex), "_T", sex),
    INDICATOR = "PPCNT")

#Population living in Private households
persons_ph <- PP_KIR20 |> 
  filter(housing_type == 'Private household') |>
  group_by(islID, sex) |> 
  summarise(totpop = n())

persons_ph <- as.data.table(persons_ph)

persons_ph_cube <- cube(persons_ph, j = round(sum(totpop),0), by = c("islID", "sex"), id = FALSE)

persons_ph_cube <- persons_ph_cube |>
  mutate(
    islID = ifelse(is.na(islID), "KI", islID),
    sex = ifelse(is.na(sex), "_T", sex),
    INDICATOR = "PPRVCNT")

#Population 15+ years living in private HHS

persons_15plus <- PP_KIR20 |> 
  filter(housing_type == 'Private household' & age >= 15) |>
  group_by(islID, sex) |> 
  summarise(totpop = n())

persons_15plus <- as.data.table(persons_15plus)

persons_15plus_cube <- cube(persons_15plus, j = round(sum(totpop),0), by = c("islID", "sex"), id = FALSE)

persons_15plus_cube <- persons_15plus_cube |>
  mutate(
    islID = ifelse(is.na(islID), "KI", islID),
    sex = ifelse(is.na(sex), "_T", sex),
    INDICATOR = "15PLSCNT")

#Population of children less than 15 years

persons_less15 <- PP_KIR20 |> 
  filter(age < 15) |>
  group_by(islID, sex) |> 
  summarise(totpop = n())

persons_less15 <- as.data.table(persons_less15)

persons_less15_cube <- cube(persons_less15, j = round(sum(totpop),0), by = c("islID", "sex"), id = FALSE)

persons_less15_cube <- persons_less15_cube |>
  mutate(
    islID = ifelse(is.na(islID), "KI", islID),
    sex = ifelse(is.na(sex), "_T", sex),
    INDICATOR = "LSS15CNT")


#Population of children 15-24 years

persons_15TO24 <- PP_KIR20 |> 
  filter(age >= 15 & age <= 24) |>
  group_by(islID, sex) |> 
  summarise(totpop = n())

persons_15TO24 <- as.data.table(persons_15TO24)

persons_15TO24_cube <- cube(persons_15TO24, j = round(sum(totpop),0), by = c("islID", "sex"), id = FALSE)

persons_15TO24_cube <- persons_15TO24_cube |>
  mutate(
    islID = ifelse(is.na(islID), "KI", islID),
    sex = ifelse(is.na(sex), "_T", sex),
    INDICATOR = "15TO24CNT")

#Population of children 25-59 years

persons_25TO59 <- PP_KIR20 |> 
  filter(age >= 25 & age <= 59) |>
  group_by(islID, sex) |> 
  summarise(totpop = n())

persons_25TO59 <- as.data.table(persons_25TO59)

persons_25TO59_cube <- cube(persons_25TO59, j = round(sum(totpop),0), by = c("islID", "sex"), id = FALSE)

persons_25TO59_cube <- persons_25TO59_cube |>
  mutate(
    islID = ifelse(is.na(islID), "KI", islID),
    sex = ifelse(is.na(sex), "_T", sex),
    INDICATOR = "25TO59CNT")

#Population of children 25-59 years

persons_60pluss <- PP_KIR20 |> 
  filter(age >= 60) |>
  group_by(islID, sex) |> 
  summarise(totpop = n())

persons_60pluss <- as.data.table(persons_60pluss)

persons_60pluss_cube <- cube(persons_60pluss, j = round(sum(totpop),0), by = c("islID", "sex"), id = FALSE)

persons_60pluss_cube <- persons_60pluss_cube |>
  mutate(
    islID = ifelse(is.na(islID), "KI", islID),
    sex = ifelse(is.na(sex), "_T", sex),
    INDICATOR = "60PLSCNT")

#Employment population by sex

employment <- PP_KIR20 |>
  filter(lf1 == 'Working for someone else for pay' | 
         lf1 == 'Working in own farming, raising animals or fishing' |
         lf1 == 'Working in any other kind of business activity' |
         lf1 == 'Doing an unpaid apprenticeship, internship' |
         lf1 == 'Doing unpaid voluntary, community, charity work'
         ) |>
  group_by(islID, sex) |>
  summarise(totpop = n())

employment <- as.data.table(employment)

employment_cube <- cube(employment, j = round(sum(totpop),0), by = c("islID", "sex"), id = FALSE)

employment_cube <- employment_cube |>
  mutate(
    islID = ifelse(is.na(islID), "KI", islID),
    sex = ifelse(is.na(sex), "_T", sex),
    INDICATOR = "EMPCNT")


#Subsistence employment by sex

employment_subsistance <- PP_KIR20 |>
  filter(  lf1 == 'Working in own farming, raising animals or fishing') |>
  group_by(islID, sex) |>
  summarise(totpop = n())

employment_subsistance <- as.data.table(employment_subsistance)

employment_subsistance_cube <- cube(employment_subsistance, j = round(sum(totpop),0), by = c("islID", "sex"), id = FALSE)

employment_subsistance_cube <- employment_subsistance_cube |>
  mutate(
    islID = ifelse(is.na(islID), "KI", islID),
    sex = ifelse(is.na(sex), "_T", sex),
    INDICATOR = "SUBEMPCNT")


#unemployment by sex

unemployment <- PP_KIR20 |>
  filter(  lf1 == 'Looking for work') |>
  group_by(islID, sex) |>
  summarise(totpop = n())

unemployment <- as.data.table(unemployment)

unemployment_cube <- cube(unemployment, j = round(sum(totpop),0), by = c("islID", "sex"), id = FALSE)

unemployment_cube <- unemployment_cube |>
  mutate(
    islID = ifelse(is.na(islID), "KI", islID),
    sex = ifelse(is.na(sex), "_T", sex),
    INDICATOR = "UNEMPCNT")



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
    DATA_SOURCE = "Population and Housing Census", OBS_STATUS = "", OBS_COMMENT = "", CONF_STATUS =""
    
  )|>
  rename(GEO_PICT = islID, SEX = sex, OBS_VALUE = V1)

combine_summary <- combine_summary |>
  select(FREQ, TIME_PERIOD, GEO_PICT, INDICATOR, SEX, OBS_VALUE, UNIT_MEASUREE, UNIT_MULT, DATA_SOURCE, OBS_STATUS, OBS_COMMENT, CONF_STATUS)
