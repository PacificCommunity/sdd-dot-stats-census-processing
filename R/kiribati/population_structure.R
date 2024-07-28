#Directory path
repository <- file.path(dirname(rstudioapi::getSourceEditorContext()$path))
setwd(repository)

source("setup.R")

#### ************************* Population Structure ************************************ ####

#Overall population by division
persons_ps_div <- PP_KIR20 |> 
  group_by(divID) |> 
  summarise(totpop = n())

persons_ps_div <- as.data.table(persons_ps_div)

persons_ps_div_cube <- cube(persons_ps_div, j = round(sum(totpop),0), by = c("divID"), id = FALSE)
persons_ps_div_cube <- persons_ps_div_cube[!is.na(persons_ps_div_cube$divID)]

persons_ps_div_cube <- persons_ps_div_cube |>
  mutate(INDICATOR = "PPCNT") |>
  rename(GEO_PICT = divID, OBS_VALUE = V1)

#Overall population by island
persons_ps_isl <- PP_KIR20 |> 
  group_by(islID) |> 
  summarise(totpop = n())

persons_ps_isl <- as.data.table(persons_ps_isl)

persons_ps_isl_cube <- cube(persons_ps_isl, j = round(sum(totpop),0), by = c("islID"), id = FALSE)

persons_ps_isl_cube <- persons_ps_isl_cube |>
  mutate(
    islID = ifelse(is.na(islID), "KI", islID),
    INDICATOR = "PPCNT") |>
  rename(GEO_PICT = islID, OBS_VALUE = V1)

persons_ps_cube <- rbind(persons_ps_div_cube, persons_ps_isl_cube)


#Population less than 15 years old
persons_ps_15less <- PP_KIR20 |> 
  filter(age < 15) |>
  group_by(islID) |> 
  summarise(totpop = n())

persons_ps_15less <- as.data.table(persons_ps_15less)

persons_ps_15less_cube <- cube(persons_ps_15less, j = round(sum(totpop),0), by = c("islID"), id = FALSE)

persons_ps_15less_cube <- persons_ps_15less_cube |>
  mutate(
    islID = ifelse(is.na(islID), "KI", islID),
    INDICATOR = "LSS15CNT")


#Population less than 15 years old
persons_ps_15less <- PP_KIR20 |> 
  filter(age < 15) |>
  group_by(islID) |> 
  summarise(totpop = n())

persons_ps_15less <- as.data.table(persons_ps_15less)

persons_ps_15less_cube <- cube(persons_ps_15less, j = round(sum(totpop),0), by = c("islID"), id = FALSE)

persons_ps_15less_cube <- persons_ps_15less_cube |>
  mutate(
    islID = ifelse(is.na(islID), "KI", islID),
    INDICATOR = "LSS15CNT")



#Population less than 15-24 years old
persons_ps_15TO24 <- PP_KIR20 |> 
  filter(age >= 15 & age <= 24) |>
  group_by(islID) |> 
  summarise(totpop = n())

persons_ps_15TO24 <- as.data.table(persons_ps_15TO24)

persons_ps_15TO24_cube <- cube(persons_ps_15TO24, j = round(sum(totpop),0), by = c("islID"), id = FALSE)

persons_ps_15TO24_cube <- persons_ps_15TO24_cube |>
  mutate(
    islID = ifelse(is.na(islID), "KI", islID),
    INDICATOR = "15TO24CNT")


#Population less than 55-59 years old
persons_ps_25TO59 <- PP_KIR20 |> 
  filter(age >= 25 & age <= 59) |>
  group_by(islID) |> 
  summarise(totpop = n())

persons_ps_25TO59 <- as.data.table(persons_ps_25TO59)

persons_ps_25TO59_cube <- cube(persons_ps_25TO59, j = round(sum(totpop),0), by = c("islID"), id = FALSE)

persons_ps_25TO59_cube <- persons_ps_25TO59_cube |>
  mutate(
    islID = ifelse(is.na(islID), "KI", islID),
    INDICATOR = "25TO59CNT")

#Population less than 60+ years old
persons_ps_60PLUSS <- PP_KIR20 |> 
  filter(age >= 60) |>
  group_by(islID) |> 
  summarise(totpop = n())

persons_ps_60PLUSS <- as.data.table(persons_ps_60PLUSS)

persons_ps_60PLUSS_cube <- cube(persons_ps_60PLUSS, j = round(sum(totpop),0), by = c("islID"), id = FALSE)

persons_ps_60PLUSS_cube <- persons_ps_60PLUSS_cube |>
  mutate(
    islID = ifelse(is.na(islID), "KI", islID),
    INDICATOR = "60PLSCNT")


#Population median age by islands
persons_ps_medAge <- PP_KIR20 |> 
  group_by(islID) |> 
  summarise(medAge = median(age))

persons_ps_medAge <- as.data.table(persons_ps_medAge)

persons_ps_medAge_cube <- cube(persons_ps_medAge, j = round(median(medAge),0), by = c("islID"), id = FALSE)

persons_ps_medAge_cube <- persons_ps_medAge_cube |>
  mutate(
    islID = ifelse(is.na(islID), "KI", islID),
    INDICATOR = "MEDAGE")

#Dependency Ratio age(15-64) years
dependRatio <- PP_KIR20 |>
  group_by(islID) |>
  mutate(
    less15 = ifelse(age <15, 1, 0),
    pluss65 = ifelse(age >64, 1, 0),
    workAge = ifelse(age >=15 & age <=64, 1, 0)
  ) |>
  summarise(totless15 = sum(less15),
            totpluss65 = sum(pluss65),
            totworkage = sum(workAge)
  )

dependRatio$Dratio <- round((dependRatio$totless15 + dependRatio$totpluss65)/dependRatio$totworkage * 100, 0)
dependRatio <- dependRatio |>
  select(islID, Dratio)

dependRatio <- as.data.table(dependRatio)
dependRatio_cube <- cube(dependRatio, j = round(mean(Dratio),0), by = c("islID"), id = FALSE)
dependRatio_cube <- dependRatio_cube |>
  mutate(
    islID = ifelse(is.na(islID), "KI", islID),
    INDICATOR = "DRATIO")

#Sex Ratio
sexRatio <- PP_KIR20 |>
  group_by(islID) |>
  mutate(
    female = ifelse(sex == 'F', 1, 0),
    male = ifelse(sex == 'M', 1, 0)
  ) |>
  summarise(totfemale = sum(female),
            totmale = sum(male)
  )

sexRatio$Sratio <- round(sexRatio$totfemale/sexRatio$totmale * 100, 0)
sexRatio <- sexRatio |>
  select(islID, Sratio)

sexRatio <- as.data.table(sexRatio)
sexRatio_cube <- cube(sexRatio, j = round(mean(Sratio),0), by = c("islID"), id = FALSE)
sexRatio_cube <- sexRatio_cube |>
  mutate(
    islID = ifelse(is.na(islID), "KI", islID),
    INDICATOR = "SRATIO")

#Combining the Population structure tables

popStructure <- rbind(persons_ps_cube,
                      persons_ps_15less_cube,
                      persons_ps_15TO24_cube,
                      persons_ps_25TO59_cube,
                      persons_ps_60PLUSS_cube,
                      persons_ps_medAge_cube,
                      dependRatio_cube,
                      sexRatio_cube
)

combine_popStructure <- popStructure |>
  mutate(
    FREQ = "A", TIME_PERIOD = 2020, UNIT_MEASUREE = "N", UNIT_MULT = "",
    DATA_SOURCE = "Population and Housing Census", OBS_STATUS = "", OBS_COMMENT = "", CONF_STATUS =""
    
  )|>
  rename(GEO_PICT = islID, OBS_VALUE = V1)

combine_popStructure <- combine_popStructure |>
  select(FREQ, TIME_PERIOD, GEO_PICT, INDICATOR, OBS_VALUE, UNIT_MEASUREE, UNIT_MULT, DATA_SOURCE, OBS_STATUS, OBS_COMMENT, CONF_STATUS)
