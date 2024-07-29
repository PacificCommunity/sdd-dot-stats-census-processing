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
  mutate(INDICATOR = "PPCNT", UNIT_MEASURE = "N") |>
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
    INDICATOR = "PPCNT", UNIT_MEASURE = "N") |>
  rename(GEO_PICT = islID, OBS_VALUE = V1)

persons_ps_cube <- rbind(persons_ps_div_cube, persons_ps_isl_cube)


#Population less than 15 years old by division
persons_ps_15less_div <- PP_KIR20 |> 
  filter(age < 15) |>
  group_by(divID) |> 
  summarise(totpop = n())

persons_ps_15less_div <- as.data.table(persons_ps_15less_div)

persons_ps_15less_div_cube <- cube(persons_ps_15less_div, j = round(sum(totpop),0), by = c("divID"), id = FALSE)
persons_ps_15less_div_cube <- persons_ps_15less_div_cube[!is.na(persons_ps_15less_div_cube$divID)]

persons_ps_15less_div_cube <- persons_ps_15less_div_cube |>
  mutate(INDICATOR = "LSS15CNT", UNIT_MEASURE = "N") |>
  rename(GEO_PICT = divID, OBS_VALUE = V1)


#Population less than 15 years old by island
persons_ps_15less_isl <- PP_KIR20 |> 
  filter(age < 15) |>
  group_by(islID) |> 
  summarise(totpop = n())

persons_ps_15less_isl <- as.data.table(persons_ps_15less_isl)

persons_ps_15less_isl_cube <- cube(persons_ps_15less_isl, j = round(sum(totpop),0), by = c("islID"), id = FALSE)

persons_ps_15less_isl_cube <- persons_ps_15less_isl_cube |>
  mutate(
    islID = ifelse(is.na(islID), "KI", islID),
    INDICATOR = "LSS15CNT", UNIT_MEASURE = "N") |>
  rename(GEO_PICT = islID, OBS_VALUE = V1)

persons_ps_15less_cube <- rbind(persons_ps_15less_div_cube, persons_ps_15less_isl_cube)


#Population less than 15-24 years old in division
persons_ps_15TO24_div <- PP_KIR20 |> 
  filter(age >= 15 & age <= 24) |>
  group_by(divID) |> 
  summarise(totpop = n())

persons_ps_15TO24_div <- as.data.table(persons_ps_15TO24_div)

persons_ps_15TO24_div_cube <- cube(persons_ps_15TO24_div, j = round(sum(totpop),0), by = c("divID"), id = FALSE)
persons_ps_15TO24_div_cube <- persons_ps_15TO24_div_cube[!is.na(persons_ps_15TO24_div_cube$divID)]

persons_ps_15TO24_div_cube <- persons_ps_15TO24_div_cube |>
  mutate(INDICATOR = "15TO24CNT", UNIT_MEASURE = "N") |>
  rename(GEO_PICT = divID, OBS_VALUE = V1)

#Population less than 15-24 years old in islands
persons_ps_15TO24_isl <- PP_KIR20 |> 
  filter(age >= 15 & age <= 24) |>
  group_by(islID) |> 
  summarise(totpop = n())

persons_ps_15TO24_isl <- as.data.table(persons_ps_15TO24_isl)

persons_ps_15TO24_isl_cube <- cube(persons_ps_15TO24_isl, j = round(sum(totpop),0), by = c("islID"), id = FALSE)

persons_ps_15TO24_isl_cube <- persons_ps_15TO24_isl_cube |>
  mutate(
    islID = ifelse(is.na(islID), "KI", islID),
    INDICATOR = "15TO24CNT", UNIT_MEASURE = "N") |>
  rename(GEO_PICT = islID, OBS_VALUE = V1)

persons_ps_15TO24_cube <- rbind(persons_ps_15TO24_div_cube, persons_ps_15TO24_isl_cube)
  

#Population less than 55-59 years old in divisions
persons_ps_25TO59_div <- PP_KIR20 |> 
  filter(age >= 25 & age <= 59) |>
  group_by(divID) |> 
  summarise(totpop = n())

persons_ps_25TO59_div <- as.data.table(persons_ps_25TO59_div)

persons_ps_25TO59_div_cube <- cube(persons_ps_25TO59_div, j = round(sum(totpop),0), by = c("divID"), id = FALSE)
persons_ps_25TO59_div_cube <- persons_ps_25TO59_div_cube[!is.na(persons_ps_25TO59_div_cube$divID)]

persons_ps_25TO59_div_cube <- persons_ps_25TO59_div_cube |>
  mutate(INDICATOR = "25TO59CNT", UNIT_MEASURE = "N") |>
  rename(GEO_PICT = divID, OBS_VALUE = V1)

#Population less than 55-59 years old in islands
persons_ps_25TO59_isl <- PP_KIR20 |> 
  filter(age >= 25 & age <= 59) |>
  group_by(islID) |> 
  summarise(totpop = n())

persons_ps_25TO59_isl <- as.data.table(persons_ps_25TO59_isl)

persons_ps_25TO59_isl_cube <- cube(persons_ps_25TO59_isl, j = round(sum(totpop),0), by = c("islID"), id = FALSE)

persons_ps_25TO59_isl_cube <- persons_ps_25TO59_isl_cube |>
  mutate(
    islID = ifelse(is.na(islID), "KI", islID),
    INDICATOR = "25TO59CNT", UNIT_MEASURE = "N") |>
  rename(GEO_PICT = islID, OBS_VALUE = V1)

persons_ps_25TO59_cube <- rbind(persons_ps_25TO59_div_cube, persons_ps_25TO59_isl_cube)


#Population less than 60+ years old in divisions
persons_ps_60PLUSS_div <- PP_KIR20 |> 
  filter(age >= 60) |>
  group_by(divID) |> 
  summarise(totpop = n())

persons_ps_60PLUSS_div <- as.data.table(persons_ps_60PLUSS_div)

persons_ps_60PLUSS_div_cube <- cube(persons_ps_60PLUSS_div, j = round(sum(totpop),0), by = c("divID"), id = FALSE)
persons_ps_60PLUSS_div_cube <- persons_ps_60PLUSS_div_cube[!is.na(persons_ps_60PLUSS_div_cube$divID)]

persons_ps_60PLUSS_div_cube <- persons_ps_60PLUSS_div_cube |>
  mutate(INDICATOR = "60PLSCNT", UNIT_MEASURE = "N") |>
  rename(GEO_PICT = divID, OBS_VALUE = V1)

#Population less than 60+ years old in islands
persons_ps_60PLUSS_isl <- PP_KIR20 |> 
  filter(age >= 60) |>
  group_by(islID) |> 
  summarise(totpop = n())

persons_ps_60PLUSS_isl <- as.data.table(persons_ps_60PLUSS_isl)

persons_ps_60PLUSS_isl_cube <- cube(persons_ps_60PLUSS_isl, j = round(sum(totpop),0), by = c("islID"), id = FALSE)

persons_ps_60PLUSS_isl_cube <- persons_ps_60PLUSS_isl_cube |>
  mutate(
    islID = ifelse(is.na(islID), "KI", islID),
    INDICATOR = "60PLSCNT", UNIT_MEASURE = "N") |>
  rename(GEO_PICT = islID, OBS_VALUE = V1)

persons_ps_60PLUSS_cube <- rbind(persons_ps_60PLUSS_div_cube, persons_ps_60PLUSS_isl_cube)


#Population median age by divisions
persons_ps_medAge_div <- PP_KIR20 |> 
  group_by(divID) |> 
  summarise(medAge = median(age))

persons_ps_medAge_div <- as.data.table(persons_ps_medAge_div)

persons_ps_medAge_div_cube <- cube(persons_ps_medAge_div, j = round(median(medAge),0), by = c("divID"), id = FALSE)
persons_ps_medAge_div_cube <- persons_ps_medAge_div_cube[!is.na(persons_ps_medAge_div_cube$divID)]

persons_ps_medAge_div_cube <- persons_ps_medAge_div_cube |>
  mutate(INDICATOR = "MEDAGE", UNIT_MEASURE = "MEDIAN") |>
  rename(GEO_PICT = divID, OBS_VALUE = V1)

#Population median age by islands
persons_ps_medAge_isl <- PP_KIR20 |> 
  group_by(islID) |> 
  summarise(medAge = median(age))

persons_ps_medAge_isl <- as.data.table(persons_ps_medAge_isl)

persons_ps_medAge_isl_cube <- cube(persons_ps_medAge_isl, j = round(median(medAge),0), by = c("islID"), id = FALSE)

persons_ps_medAge_isl_cube <- persons_ps_medAge_isl_cube |>
  mutate(
    islID = ifelse(is.na(islID), "KI", islID),
    INDICATOR = "MEDAGE", UNIT_MEASURE = "MEdIAN") |>
  rename(GEO_PICT = islID, OBS_VALUE = V1)

persons_ps_medAge_cube <- rbind(persons_ps_medAge_div_cube, persons_ps_medAge_isl_cube)


#Dependency Ratio age(15-64) years by division
dependRatio_div <- PP_KIR20 |>
  group_by(divID) |>
  mutate(
    less15 = ifelse(age <15, 1, 0),
    pluss65 = ifelse(age >64, 1, 0),
    workAge = ifelse(age >=15 & age <=64, 1, 0)
  ) |>
  summarise(totless15 = sum(less15),
            totpluss65 = sum(pluss65),
            totworkage = sum(workAge)
  )

dependRatio_div$Dratio <- round((dependRatio_div$totless15 + dependRatio_div$totpluss65)/dependRatio_div$totworkage * 100, 0)
dependRatio_div <- dependRatio_div |>
  select(divID, Dratio)

dependRatio_div <- as.data.table(dependRatio_div)

dependRatio_div_cube <- cube(dependRatio_div, j = round(mean(Dratio),0), by = c("divID"), id = FALSE)
dependRatio_div_cube <- dependRatio_div_cube[!is.na(dependRatio_div_cube$divID)]

dependRatio_div_cube <- dependRatio_div_cube |>
  mutate(INDICATOR = "DRATIO", UNIT_MEASURE = "PERCENT") |>
  rename(GEO_PICT = divID, OBS_VALUE = V1)

#Dependency Ratio age(15-64) years by island
dependRatio_isl <- PP_KIR20 |>
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

dependRatio_isl$Dratio <- round((dependRatio_isl$totless15 + dependRatio_isl$totpluss65)/dependRatio_isl$totworkage * 100, 0)
dependRatio_isl <- dependRatio_isl |>
  select(islID, Dratio)

dependRatio_isl <- as.data.table(dependRatio_isl)

dependRatio_isl_cube <- cube(dependRatio_isl, j = round(mean(Dratio),0), by = c("islID"), id = FALSE)
dependRatio_isl_cube <- dependRatio_isl_cube |>
  mutate(
    islID = ifelse(is.na(islID), "KI", islID),
    INDICATOR = "DRATIO", UNIT_MEASURE = "PERCENT") |>
  rename(GEO_PICT = islID, OBS_VALUE = V1)

dependRatio_cube <- rbind(dependRatio_div_cube, dependRatio_isl_cube)


#Sex Ratio by division
sexRatio_div <- PP_KIR20 |>
  group_by(divID) |>
  mutate(
    female = ifelse(sex == 'F', 1, 0),
    male = ifelse(sex == 'M', 1, 0)
  ) |>
  summarise(totfemale = sum(female),
            totmale = sum(male)
  )

sexRatio_div$Sratio <- round(sexRatio_div$totfemale/sexRatio_div$totmale * 100, 0)

sexRatio_div <- sexRatio_div |>
  select(divID, Sratio)

sexRatio_div <- as.data.table(sexRatio_div)

sexRatio_div_cube <- cube(sexRatio_div, j = round(mean(Sratio),0), by = c("divID"), id = FALSE)
sexRatio_div_cube <- sexRatio_div_cube[!is.na(sexRatio_div_cube$divID)]

sexRatio_div_cube <- sexRatio_div_cube |>
  mutate(INDICATOR = "SRATIO", UNIT_MEASURE = "PERCENT") |>
  rename(GEO_PICT = divID, OBS_VALUE = V1)

#Sex Ratio by island
sexRatio_isl <- PP_KIR20 |>
  group_by(islID) |>
  mutate(
    female = ifelse(sex == 'F', 1, 0),
    male = ifelse(sex == 'M', 1, 0)
  ) |>
  summarise(totfemale = sum(female),
            totmale = sum(male)
  )

sexRatio_isl$Sratio <- round(sexRatio_isl$totfemale/sexRatio_isl$totmale * 100, 0)
sexRatio_isl <- sexRatio_isl |>
  select(islID, Sratio)

sexRatio_isl <- as.data.table(sexRatio_isl)

sexRatio_isl_cube <- cube(sexRatio_isl, j = round(mean(Sratio),0), by = c("islID"), id = FALSE)
sexRatio_isl_cube <- sexRatio_isl_cube |>
  mutate(
    islID = ifelse(is.na(islID), "KI", islID),
    INDICATOR = "SRATIO", UNIT_MEASURE = "PERCENT") |>
  rename(GEO_PICT = islID, OBS_VALUE = V1)

sexRatio_cube <- rbind(sexRatio_div_cube, sexRatio_isl_cube)


#### ***************************** Household structure ***************************************** ####

#Divisional households
households_div <- PP_KIR20 |>
  filter(relat == 'Head') |>
  group_by(divID) |>
  summarise(OBS_VALUE = n())

households_div_cube <- households_div |>
  mutate(INDICATOR = "HHCNT", UNIT_MEASURE = "N") |>
  rename(GEO_PICT = divID)
  

#island households
households_isl <- PP_KIR20 |>
  filter(relat == 'Head') |>
  group_by(islID) |>
  summarise(tothh = n())

households_isl <- as.data.table(households_isl)

household_isl_cube <- cube(household_isl, j = round(sum(tothh),0), by = c("islID"), id = FALSE)
household_isl_cube <- household_isl_cube |>
  mutate(
    islID = ifelse(is.na(islID), "KI", islID),
    INDICATOR = "HHCNT", UNIT_MEASURE = "N") |>
  rename(GEO_PICT = islID, OBS_VALUE = V1)

#Combine all divisional and island households

households_cube <- rbind(households_div_cube, household_isl_cube)

#Producing the Average Household size

households_div_hh <- households_div

households_div_pp <- PP_KIR20 |>
  group_by(divID) |>
  summarise(totpop = n())

household_size_div <- merge(households_div_hh, households_div_pp)
household_size_div$div_hhSize <- round(household_size_div$totpop/household_size_div$OBS_VALUE, 1)

household_size_div_cube <- household_size_div |>
  select(divID, div_hhSize) |>
  mutate(INDICATOR = "HHSIZE", UNIT_MEASURE = "N") |>
  rename(GEO_PICT = divID, OBS_VALUE = div_hhSize)


households_isl_hh <- households_isl

households_isl_pp <- PP_KIR20 |>
  group_by(islID) |>
  summarise(totpop = n())

household_size_isl <- merge(households_isl_hh, households_isl_pp)
household_size_isl$isl_hhSize <- round(household_size_isl$totpop/household_size_isl$tothh, 1)

households_size_isl <- cube(household_size_isl, j = round(mean(isl_hhSize),1), by = c("islID"), id = FALSE)

households_size_isl_cube <- households_size_isl |>
  mutate(
    islID = ifelse(is.na(islID), "KI", islID),
    INDICATOR = "HHSIZE", UNIT_MEASURE = "N") |>
  rename(GEO_PICT = islID, OBS_VALUE = V1)


household_size_cube <- rbind(household_size_div_cube, households_size_isl_cube)



#Combining the Population structure tables

popStructure <- rbind(persons_ps_cube,
                      persons_ps_15less_cube,
                      persons_ps_15TO24_cube,
                      persons_ps_25TO59_cube,
                      persons_ps_60PLUSS_cube,
                      persons_ps_medAge_cube,
                      dependRatio_cube,
                      sexRatio_cube,
                      households_cube,
                      household_size_cube
)

combine_popStructure <- popStructure |>
  mutate(
    FREQ = "A", TIME_PERIOD = 2020, UNIT_MULT = "",
    DATA_SOURCE = "Population and Housing Census", OBS_STATUS = "", OBS_COMMENT = "", CONF_STATUS =""
  )

combine_popStructure <- combine_popStructure |>
  select(FREQ, TIME_PERIOD, GEO_PICT, INDICATOR, OBS_VALUE, UNIT_MEASURE, UNIT_MULT, DATA_SOURCE, OBS_STATUS, OBS_COMMENT, CONF_STATUS)

#Write the final table to output folder
write.csv(combine_popStructure, "../../output/population_structure.csv", row.names = FALSE)

