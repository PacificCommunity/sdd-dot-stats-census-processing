#Directory path
repository <- file.path(dirname(rstudioapi::getSourceEditorContext()$path))
setwd(repository)

# Load the general R functions
source("../functions/setup.R")
source("kiribati_census2020.R")

pp_households <- PP_KIR20 |>
  group_by(interview__key, divID, division, urbrur, islID, island, village, ea_number) |>
  summarise(totpop = n())

#Add GEOIDs to the household table
HH_KIR20_GEOID <- merge(HH_KIR20, pp_households)

#Households by divisions
hholds <- HH_KIR20_GEOID |>
  filter(!is.na(wealth_index)) |>
  group_by(divID, urbrur, wealth_index) |>
  summarise(tot = n())

hholds <- as.data.table(hholds)
hholds_div_cube <- cube(hholds, j = sum(tot), by = c("divID", "urbrur", "wealth_index"), id = FALSE)

hholds_div_cube <- hholds_div_cube |>
  mutate(
    divID = ifelse(is.na(divID), "KI", divID),
    urbrur = ifelse(is.na(urbrur), "_T", urbrur),
    wealth_index = ifelse(is.na(wealth_index), "_T", wealth_index),
    INDICATOR = "HHCNT") |>
  rename(GEO_PICT = divID, URBANIZATION = urbrur, WEALTH_INDEX = wealth_index, OBS_VALUE = V1)

#Households by islands
hholds_isl <- HH_KIR20_GEOID |>
  filter(!is.na(wealth_index)) |>
  group_by(islID, urbrur, wealth_index) |>
  summarise(tot = n())

hholds_isl <- as.data.table(hholds_isl)
hholds_isl_cube <- cube(hholds_isl, j = sum(tot), by = c("islID", "urbrur", "wealth_index"), id = FALSE)
hholds_isl_cube <- hholds_isl_cube[!is.na(hholds_isl_cube$islID)]

hholds_isl_cube <- hholds_isl_cube |>
  mutate(
    urbrur = ifelse(is.na(urbrur), "_T", urbrur),
    wealth_index = ifelse(is.na(wealth_index), "_T", wealth_index),
    INDICATOR = "HHCNT") |>
  rename(GEO_PICT = islID, URBANIZATION = urbrur, WEALTH_INDEX = wealth_index, OBS_VALUE = V1)

hholds_final <- rbind(hholds_div_cube, hholds_isl_cube)

#Adding the rest of the fields to the dataframe
hholds_final <- hholds_final |>
  mutate(
    FREQ = "A", TIME_PERIOD = 2020, UNIT_MEASUREE = "N", UNIT_MULT = "",
    DATA_SOURCE = "Population and Housing Census", OBS_STATUS = "", OBS_COMMENT = "", CONF_STATUS ="")

hholds_final <- hholds_final |>
  select(FREQ, TIME_PERIOD, GEO_PICT, INDICATOR, WEALTH_INDEX, OBS_VALUE, UNIT_MEASUREE, UNIT_MULT, DATA_SOURCE, OBS_STATUS, OBS_COMMENT, CONF_STATUS)


#Write final household table to csv output folder
write.csv(hholds_final, "../../output/households.csv", row.names = FALSE)
