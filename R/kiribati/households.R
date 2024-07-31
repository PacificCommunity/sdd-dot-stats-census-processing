#Directory path
repository <- file.path(dirname(rstudioapi::getSourceEditorContext()$path))
setwd(repository)

# Load the general R functions
source("../functions/setup.R")
source("kiribati_census2020.R")

pp_households <- PP_KIR20 |>
  group_by(interview__key, divID, division, islID, island, village, ea_number) |>
  summarise(totpop = n())

hh_households <- HH_KIR20 |>
  filter(!is.na(hhsize)) |>
  select(interview__key, hhsize, totmale, totfemale)


HH_KIR20_GEOID <- merge(HH_KIR20, pp_households)


  