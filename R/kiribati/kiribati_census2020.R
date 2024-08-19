#Directory path
repository <- file.path(dirname(rstudioapi::getSourceEditorContext()$path))
setwd(repository) # Required for file.choose() function

# Load the general R functions
source("../functions/setup.R")

#Import the data from the census

HH_KIR20 <- read_dta(glue("C:/Users/",user,"/OneDrive - SPC/NADA/Kiribati/SPC_KIR_2020_PHC_v01_M/Data/Distribute/Final/SPC_KIR_2020_PHC_Household_v01.dta")) %>%
  subset(select= -c(occupancy,division,island,village,ea_number,urbrur,dwelling_type,dwelling_other,housing_type,housing_other)) %>%
  mutate(across(where(labelled::is.labelled), haven::as_factor))

PP_KIR20 <- read_dta(glue("C:/Users/",user,"/OneDrive - SPC/NADA/Kiribati/SPC_KIR_2020_PHC_v01_M/Data/Distribute/Final/SPC_KIR_2020_PHC_Person_v01.dta")) %>%
  mutate(across(where(labelled::is.labelled), haven::as_factor))

PP_KIR20 <- PP_KIR20 %>%
  mutate(urbrur = recode(urbrur, "Urban" = "U", "Rural" = "R"),
         sex = recode(sex, "Male" = "M", "Female" = "F")
         )

PP_KIR20$sex <- as.character(PP_KIR20$sex)
PP_KIR20$urbrur <- as.character(PP_KIR20$urbrur)

KI_div <- data.frame(
  divID = c("KI-1", "KI-2", "KI-3", "KI-4", "KI-5"),
  division = c("Northern", "Sth. Tarawa", "Central", "Southern", "Line Is. & Phoenix")
) 

KI_island <- data.frame(
  islID = c("KI-101", "KI-102", "KI-103", "KI-104", "KI-105",
            "KI-201", "KI-202",
            "KI-301", "KI-302", "KI-303", "KI-304", "KI-305",
            "KI-401", "KI-402", "KI-403", "KI-404", "KI-405", "KI-406", "KI-407", "KI-408",
            "KI-501", "KI-502", "KI-503", "KI-504"
            ),
  island = c("Makin", "Butaritari", "Marakei", "Abaiang", "North Tarawa",
             "South Tarawa", "Betio",
             "Banaba", "Maiana", "Abemama", "Kuria", "Aranuka",
             "Nonouti", "North Tabiteuea", "South Tabiteuea", "Beru", "Nikunau", "Onotoa", "Tamana", "Arorae",
             "Teeraina", "Tabuaeran", "Kiritimati", "Kanton"
             )
)

PP_KIR20$country <- "KI"
PP_KIR20 <- merge(PP_KIR20, KI_div, by = "division")
PP_KIR20 <- merge(PP_KIR20, KI_island, by="island")

PP_KIR20_HH <- PP_KIR20 %>%
  group_by(interview__key, occupancy, country, division, island, village, ea_number, urbrur, divID, islID) %>%
  summarise(totpop = n())

PP_KIR20_HH <- PP_KIR20_HH %>%
  select(interview__key, occupancy, divID, islID, village, ea_number, urbrur)

HH_KIR20_New <- merge(HH_KIR20, PP_KIR20_HH)

#Add additional required columns to the dataframe 

HH_KIR20_New <- HH_KIR20_New |>
  mutate(FREQ = "A",
         TIME_PERIOD = 2020, 
         DATA_SOURCE = "POPULATION and HOUSING CENSUS"  
  )

#### ******************* Household tables **************************************** ####

households <- HH_KIR20_New |>
  filter(headeth !="Institution") |>
  group_by(FREQ, TIME_PERIOD, country, divID, islID, urbrur, headsex) |>
  rename(GEO_PICT = country, DIVISION = divID, ISLAND = islID, URBANISATION = urbrur, SEX = headsex) |>
  summarise(totHH = n())

households <- as.data.table(households)

households_cube <- cube(households, j = round(sum(totHH), 0), by = c("FREQ", "TIME_PERIOD", "GEO_PICT", "DIVISION", "ISLAND",  "URBANISATION", "SEX"), id = FALSE )

#### ****************** Population tables **************************************** ####

PP_KIR20 <- PP_KIR20 %>%
  mutate(age_grp5yr = case_when(
    age <= 4 ~ "Y00T04",
    age <= 9 ~ "Y05T09",
    age <= 14 ~ "Y10T14",
    age <= 19 ~ "Y15T19",
    age <= 24 ~ "Y20T24",
    age <= 29 ~ "Y25T29",
    age <= 34 ~ "Y30T34",
    age <= 39 ~ "Y35T39",
    age <= 44 ~ "Y40T44",
    age <= 49 ~ "Y45T49",
    age <= 54 ~ "Y50T54",
    age <= 59 ~ "Y55T59",
    age <= 64 ~ "Y60T64",
    TRUE ~ "Y65T999"
  ),
  age_grp10yr = case_when(
    age <= 9 ~ "Y00T09",
    age <= 19 ~ "Y10T19",
    age <= 29 ~ "Y20T29",
    age <= 39 ~ "Y30T39",
    age <= 49 ~ "Y40T49",
    age <= 59 ~ "Y50T59",
    age <= 69 ~ "Y60T69",
    TRUE ~ "Y70T999"
  ),
  
  household_type = case_when(
    dwelling_type == "Single housing unit (Private household)" ~ "PRVT",
    dwelling_type == "Other institutions (specify)" ~ "OTHR",
    TRUE ~ "INST"
  ),
  
  citizen_type = case_when(
    citizenship == 'Native-born citizen (I-Kiribati)' ~ "NATIVE",
    TRUE ~ "OTHER"
  )
  )