library(tidyverse)
library(readxl)
library(pins)

labels <- read_excel("C:/Users/Terrace Ewinghill/Box/Cooperative Extension Needs Assessment 2022/CRED - Incubator Collaboration/Labels for the content areas in survey.xlsx")

labels <- labels %>%
  mutate(Topic = case_when(
    str_detect(Metric, "FCHS_") ~ "Health and Well-Being",
    str_detect(Metric, "YD_") ~ "Education",
    str_detect(Metric, "AG_") ~ "Agriculture",
    str_detect(Metric, "NR_") ~ "Natural Resources",
    str_detect(Metric, "CED_") ~ "Community and Economic Development",
    TRUE ~ 'NA'
  ))

write_csv(labels, "data/labels.csv")
write_csv(labels, "dashboard/labels.csv")



data <- 
  read_csv("//corner2.sfcs.cals.arizona.edu/ERDU$/Shared/Projects/Cooperative Extension/Statewide Needs assessment 2022/Data/clean-data/scena_survey_wrangled_3.23.23 no zip.csv") %>%
  select(
    COUNTY,
    LIVE_V3,
    UserLanguage,
    DEM_11,
    Low_Income_FPL_185,
    Gender,
    AGE,
    ends_with("EVimp"),
    AIAN,
    AS,
    BL,
    HL,
    MR,
    NHPI,
    WH,
    NR,
    ends_with("EXPERIENCE"),
    ends_with("KNOWLEDGE"),
    Bach_or_higher,
    non_white,
    all_of(labels$Metric)
  )

board <- board_connect()
pin_write(board, data, "uace-na", type = "arrow")


