library(shiny)
library(mapproj)
library(maps)
library(tidyverse)
library(pins)
library(shinyWidgets)



board <- board_connect()
data <- pin_read(board, "terrace/uace-na")

#read in data
# labels <- read.csv("data/labels.csv")
# labels <- read.csv(file.path("..", "data", "labels.csv"))
labels <- read.csv(here::here("data", "labels.csv"))


# This is taking the following columns and recoding them as "Yes" and No" rather than 1, 0
# Also recodes "EN" and "ES" as "English" and "Spanish"
data <- data %>% mutate(across(
  c(CE_USER, CE_EXPOSED, Low_Income_FPL_100, Low_Income_FPL_185),
  function(x) ifelse(x == 1, "Yes", "No")),
  UserLanguage = case_when(UserLanguage == "EN" ~ "English", 
                           UserLanguage == "ES" ~ "Spanish"))
