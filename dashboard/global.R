library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(shinycssloaders) #for loading indicator
library(plotly)
library(tidyverse)
library(stringr)
library(Cairo) # for graphics
library(pins)
library(arrow)
library(ggforce)
options(shiny.usecairo = TRUE)

# read in pinned data
board <- board_connect()
data <- pin_read(board, "terrace/uace-na")

# read in data needed for both ui and server
labels <- read.csv("labels.csv")

race_vec <- c("American Indian or Alaska Native" = "AIAN",
              "Asian" = "AS",
              "Black or African American" = "BL", 
              "Hispanic or Latino" =  "HL",
              "Multiracial" = "MR", 
              "Native Hawaiian or Other Pacific Islander" = "NHPI", 
              "White" = "WH" , 
              "Prefer not to answer" = "NR") 

topical_exp_vec <- c("Agriculture" = "AG_EXPERIENCE", 
                     "Education & Youth Development" = "ED_EXPERIENCE", 
                     "Health & Well-Being" = "HLTH_EXPERIENCE", 
                     "Natural Resources" =  "NR_EXPERIENCE", 
                     "Community & Economic Development" = "CD_EXPERIENCE")

topical_knw_vec <- c("Agriculture" = "AG_KNOWLEDGE",
                     "Education & Youth Development" = "ED_KNOWLEDGE",
                     "Health & Well-Being" = "HLTH_KNOWLEDGE",
                     "Natural Resources" = "NR_KNOWLEDGE",
                     "Community & Economic Development" = "CD_KNOWLEDGE")

county_vec <- data %>%
  select(COUNTY) %>%
  unique() %>%
  drop_na %>%
  pull() %>%
  str_sort() 

# Named vector for items 
item_vec <- labels$Metric
names(item_vec) <- labels$Description

# Named vector for importance
importance_vec <- c("Extremely" = 4,
                    "Very" = 3,
                    "Somewhat" = 2,
                    "A little" = 1,
                    "Not at all" = 0)
