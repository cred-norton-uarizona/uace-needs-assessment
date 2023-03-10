#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


library(shiny)
library(tidyverse)
library(pins)
library(shinyWidgets)

#read in data
# labels <- read.csv("data/labels.csv")
# labels <- read.csv(file.path("..", "data", "labels.csv"))
labels <- read.csv(here::here("data", "labels.csv"))

board <- board_connect()
data <- pin_read(board, "terrace/raw_data")

# Create the age brackets
data <- data %>% mutate(AGE = case_when(
  DEM_9 >= 14 & DEM_9 <= 24 ~ "14-24 years old",
  DEM_9 >= 25 & DEM_9 <= 39 ~ "25-39 years old",
  DEM_9 >= 40 & DEM_9 <= 54 ~ "40-54 years old",
  DEM_9 >= 55 & DEM_9 <= 64 ~ "55-64 years old",
  DEM_9 >= 65 ~ "65 years and older"
))

# Calculate 185% FPL using the variable DEM_13 (Household income) and DEM_5 (number of people living in household)
data <- data %>% 
  mutate(Low_Income_FPL = case_when(
    DEM_5 == 1 & DEM_13 <= 26973 ~ 1,
    DEM_5 == 2 & DEM_13 <= 36482 ~ 1,
    DEM_5 == 3 & DEM_13 <= 45991 ~ 1,
    DEM_5 == 4 & DEM_13 <= 55500 ~ 1,
    DEM_5 == 5 & DEM_13 <= 65009 ~ 1,
    DEM_5 == 6 & DEM_13 <= 74518 ~ 1,
    DEM_5 == 7 & DEM_13 <= 84027 ~ 1,
    DEM_5 == 8 & DEM_13 <= 93536 ~ 1,
    DEM_5 == 9 & DEM_13 <= 103045 ~ 1,
    DEM_5 == 10 & DEM_13 <= 112554 ~ 1,
    DEM_5 == 11 & DEM_13 <= 122063 ~ 1,
    DEM_5 == 12 & DEM_13 <= 131572 ~ 1,
    DEM_5 == 13 & DEM_13 <= 141081 ~ 1,
    DEM_5 == 14 & DEM_13 <= 150590 ~ 1,
    TRUE ~ 0 
  ))



race_vec <- c("American Indian or Alaska Native" = "AIAN",
              "Asian" = "AS",
              "Black or African American" = "BL", 
              "Hispanic or Latinx" =  "HL",
              "Multiracial" = "MR", 
              "Native Hawaiian or Pacific Islander" = "NHPI", 
              "White" = "WH" , 
              "Prefer not to answer" = "NR") 
    
expert_vec <- c("Health & Well-Being" = "HLTH_EXPERT",
              "Education & Youth Development" = "ED_EXPERT", 
              "Agriculture" = "AG_EXPERT", 
              "Natural Resources" =  "NR_EXPERT",
              "Community & Economic Development" = "CD_EXPERT") 

# az_counties <- map_data("county", region = "arizona")|>
#   mutate(COUNTY = str_to_title(subregion))


# Incorporate the code from the app.R script

# data %>% count(AGE)
# Ask RG if this works for her and if these numbers look like what she found


# data %>% count(Low_Income_FPL)


# Application title
navbarPage(
  "University of Arizona Cooperative Extension Needs Assessment",
        p("Arizona Cooperative Extension conducted a statewide needs assessment survey in Fall
  2022 to better understand community needs and priorities. Cooperative Extension users,
  topical experts, and members of the general public from each county were invited to
  participate via online or paper survey. Participants were shown 99 items across topics
  relevant to Cooperative Extension and asked to rank how important it is to prioritize each
  item in their community on a 5-point scale."),
        h6("prepared by the Community Research, Evaluation and Development (CRED) team and the Communication and Cyber Technologies Data Science Team, University of Arizona"),

  # Tab panel 1 - Top 20 View
  tabPanel(
    "Top Priorities",
    sidebarLayout(
      sidebarPanel(
        selectizeGroupUI(
          id = "my-filters",
          params = list(
            COUNTY = list(inputId = "COUNTY", title = "County"),
            
            LIVE_V3 = list(inputId = "LIVE_V3", title = "Urban or Rural"),
            
            # Eng_Span = list(inputId = "NEED TO FIND", title = "Survey Language"),
            
            # Race_Ethnicity = list(inputId = "Race_Ethnicity", title = "Race/Ethnicity"),
            
            GENDER = list(inputId = "GENDER", title = "Gender"),
            
            AGE = list(inputId = "AGE", title = "Age"),
            
            DEM_11 = list(inputId = "DEM_11", title = "Educational Attainment"),
            
            Low_Income_FPL = list(inputId = "Low_Income_FPL", title = "Low-income Status (185% Federal Poverty Level or lower)"),
            
            CE_EXPOSED = list(inputId = "CE_EXPOSED", title = "Familiar with Extension"),
            
            CE_USER = list(inputId = "CE_USER", title = "Extension User")
            
          ),
          inline = FALSE
        ), status = "primary",
        selectInput(inputId = "race_ethnicity",
                    label = "Race/Ethnicity",
                    choices = race_vec,
                    multiple = TRUE),
        
        selectInput(inputId = "topical_expert",
                    label = "Topical Expert",
                    choices = expert_vec,
                    multiple = TRUE)
      ),
      
      
      # Show a plot of the generated distribution
      mainPanel(
        # This is where you show the output (data, chart, leaflet map, etc.) with commas
        # Where do we put this code for the Top Priorities and how do we specific grouping by the slicers/filters selected dynamically?
        DT::dataTableOutput(outputId = "table"),
        textOutput(outputId = "text")
        # p("hi, this is the main panel"),
        # shiny::verbatimTextOutput("table")
        
      )
    )
  )
  
  
  
)
