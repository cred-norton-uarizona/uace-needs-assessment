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


az_counties <- map_data("county", region = "arizona")|>
  mutate(COUNTY = str_to_title(subregion))


# Incorporate the code from the app.R script
# Add in the counties
# Create a vector for the counties and an "All" counties
# Design a filter/dropdown

counties <- c("All", unique(data$COUNTY))
topics <- unique(labels$Topic)

# Create the age brackets
data <- data %>% mutate(AGE = case_when(
  DEM_9 >= 14 & DEM_9 <= 24 ~ "14-24 years old",
  DEM_9 >= 25 & DEM_9 <= 39 ~ "25-39 years old",
  DEM_9 >= 40 & DEM_9 <= 54 ~ "40-54 years old",
  DEM_9 >= 55 & DEM_9 <= 64 ~ "55-64 years old",
  DEM_9 >= 65 ~ "65 years and older"
))


# data %>% count(AGE)
# Ask RG if this works for her and if these numbers look like what she found

# Calculate 185% FPL using the variabled DEM_13 (Household income) and DEM_5 (number of people living in household)
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

# data %>% count(Low_Income_FPL)



# Define UI for application that draws a histogram


# Application title
navbarPage(
  "University of Arizona Cooperative Extension Needs Assessment",
#       p("Arizona Cooperative Extension conducted a statewide needs assessment survey in Fall
# 2022 to better understand community needs and priorities. Cooperative Extension users,
# topical experts, and members of the general public from each county were invited to
# participate via online or paper survey. Participants were shown 99 items across topics
# relevant to Cooperative Extension and asked to rank how important it is to prioritize each
# item in their community on a 5-point scale."),
#       h6("prepared by the Community Research, Evaluation and Develpment (CRED) team and the Communication and Cyber Technologies Data Science Team, University of Arizona"),

    # Tab panel 1 - Top 20 View
    tabPanel(
      "Top Priorities",
      sidebarLayout(
        sidebarPanel(
          # Here is where you add the widgets for the side panel with commas
          # selectInput(
          #   "county_tab1",
          #   "Select County:",
          #   choices = counties, 
          #   selected = "All"
          # ),
          selectizeGroupUI(
            id = "my-filters",
            params = list(
              COUNTY = list(inputId = "COUNTY", title = "County"),
              
              LIVE_V3 = list(inputId = "LIVE_V3", title = "Urban or Rural"),
              
              Eng_Span = list(inputId = "NEED TO FIND", title = "Survey Language"),
              
              Race_Ethnicity = list(c(AIAN, AS, BL, HL, MR, NHPI, WH), title = "Race/Ethnicity"),
              
              Gender = list(inputId = "Gender", title = "Gender"),
              
              AGE = list(inputId = "AGE", title = "Age"),
              
              DEM_11 = list(inputId = "DEM_11", title = "Educational Attainment"),
              
              Low_Income_FPL = list(inputId = "Low_Income_FPL", title = "Low-income Status (185% Federal Poverty Level or lower)"),
              
              CE_EXPOSED = list(inputId = "CE_EXPOSED", title = "Familiar with Extension"),
              
              CE_USER = list(inputId = "CE_USER", title = "Extension User"),
              
              TOPICAL_EXPERT = list(inputId = c(HLTH_EXPERT, ED_EXPERT, AG_EXPERT, NR_EXPERT, CD_EXPERT), title = "Topical Expert")
            ),
            inline = FALSE
          ), status = "primary"
        ),
        

        # Show a plot of the generated distribution
        mainPanel(
          # This is where you show the output (data, chart, leaflet map, etc.) with commas
          # Where do we put this code for the Top Priorities and how do we specific grouping by the slicers/filters selected dynamically?
          DT::dataTableOutput(outputId = "table")
          # p("hi, this is the main panel"),
          # shiny::verbatimTextOutput("table")

        )
      )
    ),
    
    
    # Tab panel 2 - Topical Issues
    tabPanel(
      "Issues by Topic",
      sidebarLayout(
        sidebarPanel(
          # Here is where you add the widgets for the side panel with commas
          selectInput(
            "county_tab1",
            "County",
            choices = counties,
            multiple = TRUE,
            selected = "All"
          ), # Variable(s) of interest: COUNTY
          selectInput(
            "rural_urban_tab1",
            "Rural versus Urban",
            choices = c("All", "Urban", "Rural"),
            selected = "All"
          ), # Variable(s) of interest: LIVE_V3
          # Terrace, figure out if this variable for survey language is in this raw data file.
          selectInput(
            "english_spanish_tab1",
            "Survey in English or Spanish",
            choices = c("English", "Spanish", "All"),
            selected = "All"
          ), # Variable(s) of interest: unsure- need to find
          selectInput(
            "race_eth_tab1",
            "Race/Ethnicity",
            choices = c(
              "American Indian or Alaska Native",
              "Asian",
              "Black or African American",
              "Hispanic or Latino",
              "Multiracial",
              "Native Hawaiian or Other Pacific Islander",
              "White",
              "All"
            ),
            selected = "All"
          ), # Variable(s) of interest: Binary recodes - AIAN, AS, BL, HL, MR, NHPI, WH  (note that NR means no response given; we aren't including this in the selection options)
          selectInput(
            "gender_tab1",
            "Gender",
            choices = c("Male", "Female", "Non-binary", "All"),
            selected = "All"
          ), # Variable(s) of interest: GENDER
          selectInput(
            "age_tab1",
            "Age",
            choices = c(
              "14-24 years old",
              "25-39 years old",
              "40-54 years old",
              "55-64 years old",
              "65 years and older",
              "All"
            ),
            selected = "All"
          ), # Variable(s) of interest: I think we need to add this one in (AGE recode) or make this.  Check with RG
          selectInput(
            "education_tab1",
            "Educational Attainment",
            choices = c("Bachelors degree or higher", "Less than a Bachelors degree", "All"),
            selected = "All"
          ), # Variable(s) of interest: DEM_11
          selectInput(
            "low_income_tab1",
            "Income Status",
            choices = c("Low-income status (185% FPL)", "Not Low-income status", "All"),
            selected = "All"
          ), # Variable(s) of interest:
          # FPL:
          #   We will need to calculate the 185% FPL:
          #   DEM_13 (Household income ranges)—I’m assuming that we will use the lower end of the range for this calculation
          # DEM_5 (number of people living in household)
          
          
          selectInput(
            "ce_familiar_tab1",
            "Familiar with Cooperative Extension",
            choices = c("Yes", "No", "All"),
            selected = "All"
          ), # Variable(s) of interest:
          
          # Binary variable created from: DEM_1: How much do you know about Cooperative Extension? Some or more (OR)
          # DEM_2: I have participated in a CE event- Yes (OR)
          # DEM_3: I am/have been a CE volunteer- Yes (OR)
          # DEM_4: I am/have been a CE employee- Yes (OR)
          
          
          selectInput(
            "ce_user_tab1",
            "Cooperative Extension User",
            choices = c("Yes", "No", "All"),
            selected = "All"
          ), # Variable(s) of interest:
          # Binary variable created from: DEM_2: I have participated in a CE event- Yes (OR)
          # DEM_3: I am/have been a CE volunteer- Yes (OR)
          # DEM_4: I am/have been a CE employee- Yes (OR
          
          
          selectInput(
            "topical_expert_tab1",
            "Topical Expert",
            choices = c(
              "Health and Well-Being",
              "Education and Youth Development",
              "Agriculture",
              "Natural Resources",
              "Community and Economic Development",
              "All respondents (Non-expert specific)"
            ),
            selected = "All respondents (Non-expert specific)"
          ),
          # Is it better to just have no selection pre-selected?
          # Variable(s) of interest:
          
          # Ag: ifelse(_______ | _______ ...
          #                   LIVE – Farm OR Ranch  OR
          #                   KNW_AG – 2 or more  OR
          #                   DEM_12_1 = Farming OR
          #                   DEM_12_2 = Ranching OR
          #                   Ed:
          #                     ED_KNOW (a lot or some) OR
          #                   DEM_12_4, DEM_12_5, DEM_12_6, DEM_12_7 (Early Ed OR K-12 OR Social or Community Services (Children and Youth))
          #                   Health
          #                   FCHS_KNOW(a lot or some) OR
          #                   DEM_12_6, DEM_12_7, DEM_12_9 (Health OR Social or Community Services (Adults) OR Social or Community Services (Children and Youth))
          #
          #                   Natural Resources
          #                   NR_KNOW(a lot or some) OR
          #                   DEM_12_3 (Forestry/Land or Resource Management)
          #                   Community & Econ Dev
          #                   CED_KNOW(a lot or some) OR
          #                   DEM_12_8, DEM_12_6, DEM_12_7 (Public sector (e.g., government) OR Community Services (Adults) OR Social or Community Services (Children and Youth))
          #
          
          selectInput(
            "children_under_18_household_tab1",
            "Respondents with children under 18 in the household",
            choices = c("Yes", "No", "Prefer not to answer", "All"),
            selected = "All"
          ) # Variable(s) of interest: DEM_6
        ), #end of sidebarPanel
  
        # Show a plot of the generated distribution
        mainPanel(
          # This is where you show the output (data, chart, leaflet map, etc.) with commas
          plotOutput("distPlot")
        )
      )
    ),
    
    # Tab panel 3 - Map of Issues by County
    tabPanel(
      "Issues by County",
      sidebarLayout(
        sidebarPanel(
          # Here is where you add the widgets for the side panel with commas
          selectInput("county_tab1",
                      "Select County:",
                      choices = counties, 
                      selected = "All")
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
          # This is where you show the output (data, chart, leaflet map, etc.) with commas
          plotOutput("distPlot")
        )
      )
    )
)

