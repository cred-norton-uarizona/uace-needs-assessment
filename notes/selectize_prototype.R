library(shiny)
library(shinyWidgets)
library(shiny)
library(tidyverse)
library(pins)

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
  
  
data <- head(data, 20)
    
# data("mpg", package = "ggplot2")
  
ui <- navbarPage("app title",
  tabPanel("tab1",
    sidebarLayout(
      sidebarPanel(
        selectizeGroupUI(
          id = "my-filters",
          params = list(
            LIVE_V3 = list(inputId = "LIVE_V3", title = "Urban or Rural:"),
            COUNTY = list(inputId = "COUNTY", title = "County:")
          ),
          inline = FALSE
        ), status = "primary"
      ),
      mainPanel(DT::dataTableOutput(outputId = "table"))
    )
  )
)

  
  server <- function(input, output, session) {
    res_mod <- callModule(
      module = selectizeGroupServer,
      id = "my-filters",
      data = data,
      vars = c("LIVE_V3", "COUNTY"),
      inline = FALSE
    )
    output$table <- DT::renderDataTable(res_mod())
  }
  
  shinyApp(ui, server)
  
