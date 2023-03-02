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


# Incorporate the code from the app.R script
# Add in the counties
# Create a vector for the counties and an "All" counties
# Design a filter/dropdown

counties <- c("All", unique(data$COUNTY))
topics <- Unique(data$Metric)

# Define UI for application that draws a histogram
fluidPage(

    # Application title
    navbarPage("University of Arizona Cooperative Extension Needs Assessment",

    # Tab panel 1 - Top 20 View
    tabPanel(
      titlePanel("Top Priorities"),
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
    ),
    
    
    # Tab panel 2 - Topical Issues
    tabPanel(
      titlePanel("Issues by Topic"),
      sidebarLayout(
        sidebarPanel(
          # Here is where you add the widgets for the side panel with commas
          sliderInput("bins",
                      "Number of bins:",
                      min = 1,
                      max = 50,
                      value = 30)
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
          # This is where you show the output (data, chart, leaflet map, etc.) with commas
          plotOutput("distPlot")
        )
      )
    ),
    
    # Tab panel 3 - Map of Issues by County
    tabPanel(
      titlePanel("Issues by County"),
      sidebarLayout(
        sidebarPanel(
          # Here is where you add the widgets for the side panel with commas
          sliderInput("bins",
                      "Number of bins:",
                      min = 1,
                      max = 50,
                      value = 30)
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
          # This is where you show the output (data, chart, leaflet map, etc.) with commas
          plotOutput("distPlot")
        )
      )
    )
    )
)
