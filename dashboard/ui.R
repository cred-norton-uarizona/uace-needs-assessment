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

#read in data
# labels <- read.csv("data/labels.csv")
labels <- read.csv(file.path("data", "labels.csv"))

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
          # Where do we put this code for the Top Priorities and how do we specific grouping by the slicers/filters selected dynamically?
          data_Evimp <- data %>% group_by(SELECTED INPUTS????) %>%
            summarize(across(
              ends_with("Evimp"),
              ~sum(.x == 1, na.rm = TRUE)/n())*100)
          
          data_Evimp <- data_Evimp %>% pivot_longer(-COUNTY,
                                                    names_to = "Metric",
                                                    values_to = "Percentage"
          ) %>%
            group_by(SELECTED INPUTS????) %>% 
            arrange(desc(Percentage)) %>% 
            mutate(Metric = gsub("_EVimp", "", Metric))%>%
            slice(1:30),
          
          output$top_priorities <- renderPlot({
              priorities <- top_priorities  %>% 
                mutate(selected = ifelse(COUNTY == input$select_county, TRUE, FALSE))
              
              ggplot(data_Evimp, aes(x = Metric, y = Percentage)) +
                geom_col(fill = )
                     
              
            })
        )
      )
    ),
    
    
    # Tab panel 2 - Topical Issues
    tabPanel(
      titlePanel("Issues by Topic"),
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
    
    # Tab panel 3 - Map of Issues by County
    tabPanel(
      titlePanel("Issues by County"),
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
)
