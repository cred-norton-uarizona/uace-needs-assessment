#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(mapproj)
library(maps)
library(tidyverse)
library(pins)

# Define server logic required to draw a histogram
function(input, output, session) {

    selected_data <- if(input$county_tab1 == "All") {
      data
    }
    else{
      data %>% filter(COUNTY == input$county_tab1)
    }

    })

}
