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

  
  # data_Evimp <- data %>% group_by(SELECTED INPUTS????) %>%
  #   summarize(across(
  #     ends_with("Evimp"),
  #     ~sum(.x == 1, na.rm = TRUE)/n())*100),
  # 
  # data_Evimp <- data_Evimp %>% pivot_longer(-COUNTY,
  #                                           names_to = "Metric",
  #                                           values_to = "Percentage"
  # ) %>%
  #   group_by(SELECTED INPUTS????) %>% 
  #   arrange(desc(Percentage)) %>% 
  #   mutate(Metric = gsub("_EVimp", "", Metric))%>%
  #   slice(1:30),
  # 
  # output$top_priorities <- renderPlot({
  #   priorities <- top_priorities  %>% 
  #     mutate(selected = ifelse(COUNTY == input$select_county, TRUE, FALSE))
  #   
  #   ggplot(data_Evimp, aes(x = Metric, y = Percentage)) +
  #     geom_col(fill = )
  #   
  #   
  # })
  top_20_filtered <- callModule(
    module = selectizeGroupServer,
    id = "my-filters",
    data = data,
    vars = c("LIVE_V3", "COUNTY"), #add new filters here by adding column name in quotes
    inline = FALSE
  )
  
  output$table <- DT::renderDataTable(top_20_filtered())
  


}

