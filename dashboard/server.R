#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

az_counties <- map_data("county", region = "arizona")|>
  mutate(COUNTY = str_to_title(subregion))


# Incorporate the code from the app.R script
# Add in the counties
# Create a vector for the counties and an "All" counties
# Design a filter/dropdown

counties <- c("All", unique(data$COUNTY))
topics <- unique(labels$Topic)



# data %>% count(AGE)
# Ask RG if this works for her and if these numbers look like what she found


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
    vars = c("COUNTY", "LIVE_V3", "UserLanguage", "GENDER", 
             "AGE", "DEM_11", "Low_Income_FPL_100", "CE_EXPOSED", "CE_USER"), #add new filters here by adding column name in quotes
    inline = FALSE
  )
  
  refine_top_20 <- reactive({
    top_20_filtered() %>% 
      filter(if_any(all_of(input$race_ethnicity), function(x) {x == 1})) %>% # anonymous functions
      filter(if_any(all_of(input$topical_expert), function(x) {x == 1})) 
  })
  
  output$table <- DT::renderDataTable(refine_top_20())
  output$text <- renderText({dim(refine_top_20())})
  
  output$top20bar <- renderPlot({
    
   
    
  })
  


}

