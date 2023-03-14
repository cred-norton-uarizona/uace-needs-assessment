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
  
  # Make a function for refine_top_20 by adding ()
  data_Evimp <- reactive({
    refine_top_20() %>% group_by(COUNTY) %>%
      summarize(across(
        ends_with("Evimp"),
        ~sum(.x == 1, na.rm = TRUE)/n())*100) %>% 
      
      pivot_longer(-COUNTY,
                   names_to = "Metric",
                   values_to = "Percentage"
      ) %>%
      group_by(COUNTY) %>% 
      arrange(desc(Percentage)) %>% 
      mutate(Metric = gsub("_EVimp", "", Metric))%>%
      slice(1:30) %>% 
      left_join(labels) %>% 
        select(COUNTY, Topic, Metric, Description, Percentage) %>% 
    
      arrange(desc(Percentage)) %>% 
      ungroup() %>% 
      mutate(Percentage = round(Percentage)/100) %>% 
      filter(Percentage >= nth(Percentage, 20)) %>% 
      mutate(row = 1:n())
  })
  
  output$table <- DT::renderDataTable(data_Evimp())

  output$top20bar <- renderPlot({
    colors <- c("Health and Well-Being" = "#604878", "Natural Resources" = "#1B587C", "Agriculture" = "#4E8542", "Community and Economic Development" = "#C09001", "Education" = "#C65A11")

    # Remember that, because you're using 'reactive', you need to put () after the df to make it into a function
    ggplot(data_Evimp(), aes(x = row, y = Percentage)) +
      geom_col(aes(fill = Topic), width = 0.9, ) +
      geom_text(aes(label = paste(Description, scales::percent(Percentage, accuracy = 1), sep = ", ")), vjust = 0.5, hjust = "right", color = "white", size = 4) +
      scale_y_continuous(expand = c(0, 0)) +
      scale_x_reverse()+
      scale_fill_manual(values = colors)+
      coord_flip() +
      labs(title = "Top Priorities", # We can explore how to add more than one county name
           subtitle = "Percent of respondents who selected 'extremely' or 'very' important") +
      theme(
        #legend.position = "none",
        legend.position = "left",
        plot.title = element_text(size = 18, margin = margin(10, 0, 0, 0)),
        plot.subtitle = element_text(size = 12, margin = margin(10, 0, 10, 0), color = "gray"),
        panel.background = element_rect(fill = NA),
        panel.grid.major = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        axis.text.x = element_blank(),
        # axis.text.y = element_text(size = 11, margin = margin(0, 5, 0, 0)),
        axis.text.y = element_blank()
      )
    })



}

