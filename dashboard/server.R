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
    refine_top_20() %>% 
      summarize(across(
        ends_with("Evimp"),
        ~sum(.x == 1, na.rm = TRUE)/n())*100) %>% 
      pivot_longer(ends_with("Evimp"),
                   names_to = "Metric",
                   values_to = "Percentage"
      ) %>%
      arrange(desc(Percentage)) %>% 
      # Remove _EVimp from column names so join with labels works
      mutate(Metric = gsub("_EVimp", "", Metric)) %>%
      # Take top 30
      slice(1:30) %>% 
      left_join(labels) %>% 
      select(Topic, Metric, Description, Percentage) %>% 
      arrange(desc(Percentage)) %>% 
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
        legend.position = "top",
        legend.title = element_blank(),
        plot.title = element_text(size = 18, margin = margin(10, 0, 0, 0)),
        plot.subtitle = element_text(size = 12, margin = margin(10, 0, 10, 0), color = "gray"),
        panel.background = element_rect(fill = NA),
        panel.grid.major = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        axis.text.x = element_blank(),
        # axis.text.y = element_text(size = 11, margin = margin(0, 5, 0, 0)),
        axis.text.y = element_blank()
      ) +
      guides(fill = guide_legend(nrow = 2))
    })
  
  output$Ngauge <- renderPlotly({
    temp <- refine_top_20()
    
    fig <- plot_ly(
      domain = list(x = c(0, 1), y = c(0, 1)),
      value = nrow(temp), # substitute with actual sample size
      title = list(text = "Sample size"),
      type = "indicator",
      mode = "gauge+number",
      # delta = list(reference = 380),
      gauge = list(
        axis =list(range = list(NULL, nrow(data)),# include maximum
                   nticks = 5), 
        threshold = list(
          line = list(color = "red", width = 4),
          thickness = 0.75,
          value = nrow(data))))  # include maximum
    fig <- fig %>%
      layout(margin = list(l = 20,r = 30))
    print(fig)
  })
  
  output$gender_donut <- renderPlotly({
    refine_top_20() %>%
      group_by(GENDER) %>%
      summarize(count = n()) %>%
      plot_ly(labels = ~GENDER, values = ~count) %>%
      add_pie(hole = 0.5) %>%
      layout(title = "Gender",  
             showlegend = TRUE,
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    
  })

  output$race_bar <- renderPlotly({
    refine_top_20() %>%
      select(AIAN, AS, BL, HL, MR, NHPI, WH, NR) %>%
      pivot_longer(cols = everything(), 
                   names_to = "race_ethnicity") %>%
      mutate(race_long = case_when(race_ethnicity == "AIAN" ~ "American Indian or Alaska Native",
                                   race_ethnicity == "AS" ~ "Asian",
                                   race_ethnicity == "BL" ~ "Black or African American", 
                                   race_ethnicity == "HL" ~ "Hispanic or Latinx",
                                   race_ethnicity == "MR" ~ "Multiracial", 
                                   race_ethnicity == "NHPI" ~ "Native Hawaiian or Pacific Islander", 
                                   race_ethnicity == "WH" ~ "White", 
                                   race_ethnicity == "NR" ~ "Prefer not to answer")) %>%
      filter(value == 1) %>%
      group_by(race_long) %>%
      summarize(count = n(),
                frac = n()/nrow(.)) %>%
      plot_ly(x = ~frac, y = ~race_long,
              type = 'bar',
              orientation = 'h') %>%
      layout(xaxis = list(title = FALSE),
             yaxis = list(title = FALSE))
  })

}

