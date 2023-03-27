# Define server logic 
function(input, output, session) {

  top_20_filtered <- callModule(
    module = selectizeGroupServer,
    id = "my-filters",
    data = data,
    vars = c("COUNTY", "LIVE_V3", "UserLanguage", "DEM_11", "Low_Income_FPL_185",
             "Gender", "AGE"), #add new filters here by adding column name in quotes
    inline = FALSE
  )
  
  refine_top_20 <- reactive({
    top_20_filtered() %>% 
      filter(if_any(all_of(input$race_ethnicity), function(x) {x == 1})) %>% # anonymous functions
      filter(if_any(all_of(input$topical_experience), function(x) {x == 1})) %>%
      filter(if_any(all_of(input$topical_knowledge), function(x) {x == 1}))
  })
  
  # Make a function for refine_top_20 by adding ()
  data_Evimp <- reactive({
    refine_top_20() %>% 
      summarize(across(
        ends_with("Evimp"),
        ~ sum(.x == 1, na.rm = TRUE) / sum(!is.na(.x)) 
      )) %>% 
      
      pivot_longer(
        everything(),
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
      filter(Percentage >= nth(Percentage, 20)) %>% 
      mutate(row = 1:n())
  })
  
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
  

  # Sample size indicator
  
  output$n_indicator <- renderPlot({
    df <- tibble(
      count = c(nrow(refine_top_20()), nrow(data)),
      category = c("filtered", "total")
    ) %>% 
      mutate(
        fraction = count/sum(count),
        start = cumsum(fraction),
        end = c(0, head(start, n=-1))
      )
    
    ggplot(df) +
      geom_arc_bar(aes(
        x0 = 0,
        y0 = 0, 
        r0 = 0.5,
        r = 1,
        #convert to radians and shift by 180º counterclockwise
        start = start * pi - pi/2, 
        end = end * pi - pi/2,
        fill = category
      )) +
      annotate(geom = "text", label = nrow(refine_top_20()), x = 0, y = 0.1, size = 16) +
      scale_fill_manual(values = c("purple", "white")) +
      coord_equal() +
      theme_void() +
      theme(legend.position = "none")
  })
  
  
  # Gender donut
  output$gender_donut <- renderPlotly({
    refine_top_20() %>%
      mutate(Gender = ifelse(is.na(Gender), "No response", Gender)) %>%
      group_by(Gender) %>%
      summarize(count = n()) %>%
      plot_ly(labels = ~Gender, values = ~count) %>%
      add_pie(hole = 0.5) %>%
      layout(title = "Gender",  
             showlegend = TRUE,
             legend = list(orientation = "h"),
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    
  })
  
  # Bach donut
  output$bach_donut <- renderPlotly({
    refine_top_20() %>%
      mutate(Bach_or_higher = ifelse(is.na(Bach_or_higher), "No response", Bach_or_higher)) %>%
      group_by(Bach_or_higher) %>%
      summarize(count = n()) %>%
      plot_ly(labels = ~Bach_or_higher, values = ~count) %>%
      add_pie(hole = 0.5) %>%
      layout(title = "Bachelor's or higher",  
             showlegend = TRUE,
             legend = list(orientation = "h"),
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    
  })
  
  #  Donut for white/non-white
  output$race_donut <- renderPlotly({
    refine_top_20() %>%
      # mutate(non_white = ifelse(is.na(Bach_or_higher), "No response", Bach_or_higher)) %>%
      group_by(non_white) %>%
      summarize(count = n()) %>%
      plot_ly(labels = ~non_white, values = ~count) %>%
      add_pie(hole = 0.5) %>%
      layout(title = "Identity",  
             showlegend = FALSE,
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    
  })


}

