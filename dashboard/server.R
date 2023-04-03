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
  
  # Make a function to arrange filtered data for top20 bar chart
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
      labs(
        # title = "Top Priorities", # We can explore how to add more than one county name
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
    data_filtered <- refine_top_20()
    
    df <-
      tibble(
        # get the fraction of rows (observations) for after and before filtering
        fraction = c(
          nrow(data_filtered)/nrow(data),
          (nrow(data)-nrow(data_filtered))/nrow(data)
        ),
        category = c("filtered", "total")
      ) |> 
      # convert to coordinates of a bar
      mutate(
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
      annotate(geom = "text", label = nrow(refine_top_20()), x = 0, y = 0.1, size = 12) +
      scale_fill_manual(values = c("#418ab3", "white")) +
      coord_equal() +
      theme_void() +
      theme(legend.position = "none")
  })
  
  
  # Gender donut

  output$gender_donut <- renderPlotly({
    
    colors_gender <- c("Woman" = "#1b587c", "Man" = "#9f2936", 
                       "Non-binary" = "#f07f09", "No Response" = "#f2f2f2")
    
    refine_top_20() %>%
      mutate(Gender = ifelse(is.na(Gender), "No Response", Gender),
             Gender = factor(Gender, levels = c("Woman", "Man",
                                                "Non-binary", "No Response"))) %>%
      group_by(Gender) %>%
      summarize(count = n()) %>%
      plot_ly(labels = ~Gender, values = ~count, 
              textinfo = "label", # "label+percent"
              textfont = list(size = 10),
              marker = list(colors = colors_gender)) %>%
      add_pie(hole = 0.5) %>%
      layout(title = "Gender",  
             showlegend = FALSE,
             legend = list(orientation = "h"),
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  })
  
   # Bach donut
  colors_education <- c("#1b587c", "#9f2936", "#f2f2f2")
  
  output$bach_donut <- renderPlotly({
    refine_top_20() %>%
      mutate(Bach_or_higher = ifelse(is.na(Bach_or_higher), "No Response", Bach_or_higher),
             Bach_or_higher = factor(Bach_or_higher, levels = c("No", "Yes", "No Response"))) %>%
      group_by(Bach_or_higher) %>%
      summarize(count = n()) %>%
      plot_ly(labels = ~Bach_or_higher, values = ~count,
              textinfo = "label", # "label+percent"
              textfont = list(size = 10),
              marker = list(colors = colors_education)) %>%
      add_pie(hole = 0.5) %>%
      layout(title = "Bachelor's or higher",  
             showlegend = FALSE,
             legend = list(orientation = "h"),
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  })
  
  
  #  Donut for white/non-white
  
  colors_white <- c("#1b587c", "#9f2936")
  output$race_donut <- renderPlotly({
    refine_top_20() %>%
      # mutate(non_white = ifelse(is.na(Bach_or_higher), "No response", Bach_or_higher)) %>%
      group_by(non_white) %>%
      summarize(count = n()) %>%
      mutate(non_white = case_when(non_white == 0 ~ "White",
                                   non_white == 1 ~ "Non-White")) %>%
      plot_ly(labels = ~non_white, values = ~count,
              textinfo = "label", # "label+percent"
              textfont = list(size = 10),
              marker = list(colors = colors_white)) %>%
      add_pie(hole = 0.5) %>%
      layout(title = "Identity",  
             showlegend = FALSE,
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    
  })
  
  # Make a function to wrangle data for by topic bar charts
  
  data_bytopic <- reactive({
    req(input$topic)
    
    cnames <- labels$Metric[labels$Topic == input$topic] # substitute with input$topic
    
    # Make E + V only for ranking purposes
    rank_EV <- refine_top_20() %>% # substitute with refine_top_20()
      select(all_of(cnames)) %>%
      pivot_longer(cols = everything(),
                   names_to = "Metric",
                   values_to = "Response") %>%
      group_by(Metric, Response) %>%
      summarize(n = n()) %>%
      drop_na() %>%
      group_by(Metric) %>%
      mutate(total = sum(n)) %>%
      filter(Response %in% 3:4) %>%
      summarize(totalEV = sum(n),
                total = unique(total)) %>%
      mutate(percentEV = round(totalEV/total*100)) %>%
      arrange(desc(percentEV)) %>%
      left_join(labels, by = "Metric") %>%
      pull(Description)
    
    # Output long dataset for plotting
    refine_top_20() %>% # substitute with refine_top_20()
      select(all_of(cnames)) %>%
      pivot_longer(cols = everything(),
                   names_to = "Metric",
                   values_to = "Response") %>%
      group_by(Metric, Response) %>%
      summarize(n = n()) %>%
      drop_na() %>%
      group_by(Metric) %>%
      mutate(total = sum(n)) %>%
      ungroup() %>%
      mutate(percent = round(n/total*100)) %>%
      left_join(labels, by = "Metric") %>%
      mutate(Description = factor(Description, levels = rank_EV),
             Response = case_when(Response == 0 ~ "Not at all",
                                  Response == 1 ~ "A little",
                                  Response == 2 ~ "Somewhat",
                                  Response == 3 ~ "Very",
                                  Response == 4 ~ "Extremely"),
             Response = factor(Response, level = c("Not at all",
                                                   "A little",
                                                   "Somewhat", 
                                                   "Very",
                                                   "Extremely")))
    
  })
  
  # Make reactive function for pulling range of N for by topic bar chart
  nrange <- reactive({
    req(input$topic)
    
    cnames <- labels$Metric[labels$Topic == input$topic] # substitute with input$topic
    
    # For range of sample size (we're not including NAs)
    refine_top_20() %>% # substitute with refine_top_20()
      select(all_of(cnames)) %>%
      pivot_longer(cols = everything(),
                   names_to = "Metric",
                   values_to = "Response") %>%
      drop_na() %>%
      count(Metric) %>%
      summarize(range = range(n)) %>%
      pull(range)
  })
  
  # By topic bar charts
  
  output$bytopicbar <- renderPlot({
    data_bytopic <- data_bytopic()
    nrange <- nrange()
    
    if(input$topic == "Health and Well-Being"){
      colors <- c("Extremely" = "#30243c", "Very" = "#5a4a6a", "Somewhat" = "#a088b7", "A little" = "#bfafcf", "Not at all" = "#dfd7e7")
      } else if(input$topic == "Education") {
        colors <- c("Extremely" = "#783f05", "Very" = "#b45f07", "Somewhat" = "#f9b268", "A little" = "#fbcc9a", "Not at all" = "#fde5cd")
        } else if(input$topic == "Agriculture"){
          colors <- c("Extremely" = "#274221", "Very" = "#4e7345", "Somewhat" = "#8ec182", "A little" = "#b3d6ac", "Not at all" = "#d9ead5")
        } else if(input$topic == "Natural Resources") {
          colors <- c("Extremely" = "#0e2c3e", "Very" = "#2b556d", "Somewhat" = "#4ea5d8", "A little" = "#89c3e5", "Not at all" = "#c4e1f2")
        } else if(input$topic == "Community and Economic Development") {
          colors <- c("Extremely" = "#c39001", "Very" = "#f5b501", "Somewhat" = "#fecf4c", "A little" = "#fee398", "Not at all" = "#fff6dd")
          }
    
    # data labels - only for top 3 rankings
    percent_labels <- data_bytopic %>%
      filter(Response %in% c("Extremely", "Very", "Somewhat")) %>% 
      arrange(Description, desc(Response)) %>%
      group_by(Description) %>%
      mutate(location_half = percent/2,
             location_prev = lag(percent),
             location_prev2 = lag(location_prev),
             y = rowSums(across(starts_with("location")), 
                         na.rm = TRUE))
    
    # Make vector of x labels
    xlabs <- sapply(levels(data_bytopic$Description), break_string, 50)
    
    ggplot() +
      geom_col(data = data_bytopic, aes(x = fct_rev(Description), 
                                        y = percent,
                                        fill = Response)) +
      geom_text(data = percent_labels,
                aes(x = fct_rev(Description), 
                    y = y, 
                    label = paste0(percent, "%")),
                vjust = 0.5, hjust = 0.5,
                color = "white", size = 4) +
      scale_fill_manual(values = colors) +
      scale_x_discrete(labels = xlabs) +
      coord_flip() +
      guides(fill = guide_legend(reverse = TRUE)) +
      labs(subtitle = paste0("Between ", nrange[1], " and ", nrange[2], 
                             " participants responded to each item")) +
      theme(
        #legend.position = "none",
        legend.position = "top",
        legend.title = element_blank(),
        plot.title = element_text(size = 18, margin = margin(10, 0, 0, 0)),
        plot.subtitle = element_text(size = 14, margin = margin(10, 0, 10, 0), color = "gray"),
        panel.background = element_rect(fill = NA),
        panel.grid.major = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = 12, margin = margin(0, 5, 0, 0))
        # axis.text.y = element_blank()
      )
    
  })
  

}

