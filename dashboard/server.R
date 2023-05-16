## Custom functions
# TODO: not sure if these custom functions are used anymore

# function to break strings for ggplot
break_string <- function(x, n) {
  # x is a character string
  # n is number of characters before the break
  out_string <- stringi::stri_wrap(x, n)
  out_n <- length(out_string)
  
  ifelse(out_n == 1, out_string, 
         paste0(out_string[1], "\n", out_string[2]))
}

# function to break strings for plotly
break_string2 <- function(x, n) {
  # x is a character string
  # n is number of characters before the break
  out_string <- stringi::stri_wrap(x, n)
  out_n <- length(out_string)
  
  ifelse(out_n == 1, out_string, 
         paste0(out_string[1], "<br>", out_string[2]))
}


# Define server logic 
function(input, output, session) {

# Filtering ---------------------------------------------------------------
  initial_filtered <- callModule(
    module = selectizeGroupServer,
    id = "my-filters",
    data = data,
    vars = c("COUNTY", "LIVE_V3", "UserLanguage", "DEM_11", "Low_Income_FPL_185",
             "Gender", "AGE"), #add new filters here by adding column name in quotes
    inline = FALSE
  )
  
  refine_filtered <- reactive({
    initial_filtered() %>% 
      filter(if_any(all_of(input$race_ethnicity), function(x) {x == 1})) %>% # anonymous functions
      filter(if_any(all_of(input$topical_experience), function(x) {x == 1})) %>%
      filter(if_any(all_of(input$topical_knowledge), function(x) {x == 1}))
  })
  
  # Filtering by county only for demographics tab
  county_filtered <- callModule(
    module = selectizeGroupServer,
    id = "county-filter",
    data = data,
    vars = c("COUNTY"), #add new filters here by adding column name in quotes
    inline = FALSE
  )

# Top 20 bar chart --------------------------------------------------------
  # Make a function to arrange filtered data for top20 bar chart
  data_Evimp <- reactive({
    refine_filtered() %>% 
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
    colors <-
      c(
        "Health and Well-Being" = "#604878",
        "Natural Resources" = "#1B587C",
        "Agriculture" = "#4E8542",
        "Community and Economic Development" = "#C09001",
        "Education" = "#C65A11"
      )
     
    # Do not print if N < = 6
    N <- nrow(refine_filtered())
    
    if(N <= 6) {
      ggplot() +
        annotate("text", x = 1, y = 1, size = 5,
                 label = 'The filters you have applied result in too small a sample to support data visualization. \n Please remove one or more of your filters') +
        theme_void() +
        theme(text = element_text(family = "Open Sans"))
    } else {
    # Remember that, because you're using 'reactive', you need to put () after the df to make it into a function
    ggplot(data_Evimp(), aes(x = row, y = Percentage)) +
      geom_col(aes(fill = Topic), width = 0.9) +
        geom_text(
          aes(label = paste(
            Description, scales::percent(Percentage, accuracy = 1), sep = ", "
          )),
          vjust = 0.5,
          hjust = "right",
          color = "white",
          size = 4
        ) +
        scale_y_continuous(expand = c(0, 0)) +
      scale_x_reverse()+
      scale_fill_manual(name = "Topics: ", 
                        values = colors)+
      coord_flip() +
      labs(
        subtitle = "Percent of respondents who indicated it was “extremely” or “very important” to prioritize this issue in their community") +
      theme(
        text = element_text(family = "Open Sans"),
        legend.position = "top",
        legend.title = element_text(size = 14, face = "bold"),
        legend.text = element_text(size = 12),
        # plot.title = element_text(size = 18, margin = margin(10, 0, 0, 0)),
        plot.subtitle = element_text(size = 16, margin = margin(10, 10, 10, 10), color = "black"),
        panel.background = element_rect(fill = NA),
        panel.grid.major = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank()
      ) +
      guides(fill = guide_legend(nrow = 2))
    }
  })
  

# Sample size indicator ---------------------------------------------------
  
  output$n_indicator <- renderPlot({
    req(refine_filtered())
    
    if(nrow(refine_filtered()) <= 6) {
      ggplot() +
        geom_arc_bar(aes(
          x0 = 0,
          y0 = 0, 
          r0 = 0.5,
          r = 1,
          #convert to radians and shift by 180º counterclockwise
          start = -pi/2, 
          end = pi/2,
          
        ), fill = "white") +
        annotate(geom = "text", label = "Data \n suppressed", x = 0, y = 0.2, size = 6) +
        coord_equal() +
        theme_void() +
        theme(legend.position = "none",
              text = element_text(family = "Open Sans"))
    } else{
    
    df <-
      tibble(
        # get the fraction of rows (observations) for after and before filtering
        fraction = c(
          nrow(refine_filtered())/nrow(data),
          (nrow(data)-nrow(refine_filtered()))/nrow(data)
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
      annotate(geom = "text", label = nrow(refine_filtered()), x = 0, y = 0.1, size = 12) +
      scale_fill_manual(values = c("#418ab3", "white")) +
      coord_equal() +
      theme_void() +
      theme(legend.position = "none",
            text = element_text(family = "Open Sans"))   
    }
  })
  

# Donut plots -------------------------------------------------------------
  # Gender donut

  output$gender_donut <- renderPlotly({
    
    # Establish universal colors
    colors_gender <- c("Woman" = "#1b587c", "Man" = "#9f2936", 
                       "Non-binary" = "#f07f09", "No Response" = "#f2f2f2")
    
    # Wrangle data for donut
    gender_count <- refine_filtered() %>%
      mutate(Gender = ifelse(is.na(Gender), "No Response", Gender),
             Gender = factor(Gender, levels = c("Woman", "Man",
                                                "Non-binary", "No Response"))) %>%
      group_by(Gender) %>%
      summarize(count = n())
    
    # Establish specific colors needed
    colors_temp <- colors_gender[intersect(gender_count$Gender, names(colors_gender))]

    # Do not print if N < = 6
    N <- nrow(refine_filtered())
    
    if(N <= 6) {
      plotly_empty()
    } else {
    gender_count %>%
      plot_ly(labels = ~Gender, values = ~count,
              textinfo = "label", # "label+percent"
              textfont = list(size = 10),
              marker = list(colors = colors_temp)) %>%
      add_pie(hole = 0.5) %>%
      layout(title = "Gender",  
             showlegend = FALSE,
             legend = list(orientation = "h"),
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    }
  })

   # Education donut

  output$edu_donut <- renderPlotly({
    
    # Establish universal colors
    colors_education <- c("Yes" = "#1b587c", "No" = "#9f2936", 
                          "No Response" = "#f2f2f2")
    
    # Wrangle data for donut
    edu_count <- refine_filtered() %>%
      mutate(Bach_or_higher = ifelse(is.na(Bach_or_higher), "No Response", Bach_or_higher),
             Bach_or_higher = factor(Bach_or_higher, levels = c("No", "Yes", "No Response"))) %>%
      group_by(Bach_or_higher) %>%
      summarize(count = n()) 
    
    # Establish specific colors needed
    colors_temp <- colors_education[intersect(edu_count$Bach_or_higher, names(colors_education))]
    
    # Do not print if N < = 6
    N <- nrow(refine_filtered())
    
    if(N <= 6) {
      plotly_empty()
    } else {
    edu_count %>%
      plot_ly(labels = ~Bach_or_higher, values = ~count,
              textinfo = "label", # "label+percent"
              textfont = list(size = 10),
              marker = list(colors = colors_temp)) %>%
      add_pie(hole = 0.5) %>%
      layout(title = "Bachelor's or higher",  
             showlegend = FALSE,
             legend = list(orientation = "h"),
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    }
  })
  
  
  #  Plotly bar graph for race/ethnicity

  output$race_bar <- renderPlotly({
    
    # Wrangle data for donut
    race_count <- refine_filtered() %>%
      select(race_vec) %>%
      pivot_longer(cols = everything(), 
                   names_to = "race_ethnicity") %>%
        filter(value == 1) %>%
      group_by(race_ethnicity) %>%
      summarize(count = n(),
                frac = n()/nrow(refine_filtered())) %>%
      mutate(percent = sprintf("%d%%", round(frac*100)),
             race_ethnicity = factor(race_ethnicity, levels = c("American Indian or Alaska Native", 
                                                                "Asian",
                                                                "Black or African American", 
                                                                "Hispanic or Latino", 
                                                                "Multiracial", 
                                                                "Native Hawaiian or Other Pacific Islander", 
                                                                "White", "Prefer not to answer"))) %>%
      arrange(race_ethnicity)
    
    # Add string breaks
    race_count$race_labs <- sapply(race_count$race_ethnicity, break_string2, 25)
    race_count$race_labs <- factor(race_count$race_labs, levels = race_count$race_labs)
    
    # Do not print if N < = 6
    N <- nrow(refine_filtered())
    
    if(N <= 6) {
      plotly_empty()
    } else {
      race_count %>%
        plot_ly(x = ~count, y = ~fct_rev(race_labs),
                type = 'bar',
                orientation = 'h',
                text = ~percent,
                marker = list(color = "#1b587c")) %>% 
        # ~paste(percent, race_ethnicity)) %>%
        layout(title = "Race/Ethnicity",
               xaxis = list(title = "", showgrid = FALSE, zeroline = FALSE),
               yaxis = list(title = "", showgrid = FALSE, zeroline = FALSE))
    }
    
  })

# By Topic bar chart ------------------------------------------------------

  # Make a function to wrangle data for by topic bar charts
  
  data_bytopic <- reactive({
    req(input$topic)
    
    cnames <- labels$Metric[labels$Topic == input$topic] 
    
    # Make E + V only for ranking purposes
    rank_EV <- refine_filtered() %>% 
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
      mutate(percentEV = totalEV/total*100) %>% 
      arrange(desc(percentEV)) %>%
      left_join(labels, by = "Metric") %>%
      pull(Description)
    
    # Output long dataset for plotting
    refine_filtered() %>% 
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
      mutate(percent_round = round(n/total*100),
             percent = n/total*100) %>%
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
    
    cnames <- labels$Metric[labels$Topic == input$topic]
    
    # For range of sample size (we're not including NAs)
    refine_filtered() %>% 
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
    
    # Do not print if N < = 6
    N <- nrow(refine_filtered())
    
    if(N <= 6) {
      ggplot() +
        annotate("text", x = 1, y = 1, size = 5,
                 label = "The filters you have applied result in too small a sample to support data visualization. Please remove one or more of your filters") +
        theme_void() +
        theme(text = element_text(family = "Open Sans"))
    } else {
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
                    label = paste0(percent_round, "%")),
                vjust = 0.5, hjust = 0.5,
                color = "white", size = 4) +
      scale_fill_manual(name = "Importance level: ",
                        values = colors) +
      scale_x_discrete(labels = xlabs) +
      coord_flip() +
      guides(fill = guide_legend(reverse = TRUE)) +
      labs(subtitle = paste0("Between ", nrange[1], " and ", nrange[2], 
                             " participants responded to each item")) +
      theme(
        text = element_text(family = "Open Sans"),
        legend.position = "top",
        legend.title = element_text(size = 14, face = "bold"),
        legend.text = element_text(size = 12),
        plot.subtitle = element_text(size = 16, hjust = 0, color = "black"),
        plot.title.position = "plot",
        panel.background = element_rect(fill = NA),
        panel.grid.major = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = 12, margin = margin(0, 5, 0, 0), color = "black")
      )
    }
    
  })
  
  #### Demographics page ####
  
  output$az_map <- renderPlot({
    # Unique counties
    counties <- unique(county_filtered()$COUNTY)
    
    # Create map of selected counties
    az_counties <- map_data("county", region = "arizona") |>
      mutate(COUNTY = str_to_title(subregion),
             selected = case_when(COUNTY %in% counties ~ TRUE,
                                  .default = FALSE))
    
    # Label with sample size of respondents
    N <- nrow(county_filtered())
    
    ggplot(data = az_counties,
           mapping = aes(x = long, y = lat,
                         group = group, fill = selected)) + 
      geom_polygon(color = "black", show.legend = FALSE) +
      scale_fill_manual(values = c("TRUE" = "#2b556d", "FALSE" = "#89c3e5")) +
      coord_map() +
      theme_void() +
      annotate("text", x = -118, y = 34.5, label = paste0("N = \n", N),
               size = 10,
               vjust = 0.25,
               hjust = 0) +
      theme(text = element_text(family = "Open Sans"))
    
  })
  
  output$county_bar <- renderPlotly({
    # Create color vec to highlight selected counties
    sel <- county_filtered() %>%
      select(COUNTY) %>%
      drop_na() %>%
      pull() %>%
      unique() %>%
      str_sort()
    
    ind <- county_vec %in% sel
    color_vec <- ifelse(ind == TRUE, "#2b556d", "#89c3e5")

    # Static plotly - shows all counties
    data %>% 
      group_by(COUNTY) %>%
      summarize(count = n(),
                frac = n()/nrow(.)) %>%
      drop_na() %>%
      mutate(percent = sprintf("%d%%", round(frac*100))) %>%
      plot_ly(x = ~count, y = ~fct_rev(COUNTY),
              type = 'bar',
              orientation = 'h',
              text = ~percent,
              marker = list(color = color_vec)) %>% 
      layout(title = "Respondents by County",
             xaxis = list(title = "", showgrid = FALSE, zeroline = FALSE),
             yaxis = list(title = "", showgrid = FALSE, zeroline = FALSE))
    
  })
  
  output$rural_donut <- renderPlotly({
    # Establish universal colors
    colors_location <- c("Rural" = "#4e8542", "Urban" = "#594a6a", "No Response" = "#f2f2f2")
    
    # Wrangle data for donut
    location_count <- county_filtered() %>%
    mutate(LIVE_V3 = ifelse(is.na(LIVE_V3), "No Response", LIVE_V3)) %>%
      group_by(LIVE_V3) %>%
      summarize(count = n())
      
    # Establish specific colors needed
    colors_temp <- colors_location[intersect(location_count$LIVE_V3, 
                                         names(colors_location))]
    
    location_count %>%
      plot_ly(labels = ~LIVE_V3, values = ~count,
              textinfo = "label", # "label+percent"
              textfont = list(size = 10),
              marker = list(colors = colors_temp)) %>%
      add_pie(hole = 0.5) %>%
      layout(title = "Location",  
             # margin = list( top = 200, b = 50, l = 50, r = 50 ),
             showlegend = FALSE,
             # legend = list(orientation = "h"),
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             margin = list(t = 50)) # You can adjust t to see how it looks on the plot)
    
  })
  
  output$language_donut <- renderPlotly({
    # Establish universal colors
    colors_lang <- c("English" = "#2b556d", "Spanish" = "#9f2936")
    
    # Wrangle data for donut
    lang_count <- county_filtered() %>%
      group_by(UserLanguage) %>%
      summarize(count = n()) 
    # %>%
    #   mutate(frac = count / sum(count), 
    #          percent = paste0(round(frac*100), "%"))  
    
    # Establish specific colors needed
    colors_temp <- colors_lang[intersect(lang_count$UserLanguage, 
                                         names(colors_lang))]
    

    lang_count %>%
      plot_ly(labels = ~UserLanguage, 
              values = ~count, 
              textinfo = "label", # "label+percent"
              textfont = list(size = 10),
              marker = list(colors = colors_temp)) %>%
      add_pie(hole = 0.5) %>%
      layout(title = "Language", 
             showlegend = FALSE,
             # legend = list(orientation = "h"),
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             margin = list(t = 50)) # You can adjust t to see how it looks on the plot
    
  })
  
  output$gender_donut_county <- renderPlotly({
    
    # Establish universal colors
    colors_gender <- c("Woman" = "#1b587c", "Man" = "#9f2936", 
                       "Non-binary" = "#f07f09", "No Response" = "#f2f2f2")
    
    # Wrangle data for donut
    gender_count <- county_filtered() %>%
      mutate(Gender = ifelse(is.na(Gender), "No Response", Gender),
             Gender = factor(Gender, levels = c("Woman", "Man",
                                                "Non-binary", "No Response"))) %>%
      group_by(Gender) %>%
      summarize(count = n())
    
    # Establish specific colors needed
    colors_temp <- colors_gender[intersect(gender_count$Gender, names(colors_gender))]
    
    gender_count %>%
        plot_ly(labels = ~Gender, values = ~count,
                textinfo = "label", # "label+percent"
                textfont = list(size = 10),
                marker = list(colors = colors_temp)) %>%
        add_pie(hole = 0.5) %>%
        layout(title = "Gender",  
               showlegend = FALSE,
               # legend = list(orientation = "h"),
               xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               margin = list(t = 50)) # You can adjust t to see how it looks on the plot
    
  })
  
  # CE_USER donut
  
  output$ce_user_donut <- renderPlotly({
    
    # Establish universal colors
    colors_ce_user <- c("Yes" = "#2b556d", "No" = "#9f2936", "No Response" = "#f2f2f2")
    
    # Wrangle count data
    ceuser_count <- county_filtered() %>%
      group_by(CE_USER) %>%
      summarize(count = n()) %>%
      mutate(frac = count / sum(count), 
             percent = paste0(round(frac*100), "%"))
    
    # Establish specific colors needed
    colors_temp <- colors_ce_user[intersect(ceuser_count$CE_USER, names(colors_ce_user))]
    
    ceuser_count %>%
      plot_ly(labels = ~CE_USER, values = ~count,
              textinfo = "label", # "label+percent"
              textfont = list(size = 10),
              marker = list(colors = colors_temp)) %>%
      add_pie(hole = 0.5) %>%
      layout(title = "User of Extension",  
             showlegend = FALSE,
             # legend = list(orientation = "h"),
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             margin = list(t = 50)) 
  })
  
  # CE_EXPOSED donut
  
  output$ce_exposed_donut <- renderPlotly({
    
    # Establish universal colors
    colors_ce_exposed <- c("Yes" = "#2b556d", "No" = "#9f2936", "No Response" = "#f2f2f2")
    
    # Wrangle count data
    ceexposed_count <- county_filtered() %>%
      group_by(CE_EXPOSED) %>%
      summarize(count = n()) %>%
      mutate(frac = count / sum(count), 
             percent = paste0(round(frac*100), "%"))
    
    # Establish specific colors needed
    colors_temp <- colors_ce_exposed[intersect(ceexposed_count$CE_EXPOSED, names(colors_ce_exposed))]
    
    ceexposed_count %>%
      plot_ly(labels = ~CE_EXPOSED, values = ~count,
              textinfo = "label", # "label+percent"
              textfont = list(size = 10),
              marker = list(colors = colors_temp)) %>%
      add_pie(hole = 0.5) %>%
      layout(title = "Familiar with Extension",  
             showlegend = FALSE,
             # legend = list(orientation = "h"),
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             margin = list(t = 50)) 
  })
  
  # Race barchart for demographics tab only
  output$race_bar2 <- renderPlotly({
    
    # Wrangle data for bar chart
    race_count <- county_filtered() %>% # 
      select(race_vec) %>%
      pivot_longer(cols = everything(), 
                   names_to = "race_ethnicity") %>%
      filter(value == 1) %>%
      group_by(race_ethnicity) %>%
      summarize(count = n(),
                frac = n()/nrow(county_filtered())) %>%
      mutate(percent = sprintf("%d%%", round(frac*100)),
             race_ethnicity = factor(race_ethnicity, levels = c("American Indian or Alaska Native", 
                                                                "Asian",
                                                                "Black or African American", 
                                                                "Hispanic or Latino", 
                                                                "Multiracial", 
                                                                "Native Hawaiian or Other Pacific Islander", 
                                                                "White", "Prefer not to answer"))) %>%
      arrange(race_ethnicity)
    
    race_count$race_labs <- sapply(race_count$race_ethnicity, break_string2, 25)
    race_count$race_labs <- factor(race_count$race_labs, levels = race_count$race_labs)
    
    race_count %>%
        plot_ly(x = ~count, y = ~fct_rev(race_labs),
                type = 'bar',
                orientation = 'h',
                text = ~percent,
                marker = list(color = "#1b587c")) %>% 
        # ~paste(percent, race_ethnicity)) %>%
        layout(title = "Race/Ethnicity",  
               xaxis = list(title = "", showgrid = FALSE, zeroline = FALSE),
               yaxis = list(title = "", showgrid = FALSE, zeroline = FALSE))
    
    
  })
  
  # Age bar chart
  output$age_bar <- renderPlotly({
    
    county_filtered() %>%
      group_by(AGE) %>%
      summarize(count = n(),
                frac = n()/nrow(.)) %>%
      mutate(percent = sprintf("%d%%", round(frac*100))) %>%
      plot_ly(x = ~count, y = ~fct_rev(AGE),
              type = 'bar',
              orientation = 'h',
              text = ~percent,
              marker = list(color = "#4e8542")) %>% 
      layout(title = "Age",
             xaxis = list(title = "", showgrid = FALSE, zeroline = FALSE),
             yaxis = list(title = "", showgrid = FALSE, zeroline = FALSE))
    
  })
  
  output$edu_bar <- renderPlotly({
    
    edu_count <- county_filtered() %>%
      drop_na(DEM_11) %>%
      mutate(DEM_11 = factor(DEM_11, levels = c("Graduate or professional degree",
                                                "Bachelor's degree",
                                                "Associate's degree",
                                                "Trade/technical/vocational training",
                                                "Some college",
                                                "High school diploma or GED",
                                                "Less than high school diploma",
                                                "Prefer not to answer"))) %>%
      group_by(DEM_11) %>%
      summarize(count = n()) %>%
      arrange(desc(count)) %>%
      mutate(percent = paste0(round(count/sum(count)*100), "%"))

    edu_count %>%
      plot_ly(x = ~count, y = ~fct_rev(DEM_11),
              type = 'bar',
              orientation = 'h',
              text = ~percent,
              marker = list(color = "#9f2936")) %>%
      layout(title = "Educational Attainment",
             showlegend = FALSE,
             xaxis = list(title = "", showgrid = FALSE, zeroline = FALSE),
             yaxis = list(title = "", showgrid = FALSE, zeroline = FALSE))
    
    
  })
  
  output$income_bar <- renderPlotly({
    county_filtered() %>%
      drop_na(DEM_13) %>%
      mutate(DEM_13 = fct_relevel(factor(DEM_13), c("$200,000 and above",
                                                        "$150,000 to $199,999",
                                                        "$100,000 to $149,999",
                                                        "$75,000 to $99,999",
                                                        "$50,000 to $74,999",
                                                        "$35,000 to $49,999",
                                                        "$25,000 to $34,999",
                                                        "Less than $25,000",
                                                        "I don't know", 
                                                        "Prefer not to answer"))) %>%
      group_by(DEM_13) %>%
      summarize(count = n()) %>%
      arrange(desc(count)) %>%
      mutate(percent = paste0(round(count/sum(count)*100), "%")) %>%
      plot_ly(x = ~count, y = ~fct_rev(DEM_13),
              type = 'bar',
              orientation = 'h',
              text = ~percent,
              marker = list(color = "#594a6a")) %>%
      layout(title = "Household Income",
             showlegend = FALSE,
             xaxis = list(title = "", showgrid = FALSE, zeroline = FALSE),
             yaxis = list(title = "", showgrid = FALSE, zeroline = FALSE))
    
    
    
    
  })
  
  # Information source bar
  output$info_bar <- renderPlotly({
    info_vec <- c("Physical brochure, fact sheet, article, or similar" = "DEM_14_1",
                  "Talk with an expert" = "DEM_14_2",
                  "In-person class or workshop" = "DEM_14_3",
                  "Online class or workshop" = "DEM_14_4",
                  "Radio" = "DEM_14_6",
                  "Website or online article" = "DEM_14_7",
                  "Video" = "DEM_14_8",
                  "Social media" = "DEM_14_9",
                  "Listening session/ community conversation" = "DEM_14_10",
                  "Prefer not to answer" = "DEM_14_NR"
    )
    
    
    info_count <- county_filtered() %>%
      select(all_of(info_vec)) %>%
      pivot_longer(cols = everything(), 
                   names_to = "information_type") %>%
      mutate(information_type = factor(information_type, levels = c("Physical brochure, fact sheet, article, or similar", 
                                                                    "Talk with an expert",
                                                                    "In-person class or workshop",
                                                                    "Online class or workshop",
                                                                    "Radio",
                                                                    "Website or online article",
                                                                    "Video",
                                                                    "Social media",
                                                                    "Listening session/ community conversation",
                                                                    "Prefer not to answer"
      ))) %>%
      filter(!is.na(value)) %>%
      group_by(information_type) %>%
      summarize(count = n(),
                frac = n()/nrow(county_filtered())) %>%
      mutate(percent = paste0(round(frac * 100), "%"))
    
    # Add string breaks
    info_count$info_labs <- sapply(info_count$information_type, break_string2, 30)
    info_count$info_labs <- factor(info_count$info_labs, levels = info_count$info_labs)
    
    
      info_count %>%
        plot_ly(x = ~count, y = ~fct_rev(info_labs),
              type = 'bar',
              orientation = 'h',
              text = ~percent,
              # hoverinfo = "text",
              marker = list(color = "#594a6a")) %>% 
      layout(title = "Preferred Information Sources",
             showlegend = FALSE,
             xaxis = list(title = "", showgrid = FALSE, zeroline = FALSE),
             yaxis = list(title = "", showgrid = FALSE, zeroline = FALSE))
  })

}

