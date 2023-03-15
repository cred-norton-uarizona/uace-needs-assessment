library(pins)
library(tidyverse)
library(plotly)

board <- board_connect()
data <- pin_read(board, "terrace/uace-na")


# Gauge chart for sample size

fig <- plot_ly(
  domain = list(x = c(0, 1), y = c(0, 1)),
  value = nrow(data), # substitute with actual sample size
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
  layout(margin = list(l=20,r=30))
fig
# Demographic distribution/donut plots, interactive
# Urban or Rural
# LIVE_V3

data %>%
  group_by(LIVE_V3) %>%
  summarize(count = n()) %>%
  plot_ly(labels = ~LIVE_V3, values = ~count) %>%
  add_pie(hole = 0.5) %>%
  layout(title = "Location",  
         showlegend = TRUE,
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))

# UserLanguage
data %>%
  group_by(UserLanguage) %>%
  summarize(count = n()) %>%
  plot_ly(labels = ~UserLanguage, values = ~count) %>%
  add_pie(hole = 0.5) %>%
  layout(title = "Language",  
         showlegend = TRUE,
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))

  
# GENDER
data %>%
  group_by(GENDER) %>%
  summarize(count = n()) %>%
  plot_ly(labels = ~GENDER, values = ~count) %>%
  add_pie(hole = 0.5) %>%
  layout(title = "Gender",  
         showlegend = TRUE,
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))

  
# AGE
data %>%
  group_by(AGE) %>%
  summarize(count = n()) %>%
  plot_ly(labels = ~AGE, values = ~count) %>%
  add_pie(hole = 0.5) %>%
  layout(title = "Age",  
         showlegend = TRUE,
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))

# DEM_11 # educational attainment, maybe don't show all?
data %>%
  group_by(DEM_11) %>%
  summarize(count = n()) %>%
  plot_ly(labels = ~DEM_11, values = ~count) %>%
  add_pie(hole = 0.5) %>%
  layout(title = "Educational Attainment",  
         showlegend = FALSE,
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))

# Low_Income_FPL_100 # to simplify?
data %>%
  group_by(Low_Income_FPL_100) %>%
  summarize(count = n()) %>%
  plot_ly(labels = ~Low_Income_FPL_100, values = ~count) %>%
  add_pie(hole = 0.5) %>%
  layout(title = "Low Income",  
         showlegend = TRUE,
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))

# CE_EXPOSED
data %>%
  group_by(CE_EXPOSED) %>%
  summarize(count = n()) %>%
  plot_ly(labels = ~CE_EXPOSED, values = ~count) %>%
  add_pie(hole = 0.5) %>%
  layout(title = "County Extension Exposure",  
         showlegend = TRUE,
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))

# CE_USER
data %>%
  group_by(CE_USER) %>%
  summarize(count = n()) %>%
  plot_ly(labels = ~CE_USER, values = ~count) %>%
  add_pie(hole = 0.5) %>%
  layout(title = "County Extension User",  
         showlegend = TRUE,
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))


# race_ethnicity
data %>%
  select(AIAN, AS, BL, HL, MR, NHPI, WH, NR) %>%
  pivot_longer(cols = everything(), 
               names_to = "race_ethnicity") %>%
  filter(value == 1) %>%
  group_by(race_ethnicity) %>%
  summarize(count = n(),
            frac = n()/nrow(.)) %>%
  plot_ly(x = ~frac, y = ~race_ethnicity,
          type = 'bar',
          orientation = 'h')

race_vec <- c("American Indian or Alaska Native" = "AIAN",
              "Asian" = "AS",
              "Black or African American" = "BL", 
              "Hispanic or Latinx" =  "HL",
              "Multiracial" = "MR", 
              "Native Hawaiian or Pacific Islander" = "NHPI", 
              "White" = "WH" , 
              "Prefer not to answer" = "NR") 