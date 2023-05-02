# Load the data and libraries  -----------------------------------------------------------
library(tidyverse)
library(pins)
library(plotly)



board <- board_connect()
data <- pin_read(board, "terrace/uace-na")
labels <- read.csv(here::here("data", "labels.csv"))

# Bar chart of participation across the state

# Donut for Rural versus Urban
colors_rural_urban<- c("Rural" = "#4e8542", "Urban" = "#2b556d", "No Response" = "#594a6a")

data %>%
  mutate(LIVE_V3 = ifelse(is.na(LIVE_V3), "No Response", LIVE_V3)) %>%
  mutate(LIVE_V3 = factor(LIVE_V3, levels = names(colors_rural_urban))) %>%
  group_by(LIVE_V3) %>%
  summarize(count = n()) %>%
  plot_ly(labels = ~LIVE_V3, values = ~count) %>%
  add_pie(hole = 0.5, marker = list(colors = colors_rural_urban)) %>%
  layout(title = "Location",  
         showlegend = TRUE,
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))

         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))


# Donut for Spanish versus English

# Define color scheme
colors_language <- c("English" = "#9f2936", "Spanish" = "#2b556d", "No Response" = "#594a6a")

# Replace "EN" and "ES" with "English" and "Spanish" respectively
data$UserLanguage[data$UserLanguage == "EN"] <- "English"
data$UserLanguage[data$UserLanguage == "ES"] <- "Spanish"

# Convert UserLanguage to factor and set factor levels to match color scheme
data$UserLanguage <- factor(ifelse(is.na(data$UserLanguage), "No Response", data$UserLanguage), levels = c("English", "Spanish", "No Response"))

# Plot pie chart
data %>%
  count(UserLanguage) %>%
  plot_ly(labels = ~UserLanguage, values = ~n) %>%
  add_pie(hole = 0.5, marker = list(colors = colors_language)) %>%
  layout(title = "User Language",  
         showlegend = TRUE,
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))


# Bar chart for Familiar with Extension, Extension User



# Bar chart of race/ethnicity

# Bar chart of educational attainment

# Bar chart of household income

# Donut chart for gender



# Bar chart for age