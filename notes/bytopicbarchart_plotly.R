# Ranked by topic
# Load the data and libraries  -----------------------------------------------------------
library(tidyverse)
library(pins)
library(plotly)


board <- board_connect()
data <- pin_read(board, "terrace/uace-na")
labels <- read.csv(here::here("data", "labels.csv"))

cnames <- labels$Metric[labels$Topic == "Agriculture"] # substitute with input$topic

# For range of sample size (we're not including NAs)
n_range <- data %>% # substitute with refine_top_20()
  select(all_of(cnames)) %>%
  pivot_longer(cols = everything(),
               names_to = "Metric",
               values_to = "Response") %>%
  drop_na() %>%
  count(Metric) %>%
  summarize(range = range(n)) %>%
  pull(range)

# Make E + V only for ranking purposes
rank_EV <- data %>% # substitute with refine_top_20()
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









  
# # Get long dataset for plotting
# data_bytopic <- data %>% # substitute with refine_top_20()
#   select(all_of(cnames)) %>%
#   pivot_longer(cols = everything(),
#                names_to = "Metric",
#                values_to = "Response") %>%
#   group_by(Metric, Response) %>%
#   summarize(n = n()) %>%
#   drop_na() %>%
#   group_by(Metric) %>%
#   mutate(total = sum(n)) %>%
#   ungroup() %>%
#   mutate(percent = round(n/total*100)) %>%
#   left_join(labels, by = "Metric") %>%
#   mutate(Description = factor(Description, levels = rank_EV),
#          Response = case_when(Response == 0 ~ "Not at all",
#                               Response == 1 ~ "A little",
#                               Response == 2 ~ "Somewhat",
#                               Response == 3 ~ "Very",
#                               Response == 4 ~ "Extremely"),
#          Response = factor(Response, level = c("Not at all",
#                                                "A little",
#                                                "Somewhat", 
#                                                "Very",
#                                                "Extremely")))

str(data_bytopic)

data_bytopic_wide <- data_bytopic %>% 
  pivot_wider(names_from = c(Topic, Metric),
              values_from = c(Description, percent, Response, n, total))



https://plotly.com/r/horizontal-bar-charts/

# Set the colors for each topical area
colors_health <- c("Extremely" = "#30243c", "Very" = "#5a4a6a", "Somewhat" = "#a088b7", "A little" = "#bfafcf", "Not at all" = "#dfd7e7")
colors_education <- c("Extremely" = "#783f05", "Very" = "#b45f07", "Somewhat" = "#f9b268", "A little" = "#fbcc9a", "Not at all" = "#fde5cd")
colors_ag <- c("Extremely" = "#274221", "Very" = "#4e7345", "Somewhat" = "#8ec182", "A little" = "#b3d6ac", "Not at all" = "#d9ead5")
colors_nr <- c("Extremely" = "#0e2c3e", "Very" = "#2b556d", "Somewhat" = "#4ea5d8", "A little" = "#89c3e5", "Not at all" = "#c4e1f2")
colors_ced <- c("Extremely" = "#c39001", "Very" = "#f5b501", "Somewhat" = "#fecf4c", "A little" = "#fee398", "Not at all" = "#fff6dd")

# Create the stacked bar chart
plot_ly(data_bytopic, x = ~percent, y = ~Description, type = 'bar', orientation = 'h') %>%
  add_trace(x = ~Description, name = "Agriculture", marker = list(color = colors_ag)) %>%
  layout(title = "Horizontal Stacked Barchart", xaxis = list(title = "Percentage"), 
         yaxis = list(title = "Metric"), barmode = 'stack')


