# Ranked by topic
# Load the data and libraries  -----------------------------------------------------------
library(tidyverse)
library(pins)
library(plotly)
library(readxl)
library(tidyr)
library(dplyr)
library(plotly)



board <- board_connect()
data <- pin_read(board, "terrace/uace-na")
labels <- read.csv("C:/Users/Terrace Ewinghill/Desktop/SCENA Incubator Personal Not Shared/uace-needs-assessment/data/labels.csv")
# labels <- read.csv(here::here("data", "labels.csv"))
# labels <- read_excel("C:/Users/Terrace Ewinghill/Box/Cooperative Extension Needs Assessment 2022/CRED - Incubator Collaboration/Labels for the content areas in survey.xlsx")

cnames <- labels$Metric[labels$Topic == "Agriculture"] # substitute with input$topic




# Set the colors for each topical area
colors_health <- c("Extremely" = "#30243c", "Very" = "#5a4a6a", "Somewhat" = "#a088b7", "A little" = "#bfafcf", "Not at all" = "#dfd7e7")
colors_education <- c("Extremely" = "#783f05", "Very" = "#b45f07", "Somewhat" = "#f9b268", "A little" = "#fbcc9a", "Not at all" = "#fde5cd")
colors_ag <- c("Extremely" = "#274221", "Very" = "#4e7345", "Somewhat" = "#8ec182", "A little" = "#b3d6ac", "Not at all" = "#d9ead5")
colors_nr <- c("Extremely" = "#0e2c3e", "Very" = "#2b556d", "Somewhat" = "#4ea5d8", "A little" = "#89c3e5", "Not at all" = "#c4e1f2")
colors_ced <- c("Extremely" = "#c39001", "Very" = "#f5b501", "Somewhat" = "#fecf4c", "A little" = "#fee398", "Not at all" = "#fff6dd")

# Get long dataset for plotting
data_bytopic <- data() %>%
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
  mutate(percent = round(n/total*100),
         Response = factor(Response, level = c("Not at all",
                                               "A little",
                                               "Somewhat", 
                                               "Very",
                                               "Extremely")),
         Description = factor(Description, levels = c("Healthcare",
                                                      "Education",
                                                      "Agriculture",
                                                      "Natural Resources",
                                                      "Community and Economic Development"))) %>%
  left_join(labels, by = "Metric")

# Create wide dataset for plotly
data_bytopic_wide <- data_bytopic %>%
  pivot_wider(names_from = "Response",
              values_from = "percent") %>%
  group_by(Description) %>%
  mutate(percent_total = sum(percent)) %>%
  ungroup() %>%
  pivot_longer(cols = c("Not at all", "A little", "Somewhat", "Very", "Extremely"),
               names_to = "Response",
               values_to = "percent") %>%
  mutate(percent = round(percent/percent_total*100)) %>%
  select(-percent_total)

# Create the stacked bar chart
plot_ly(data_bytopic_wide, x = ~percent, y = ~Description, type = 'bar', orientation = 'h',
        color = ~Response, colors = c(colors_health, colors_education, colors_ag, colors_nr, colors_ced)) %>%
  layout(title = "Horizontal Stacked 100% Bar Chart", xaxis = list(title = "Percentage", tickformat = ".0%"),
         yaxis = list(title = "Topic"), barmode = 'stack')



# https://plotly.com/r/horizontal-bar-charts/