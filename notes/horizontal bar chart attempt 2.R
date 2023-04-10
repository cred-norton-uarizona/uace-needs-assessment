# Ranked by topic
# Load the data and libraries  -----------------------------------------------------------
library(tidyverse)
library(pins)
library(plotly)
library(readxl)
library(tidyr)
library(dplyr)
library(plotly)

# Set the colors for each topical area
colors_health <- c("Extremely" = "#30243c", "Very" = "#5a4a6a", "Somewhat" = "#a088b7", "A little" = "#bfafcf", "Not at all" = "#dfd7e7")
colors_education <- c("Extremely" = "#783f05", "Very" = "#b45f07", "Somewhat" = "#f9b268", "A little" = "#fbcc9a", "Not at all" = "#fde5cd")
colors_ag <- c("Extremely" = "#274221", "Very" = "#4e7345", "Somewhat" = "#8ec182", "A little" = "#b3d6ac", "Not at all" = "#d9ead5")
colors_nr <- c("Extremely" = "#0e2c3e", "Very" = "#2b556d", "Somewhat" = "#4ea5d8", "A little" = "#89c3e5", "Not at all" = "#c4e1f2")
colors_ced <- c("Extremely" = "#c39001", "Very" = "#f5b501", "Somewhat" = "#fecf4c", "A little" = "#fee398", "Not at all" = "#fff6dd")


board <- board_connect()
data <- pin_read(board, "terrace/uace-na")
labels <- read.csv("C:/Users/Terrace Ewinghill/Desktop/SCENA Incubator Personal Not Shared/uace-needs-assessment/data/labels.csv")
# labels <- read.csv(here::here("data", "labels.csv"))
# labels <- read_excel("C:/Users/Terrace Ewinghill/Box/Cooperative Extension Needs Assessment 2022/CRED - Incubator Collaboration/Labels for the content areas in survey.xlsx")

cnames <- labels$Metric[labels$Topic == "Agriculture"] # substitute with input$topic

top_labels <- c('Extremely', "Very", "Somewhat", "A<br>little", "Not<br>at<br>all")

fig <- plot_ly(data, x = Response, y = Description, type = 'bar', orientation = 'h',
               marker = list(color = colors_ag), width = 1)
fig <- fig %>% add_trace(x = ~x2, marker = list(color = 'rgba(71, 58, 131, 0.8)')) 
fig <- fig %>% add_trace(x = ~x3, marker = list(color = 'rgba(122, 120, 168, 0.8)')) 
fig <- fig %>% add_trace(x = ~x4, marker = list(color = 'rgba(164, 163, 204, 0.85)')) 
fig <- fig %>% add_trace(x = ~x5, marker = list(color = 'rgba(190, 192, 213, 1)')) 
fig <- fig %>% layout(xaxis = list(title = "",
                                   showgrid = FALSE,
                                   showline = FALSE,
                                   showticklabels = FALSE,
                                   zeroline = FALSE,
                                   domain = c(0.15, 1)),
                      yaxis = list(title = "",
                                   showgrid = FALSE,
                                   showline = FALSE,
                                   showticklabels = FALSE,
                                   zeroline = FALSE),
                      barmode = 'stack',
                      paper_bgcolor = 'rgb(248, 248, 255)', plot_bgcolor = 'rgb(248, 248, 255)',
                      margin = list(l = 120, r = 10, t = 140, b = 80),
                      showlegend = FALSE) 
# labeling the y-axis
fig <- fig %>% add_annotations(xref = 'paper', yref = 'y', x = 0.14, y = y,
                               xanchor = 'right',
                               text = y,
                               font = list(family = 'Arial', size = 12,
                                           color = 'rgb(67, 67, 67)'),
                               showarrow = FALSE, align = 'right') 
# labeling the percentages of each bar (x_axis)
fig <- fig %>% add_annotations(xref = 'x', yref = 'y',
                               x = x1 / 2, y = y,
                               text = paste(data[,"x1"], '%'),
                               font = list(family = 'Arial', size = 12,
                                           color = 'rgb(248, 248, 255)'),
                               showarrow = FALSE) 
fig <- fig %>% add_annotations(xref = 'x', yref = 'y',
                               x = x1 + x2 / 2, y = y,
                               text = paste(data[,"x2"], '%'),
                               font = list(family = 'Arial', size = 12,
                                           color = 'rgb(248, 248, 255)'),
                               showarrow = FALSE) 
fig <- fig %>% add_annotations(xref = 'x', yref = 'y',
                               x = x1 + x2 + x3 / 2, y = y,
                               text = paste(data[,"x3"], '%'),
                               font = list(family = 'Arial', size = 12,
                                           color = 'rgb(248, 248, 255)'),
                               showarrow = FALSE) 
fig <- fig %>% add_annotations(xref = 'x', yref = 'y',
                               x = x1 + x2 + x3 + x4 / 2, y = y,
                               text = paste(data[,"x4"], '%'),
                               font = list(family = 'Arial', size = 12,
                                           color = 'rgb(248, 248, 255)'),
                               showarrow = FALSE) 
fig <- fig %>% add_annotations(xref = 'x', yref = 'y',
                               x = x1 + x2 + x3 + x4 + x5 / 2, y = y,
                               text = paste(data[,"x5"], '%'),
                               font = list(family = 'Arial', size = 12,
                                           color = 'rgb(248, 248, 255)'),
                               showarrow = FALSE) 
# labeling the first Likert scale (on the top)
fig <- fig %>% add_annotations(xref = 'x', yref = 'paper',
                               x = c(21 / 2, 21 + 30 / 2, 21 + 30 + 21 / 2, 21 + 30 + 21 + 16 / 2,
                                     21 + 30 + 21 + 16 + 12 / 2),
                               y = 1.15,
                               text = top_labels,
                               font = list(family = 'Arial', size = 12,
                                           color = 'rgb(67, 67, 67)'),
                               showarrow = FALSE)