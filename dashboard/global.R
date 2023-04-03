library(shiny)
library(mapproj)
library(maps)
library(tidyverse)
library(pins)
library(arrow)
library(shinydashboard)
library(shinyWidgets)
library(plotly)
library(ggforce)


board <- board_connect()
data <- pin_read(board, "terrace/uace-na")

# read in data

labels <- read.csv("labels.csv")

# funtion to break strings for ggplot
break_string <- function(x, n) {
  # x is a character string
  # n is number of characters before the break
  out_string <- stringi::stri_wrap(x, n)
  out_n <- length(out_string)
  
  ifelse(out_n == 1, out_string, 
         paste0(out_string[1], "\n", out_string[2]))
}

