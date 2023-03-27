library(shiny)
library(mapproj)
library(maps)
library(tidyverse)
library(pins)
library(shinydashboard)
library(shinyWidgets)
library(plotly)
library(ggforce)


board <- board_connect()
data <- pin_read(board, "terrace/uace-na")

# read in data

labels <- read.csv("labels.csv")


