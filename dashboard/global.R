library(shiny)
library(shinydashboard)
library(mapproj)
library(maps)
library(tidyverse)
library(pins)
library(shinyWidgets)
library(plotly)



board <- board_connect()
data <- pin_read(board, "terrace/uace-na")

# read in data

labels <- read.csv("labels.csv")


