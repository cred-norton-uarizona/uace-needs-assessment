library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(plotly)

# read in data needed for both ui and server
labels <- read.csv("labels.csv")
