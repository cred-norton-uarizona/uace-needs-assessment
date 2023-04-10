library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(plotly)
library(Cairo)
options(shiny.usecairo = TRUE)

# read in data needed for both ui and server
labels <- read.csv("labels.csv")
