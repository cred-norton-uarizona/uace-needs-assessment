library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(shinycssloaders) #for loading indicator
library(plotly)

# read in data needed for both ui and server
labels <- read.csv("labels.csv")
