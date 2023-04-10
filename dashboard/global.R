library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(plotly)
library(Cairo)
options(shiny.usecairo = TRUE)

# read in data needed for both ui and server
labels <- read.csv("labels.csv")

race_vec <- c("American Indian or Alaska Native" = "AIAN",
              "Asian" = "AS",
              "Black or African American" = "BL", 
              "Hispanic or Latino" =  "HL",
              "Multiracial" = "MR", 
              "Native Hawaiian or Other Pacific Islander" = "NHPI", 
              "White" = "WH" , 
              "Prefer not to answer" = "NR") 
