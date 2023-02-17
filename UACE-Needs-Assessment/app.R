#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(readxl)

#read in data
path <- "/Users/ericscott/Library/CloudStorage/Box-Box/CRED - Incubator Collaboration/Data without zips.xlsx"
data <- read_excel(path)

az_counties <- map_data("county", region = "arizona")

# Define UI for application that draws a histogram
ui <- fluidPage(
  
    # Application title
    titlePanel("Old Faithful Geyser Data"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
          shiny::selectInput(
            "select_county",
            label = "Select County",
            choices = unique(az_counties$subregion)
          )

        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("countyMap"),
           textOutput("text_out")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$countyMap <- renderPlot({
      az_counties <- az_counties |>
        mutate(selected = ifelse(subregion == input$select_county, TRUE, FALSE))
      
      ggplot(data = az_counties,
             mapping = aes(x = long, y = lat,
                           group = group, fill = selected)) + 
        geom_polygon(color = "black", show.legend = FALSE) +
        scale_fill_manual(values = c("TRUE" = "darkblue", "FALSE" = "grey")) +
        coord_map() +
        theme_void()

    })
    output$text_out <- renderPrint({
      input$bins
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
