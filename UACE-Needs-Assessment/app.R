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
data <- read_excel("C:/Users/Terrace Ewinghill/Box/Cooperative Extension Needs Assessment 2022/CRED - Incubator Collaboration/Data without zips.xlsx")


az_counties <- map_data("county", region = "arizona")|>
  mutate(COUNTY = str_to_title(subregion))

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
            choices = unique(az_counties$COUNTY)
          )

        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("county_map"),
           tableOutput("text_out")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$county_map <- renderPlot({
      az_counties <- az_counties |>
        mutate(selected = ifelse(COUNTY == input$select_county, TRUE, FALSE))
      
      ggplot(data = az_counties,
             mapping = aes(x = long, y = lat,
                           group = group, fill = selected)) + 
        geom_polygon(color = "black", show.legend = FALSE) +
        scale_fill_manual(values = c("TRUE" = "darkblue", "FALSE" = "grey")) +
        coord_map() +
        theme_void()

    })
    output$text_out <- renderTable({
      data |> filter(COUNTY == input$select_county) |> head()
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
