#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


race_vec <- c("American Indian or Alaska Native" = "AIAN",
              "Asian" = "AS",
              "Black or African American" = "BL", 
              "Hispanic or Latinx" =  "HL",
              "Multiracial" = "MR", 
              "Native Hawaiian or Pacific Islander" = "NHPI", 
              "White" = "WH" , 
              "Prefer not to answer" = "NR") 
    
expert_vec <- c("Health & Well-Being" = "HLTH_EXPERT",
              "Education & Youth Development" = "ED_EXPERT", 
              "Agriculture" = "AG_EXPERT", 
              "Natural Resources" =  "NR_EXPERT",
              "Community & Economic Development" = "CD_EXPERT") 



# az_counties <- map_data("county", region = "arizona")|>
#   mutate(COUNTY = str_to_title(subregion))


# Incorporate the code from the app.R script

# data %>% count(AGE)
# Ask RG if this works for her and if these numbers look like what she found


# data %>% count(Low_Income_FPL)


# Application title
navbarPage(
  title = "University of Arizona Cooperative Extension Needs Assessment",
  header =  mainPanel(
    p("Arizona Cooperative Extension conducted a statewide needs assessment survey in Fall
  2022 to better understand community needs and priorities. Cooperative Extension users,
  topical experts, and members of the general public from each county were invited to
  participate via online or paper survey. Participants were shown 99 items across topics
  relevant to Cooperative Extension and asked to rank how important it is to prioritize each
  item in their community on a 5-point scale."),
  h6("prepared by the Community Research, Evaluation and Development (CRED) team and the Communication and Cyber Technologies Data Science Team, University of Arizona")
  ),
  
  # Tab panel 1 - Top 20 View
  tabPanel(
    "Top Priorities",
    sidebarLayout(
      sidebarPanel(
        selectizeGroupUI(
          id = "my-filters",
          params = list(
            COUNTY = list(inputId = "COUNTY", title = "County"),
            
            LIVE_V3 = list(inputId = "LIVE_V3", title = "Urban or Rural"),
            
            UserLanguage = list(inputId = "UserLanguage", title = "Survey Language"),
            
            GENDER = list(inputId = "GENDER", title = "Gender"),
            
            AGE = list(inputId = "AGE", title = "Age"),
            
            DEM_11 = list(inputId = "DEM_11", title = "Educational Attainment"),
            
            Low_Income_FPL_100 = list(inputId = "Low_Income_FPL_100", title = "Low-income Status (100% Federal Poverty Level or lower)"),
            
            CE_EXPOSED = list(inputId = "CE_EXPOSED", title = "Familiar with Extension"),
            
            CE_USER = list(inputId = "CE_USER", title = "Extension User")
            
          ),
          inline = FALSE
        ), status = "primary",
        selectInput(inputId = "race_ethnicity",
                    label = "Race/Ethnicity",
                    choices = race_vec,
                    multiple = TRUE),
        
        selectInput(inputId = "topical_expert",
                    label = "Topical Expert",
                    choices = expert_vec,
                    multiple = TRUE)
      ),
      
      
      # Show a plot of the generated distribution
      mainPanel(
        # This is where you show the output (data, chart, leaflet map, etc.) with commas
        # Where do we put this code for the Top Priorities and how do we specific grouping by the slicers/filters selected dynamically?
        DT::dataTableOutput(outputId = "table"),
        textOutput(outputId = "text")
        # p("hi, this is the main panel"),
        # shiny::verbatimTextOutput("table")
        
      )
    )
  )
  
  
  
)
