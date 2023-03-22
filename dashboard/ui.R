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
    
topical_exp_vec <- c("Agriculture" = "AG_EXPERIENCE", 
                     "Education & Youth Development" = "ED_EXPERIENCE", 
                     "Health & Well-Being" = "HLTH_EXPERIENCE", 
                     "Natural Resources" =  "NR_EXPERIENCE", 
                     "Community & Economic Development" = "CD_EXPERIENCE")

topical_knw_vec <- c("Agriculture" = "AG_KNOWLEDGE",
                     "Education & Youth Development" = "ED_KNOWLEDGE",
                     "Health & Well-Being" = "HLTH_KNOWLEDGE",
                     "Natural Resources" = "NR_KNOWLEDGE",
                     "Community & Economic Development" = "CD_KNOWLEDGE")

# az_counties <- map_data("county", region = "arizona")|>
#   mutate(COUNTY = str_to_title(subregion))


# Incorporate the code from the app.R script

# data %>% count(AGE)
# Ask RG if this works for her and if these numbers look like what she found


# data %>% count(Low_Income_FPL)



dashboardPage(
  dashboardHeader(title = "University of Arizona Cooperative Extension Needs Assessment"),
  dashboardSidebar(
    selectizeGroupUI(
      id = "my-filters",
      params = list(
        COUNTY = list(inputId = "COUNTY", title = "County"),
        
        LIVE_V3 = list(inputId = "LIVE_V3", title = "Urban or Rural"),
        
        UserLanguage = list(inputId = "UserLanguage", title = "Survey Language"),
        
        Gender = list(inputId = "Gender", title = "Gender"),
        
        AGE = list(inputId = "AGE", title = "Age"),
        
        DEM_11 = list(inputId = "DEM_11", title = "Educational Attainment"),
        
        Low_Income_FPL_185 = list(inputId = "Low_Income_FPL_185", title = "Low-income Status (185% Federal Poverty Level or lower)"),
        
        CE_EXPOSED = list(inputId = "CE_EXPOSED", title = "Familiar with Extension"),
        
        CE_USER = list(inputId = "CE_USER", title = "Extension User")
        
      ),
      inline = FALSE
    ),
    # status = "primary",
    selectInput(
      inputId = "race_ethnicity",
      label = "Race/Ethnicity",
      choices = race_vec,
      multiple = TRUE
    ),
    selectInput(
      inputId = "topical_experience",
      label = "Topical Experience",
      choices = topical_exp_vec,
      multiple = TRUE
    ),
    selectInput(
      inputId = "topical_knowledge",
      label = "Topical Knowledge",
      choices = topical_knw_vec,
      multiple = TRUE
    )
  ),
  dashboardBody(# Boxes need to be put in a row (or column)
    fluidRow(column(width = 8,
                    box(
                      plotOutput("top20bar", height = 600),
                      width = NULL),
                    box(plotlyOutput("race_bar", height = 300),
                        width = NULL)),
             column(
               width = 4,
               box(plotlyOutput("Ngauge", height = 200),
                   width = NULL),
               box(plotlyOutput("gender_donut", height = 200),
                   width = NULL)
               
             )))
  )



# # Application title
# navbarPage(
#   "University of Arizona Cooperative Extension Needs Assessment",
#         p("Arizona Cooperative Extension conducted a statewide needs assessment survey in Fall
#   2022 to better understand community needs and priorities. Cooperative Extension users,
#   topical experts, and members of the general public from each county were invited to
#   participate via online or paper survey. Participants were shown 99 items across topics
#   relevant to Cooperative Extension and asked to rank how important it is to prioritize each
#   item in their community on a 5-point scale."),
#         h6("prepared by the Community Research, Evaluation and Development (CRED) team and the Communication and Cyber Technologies Data Science Team, University of Arizona"),
# 
#   # Tab panel 1 - Top 20 View
#   tabPanel(
#     "Top Priorities",
#     sidebarLayout(
#       sidebarPanel(
#         
#       ),
#       
#       
#       # Show a plot of the generated distribution
#       mainPanel(
#         # This is where you show the output (data, chart, leaflet map, etc.) with commas
#         # Where do we put this code for the Top Priorities and how do we specific grouping by the slicers/filters selected dynamically?
#         
#     )
#   )
# )
# )
  

