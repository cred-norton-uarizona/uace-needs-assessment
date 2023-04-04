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
      sidebarPanel( width = 3,
        selectizeGroupUI(
          id = "my-filters",
          params = list(
            COUNTY = list(inputId = "COUNTY", title = "County"),
            LIVE_V3 = list(inputId = "LIVE_V3", title = "Urban or Rural"),
            UserLanguage = list(inputId = "UserLanguage", title = "Survey Language"),
            DEM_11 = list(inputId = "DEM_11", title = "Educational Attainment"),
            Low_Income_FPL_185 = list(inputId = "Low_Income_FPL_185", title = "Low-income Status"),
            Gender = list(inputId = "Gender", title = "Gender"),
            AGE = list(inputId = "AGE", title = "Age")),
          inline = FALSE
        ), 
        selectInput(inputId = "race_ethnicity",
                    label = "Race/Ethnicity",
                    choices = race_vec,
                    multiple = TRUE),
        
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
      
      # Show a plot of the generated distribution
      mainPanel(
        tabsetPanel(type = "tabs",
                    tabPanel("Overall",
                             fluidRow(column(width = 8,
                                             box(
                                               plotOutput("top20bar", height = 800),
                                               width = NULL),
                             ),
                             column(width = 4,
                                    box(
                                      plotOutput("n_indicator", height = 200), 
                                      width = NULL
                                    ),
                                    box(
                                      plotlyOutput("gender_donut", height = 225),
                                      width = NULL),
                                    box(
                                      plotlyOutput("race_donut", height = 225),
                                        width = NULL),
                                    box(
                                      plotlyOutput("bach_donut", height = 225),
                                      width = NULL))
                             
                             )),
                    tabPanel("By Topic",
                             fluidRow(box(selectInput(
                               inputId = "topic",
                               label = "Select Topic",
                               choices = unique(labels$Topic),
                               multiple = FALSE
                             ), width = 6)),
                             fluidRow(
                               plotOutput("bytopicbar", height = 800,
                                          width = NULL)
                             )))
                    )
        ))
    )
  


  

