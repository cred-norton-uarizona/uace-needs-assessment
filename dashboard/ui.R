#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# Application title
navbarPage(
  title = "University of Arizona Cooperative Extension",
  # header =  mainPanel(
  #   p("Arizona Cooperative Extension conducted a statewide needs assessment survey in Fall
  # 2022 to better understand community needs and priorities. Cooperative Extension users,
  # topical experts, and members of the general public from each county were invited to
  # participate via online or paper survey. Participants were shown 99 items across topics
  # relevant to Cooperative Extension and asked to rank how important it is to prioritize each
  # item in their community on a 5-point scale."),
  # h6("prepared by the Community Research, Evaluation and Development (CRED) team and the Communication and Cyber Technologies Data Science Team, University of Arizona")
  # ),
  
  # Tab panel 1 - instructions
  tabPanel("Needs Assessment",
           img(src = "CE_logo.png", height = "100px"),
           h1("2022 Needs Assessment Survey", 
              style = "font-weight: bold;"),
           p("Arizona Cooperative Extension conducted a statewide needs assessment survey in Fall 2022 to better understand community needs as a basis for examining and prioritizing Extension activities. Cooperative Extension users, topical experts, and members of the general public from each county were invited to participate via online or paper survey (available in English and Spanish). Participants were shown 99 items across topics relevant to Cooperative Extension and asked to rank how important it is to prioritize each item in their community on a 5-point scale."),
           br(),
           p("How to use this dashboard: This dashboards enables users to look at the needs assessment results by different filters including:"),
           tags$ul(
             tags$li("County"),
             tags$li("Urban or rural"),
             tags$li("Survey language"),
             tags$li("Educational attainment"),
             tags$li("Low-income status (185% FPL)"),
             tags$li("Gender"),
             tags$li("Age"),
             tags$li("Race/ethnicity"),
             tags$li("Topical experience"),
             tags$li("Topical knowledge")
           ),
           p("Please note that samples of 6 or fewer will not be shown to protect data privacy. If you receive an error message, remove one or more of your filters to see results."),
           br(),
           p("The demographics page includes information about the respondents across the state and by county. When looking at the results, consider whose perspectives are more (or less) represented, and what else could be done to engage the underrepresented communities. You may want to compare the survey respondents with your county’s overall demographics (see the provided County Secondary Data Profiles)."),
           br(),
           p("Prepared by the Community Research, Evaluation and Development (CRED) team and the Communication and Cyber Technologies Data Science Team, University of Arizona"),
           br(),
           p(HTML("Do we want to <a href='https://extension.arizona.edu/statewide-needs-assessment?_gl=1*1p34zv*_ga*OTkxOTgzOTAzLjE2NzkzNDg3NDg.*_ga_7PV3540XS3*MTY4MjUzMjkxMC4zNC4xLjE2ODI1MzI5MTMuNTcuMC4w' target='_blank'>link</a> to the Extension page where the other products are posted?"))
  ),
  
    # Tab panel 2 - Top 20 View and By Topic View as subtabs
  tabPanel(
    "Top Priorities",
    sidebarLayout(
      sidebarPanel( width = 3,
                    plotOutput("n_indicator", height = 150),
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
                    br(),
                    hr(style = "border-top: 1px solid #000000;"),
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
                                               plotOutput("top20bar", height = 800) %>%
                                                 withSpinner(type = 8), #loading indicator for plot,
                                               width = NULL),
                                             box(
                                               plotlyOutput("race_bar", height = 350),
                                               width = NULL)
                             ),
                             column(width = 4,
                                    box(
                                      plotlyOutput("gender_donut", height = 250),
                                      width = NULL),
                                    box(
                                      plotlyOutput("edu_donut", height = 250),
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
                                          width = NULL)  %>%
                                 withSpinner(type = 8) #loading indicator for plot,
                              
                             )))
                    )
        )),
  
  # Tab panel 3 - Demographics by county
  tabPanel(
    "Demographics",
    fluidPage(
      fluidRow(column(width = 4,
                      box(
                        selectizeGroupUI(
                          id = "county-filter",
                          params = list(
                            COUNTY = list(inputId = "COUNTY", title = "Select County")),
                          inline = FALSE
                        ), 
                        width = NULL),
                      box(
                        plotlyOutput("county_bar", height = 350),
                        width = NULL),
                      box(
                        plotlyOutput("income_bar",height = 250),
                        width = NULL
                      )
      ),
      column(width = 3,
             box(plotlyOutput("rural_donut", height = 200),
                 width = NULL),
             box(plotlyOutput("language_donut", height = 200),
                 width =  NULL),
             box(plotlyOutput("gender_donut_county", height = 200),
                 width =  NULL)
      ),
      column(width = 5,
             box(plotlyOutput("race_bar2", height = 250),
                 width = NULL),
             box(plotlyOutput("age_bar", height = 200),
                 width = NULL),
             box(plotlyOutput("edu_bar", height = 250),
                 width = NULL)
      )
      )
    ))
  )

  


  

