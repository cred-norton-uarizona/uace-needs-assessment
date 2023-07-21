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
  # use_telemetry(), # 2. Add necessary Javascript to Shiny
  # numericInput("n", "n", 1),
  # plotOutput('plot'),
  tags$head(includeHTML("google_tag_head.html")),
  tags$body(includeHTML("google_tag_body.html")),
  
  # Tab panel 1 - instructions
  tabPanel("Needs Assessment",
           img(src = "CE_logo.png", height = "100px"),
           h1("2022 Needs Assessment Survey", 
              style = "font-weight: bold;"),
           p("Arizona Cooperative Extension conducted a statewide needs assessment survey in Fall 2022 to better understand community needs as a basis for examining and prioritizing Extension activities. Cooperative Extension users, topical experts, and members of the general public from each county were invited to participate via online or paper survey (available in English and Spanish). Participants were shown 99 items across topics relevant to Cooperative Extension and asked to rank how important it is to prioritize each item in their community on a 5-point scale."),
           br(),
           p(HTML("Click this <a href='https://arizona.hosted.panopto.com/Panopto/Pages/Viewer.aspx?id=4176d6af-67c3-412a-955d-b046011f9fb2&start=0'>link</a> to watch the dashboard video tutorial.")),
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
           p(HTML("Click this <a href='https://extension.arizona.edu/statewide-needs-assessment?_gl=1*1p34zv*_ga*OTkxOTgzOTAzLjE2NzkzNDg3NDg.*_ga_7PV3540XS3*MTY4MjUzMjkxMC4zNC4xLjE2ODI1MzI5MTMuNTcuMC4w' target='_blank'>link</a> to see the other products for this assessment.")),
           p("Prepared by the Community Research, Evaluation and Development (CRED) team and the Communication and Cyber Technologies Data Science Team, University of Arizona"),
           br()
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
                        Low_Income_FPL_185 = list(inputId = "Low_Income_FPL_185", title = "Low-income (<185% FPL)"),
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
                      label = HTML("Topical Experience <br> (education, work, volunteer)"),
                      choices = topical_exp_vec,
                      multiple = TRUE
                    )
                    ,
                    
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
                             fluidRow(plotOutput("top20bar", height = 800) %>% withSpinner(type = 8)),
                             fluidRow(
                               column(width = 8, 
                                      plotlyOutput("race_bar", height = 400)),
                               column(width = 4,
                                      plotlyOutput("gender_donut", height = 200),
                                      plotlyOutput("edu_donut", height = 200))
                             )
                    ),
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
                        div(
                          "This tab presents key demographic characteristics of the respondents to the Needs Assessment Survey. When thinking about the survey results, it's important to think about 'who' is represented in the survey sample. To look more closely at the demographics for a particular region, use the dropdown filter to select one or more counties.",
                          style = "font-size: 14px; margin-bottom: 10px;"
                        ),
                        selectizeGroupUI(
                          id = "county-filter",
                          params = list(
                            COUNTY = list(inputId = "COUNTY", title = "Select County")),
                          inline = FALSE
                        ), 
                        width = NULL),
                      br(),
                      box(
                        plotOutput("az_map", height = 200),
                        width = NULL),
                      br(),
                      box(
                        plotlyOutput("county_bar", height = 350),
                        width = NULL),
                      br(),
                      box(
                        plotlyOutput("income_bar",height = 350),
                        width = NULL
                      )
      ),
      column(width = 3,
             box(plotlyOutput("rural_donut", height = 210),
                 width = NULL),
             box(plotlyOutput("language_donut", height = 210),
                 width =  NULL),
             box(plotlyOutput("gender_donut_county", height = 210),
                 width =  NULL),
             box(plotlyOutput("ce_user_donut", height = 210),
                 width =  NULL),
             box(plotlyOutput("ce_exposed_donut", height = 210),
                 width =  NULL),
      ),
      column(width = 5,
             box(plotlyOutput("race_bar2", height = 250),
                 width = NULL),
             box(plotlyOutput("age_bar", height = 200),
                 width = NULL),
             box(plotlyOutput("edu_bar", height = 250),
                 width = NULL),
             box(plotlyOutput("info_bar", height = 350),
                 width = NULL)
      )
      )
    ))
)






