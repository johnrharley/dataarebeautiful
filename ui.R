# this is the user interface for the alaska covid-19 shiny app
ui <- fluidPage(
  theme=shinytheme("flatly"),
  titlePanel("Novel coronavirus in Alaska"), updated_date,
  navlistPanel(
    "Visualizations",
    tabPanel("Map of Cases", leafletOutput('covid_map')),
    "Cases",
    tabPanel("Number of cases (linear)", plotlyOutput('casesPlot')),
    tabPanel("Number of cases (logarithmic)", plotlyOutput('casesPlotLog')),
    "Testing",
    tabPanel("Number of tests", plotlyOutput('testingPlot'))),
  "From the DHHS website: updates made daily by 5pm and reflect cases reported as of 3pm that day",
  br(),
  uiOutput("tab"),
  br(),
  "For technical questions about the visualization or to report an error, please email: john.r.harley@gmail.com",
  br(),
  "For all other questions", strong("talk to a health expert."),
  br(),
  "See the code on", uiOutput("git"),
  p(),
)
