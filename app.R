library(shiny)
library(bslib)
library(pins)
library(DT)
library(dplyr)
library(shinyjs)
library(shinyTime)

# Source the modules
source('mod_sidebar.R')
source('mod_mainPanel.R')

# Create a pin board
board <- board_connect()

# Define UI
ui <- fluidPage(
  theme = bs_theme(bootswatch = 'sketchy'),
  
  # Header
  tags$head(tags$style(
    HTML(
      '
      .header {
        text-align: center;
        padding: 20px;
        background-color: #f8f9fa;
        border-bottom: 2px solid #dee2e6;
        gap: 3em;
        display: flex;
        align-items: center;
      }
      .header img {
        max-height: 150px;
        margin-bottom: 10px;
      }
    '
    )
  )),
  
  div(class = 'header', div(img(
    src = 'logo.webp', alt = 'Logo'
  )), div(
    h1('VI Cake Planner'), uiOutput('subtitle')
  )),
  
  sidebarLayout(
    sidebarPanel(
      sidebarUI('sidebar1') # Call sidebar module UI
    ), 
    mainPanel(
      # Split the main panel into 3 separate tabs using navset_card_underline()
      navset_card_underline(
        title = "When do you want cake? Are you a time traveller (Click Historic then!)?",
        nav_panel("Today", mainPanelUI('mainpanel2')), # Call main panel module UI for Today tab
        nav_panel("Upcoming", mainPanelUI('mainpanel3')), # Call main panel module UI for Upcoming tab
        nav_panel("Historic (All)", mainPanelUI('mainpanel1')), # Call main panel module UI for Historic(All) tab
      )
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  # Generate a random subtitle
  subtitles <- c(
    'Don\'t miss on all the fun!',
    'Keep your blood sugar on par with your caffeine intake!',
    'Never miss a cake!'
  )
  
  output$subtitle <- renderUI({
    subtitle <- sample(subtitles, 1)
    h3(subtitle)
  })
  
  # Call the modules
  input_data <- sidebarServer('sidebar1', board)  # Call sidebar module server
  mainPanelServer('mainpanel1', board)           # Call main panel module server
  mainPanelServer_today('mainpanel2', board)     # Call main panel module server
  mainPanelServer_up('mainpanel3', board)        # Call main panel module server
}

# Run the application
shinyApp(ui = ui, server = server)
