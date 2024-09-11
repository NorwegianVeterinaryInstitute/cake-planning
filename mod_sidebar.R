library(shiny)

# Sidebar UI Module
sidebarUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    dateInput(ns('date'), 'Select Date:', value = Sys.Date()),
    selectInput(ns('hour'), 'Select Hour:', choices = 9:15),
    selectInput(ns('room'), 'Select Room:', choices = c("Akutten",
                 "Aksjonen", "Klekkeriet", "Utsikten", "Adamstua",
                 "Prionet", "BSL3", "Skalpellen", "Rapporten",
                 "Rugeriet", "Hensikten", "Sekvensen", "Malm")),
    selectInput(ns('section'), 'Select Section:', choices = paste('Section', 102:141)),
    textInput(ns('person'), 'Person Name:'),
    textAreaInput(ns('cake_desc'), 'Cake Description:', rows = 3),
    actionButton(ns('submit'), 'Submit')
  )
}

# Sidebar Server Module
sidebarServer <- function(id, board) {
  moduleServer(id, function(input, output, session) {
    
    inputData <- reactive({
      data.frame(
        'Date' = input$date,
        'Hour' = input$hour,
        'Room' = input$room,
        'Section' = input$section,
        'Person Name' = input$person,
        'Cake Description' = input$cake_desc,
        stringsAsFactors = FALSE
      )
    })
    
    observeEvent(input$submit, {
      req(input$date, input$hour, input$room, input$section, input$person, input$cake_desc)
      # Save the input data frame to the pin board
      pin_write(board, inputData(), name = paste0(Sys.getenv("user_name"),'/cake_user_inputs'), description = 'User input data')
    })
    
    return(inputData)
  })
}
