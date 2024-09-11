library(shiny)
library(DT)

# Main Panel UI Module
mainPanelUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    DTOutput(ns('dataTable'))
  )
}

# Main Panel Server Module
mainPanelServer <- function(id, board) {
  moduleServer(id, function(input, output, session) {
    
    output$dataTable <- renderDT({
      # Read the pinned data frame from the pin board
      pinned_cakes <- pin_reactive_read(board, name = paste0(Sys.getenv("user_name"),'/cake_user_inputs'), interval = 1000)
      datatable(pinned_cakes(), options = list(
        columnDefs = list(list(
          targets = '_all',
          className = 'dt-center'
        ))
      ))
    })
  })
}
