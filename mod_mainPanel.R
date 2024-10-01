library(shiny)
library(DT)
library(dplyr)

# Main Panel UI Module
mainPanelUI_card <- function(id) {
  ns <- NS(id)
  
  tagList(
    verbatimTextOutput(ns('text'))
  )
}  

mainPanelUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    DTOutput(ns('dataTable'))
  )
}

# Main Panel Server Module
mainPanelServer_card <- function(id, problem) {
  moduleServer(id, function(input, output, session) {
    
    output$text <- renderText({
      paste0(problem())
    })
  })
}


mainPanelServer <- function(id, pinned_cakes, when) {
  moduleServer(id, function(input, output, session) {
    
    output$dataTable <- renderDT({
      # Read the pinned data frame from the pin board
      pinned_cakes <- pinned_cakes()[,c(1:5,7)]
      if (when == "today") {
        pinned_cakes <- pinned_cakes |>
          filter(Date == Sys.Date())
      }
      if (when == "upcoming") {
        pinned_cakes <- pinned_cakes |>
          filter(Date > Sys.Date())
      }
      datatable(pinned_cakes,
                rownames = FALSE,
                colnames = c("Date", "Hour", "Room",
                              "Section", "Person Name",
                              "Cake Description"),
                filter = "top",
        options = list(
          order = list(list(0, 'asc'), list(1, 'asc')),
        columnDefs = list(list(
          targets = '_all',
          className = 'dt-center'
        ))
      ))
    })
  })
}