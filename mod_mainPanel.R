# Main Panel UI card Module
mainPanelUI_card <- function(id) {
  ns <- NS(id)
  # We want the output as a text
  tagList(
    verbatimTextOutput(ns('text'))
  )
} 

# Main Panel UI Module
mainPanelUI <- function(id) {
  ns <- NS(id)
  # We want to output a datatable
  tagList(DTOutput(ns('dataTable')))
}

# Main Panel Server card Module
mainPanelServer_card <- function(id, problem) {
  moduleServer(id, function(input, output, session) {
    # Render the text as output
    output$text <- renderText({
      paste0(problem())
    })
  })
}

# Main Panel Server Module
# This module will display the Historic table
mainPanelServer <- function(id, pinned_cakes, when) {
  moduleServer(id, function(input, output, session) {
    
    output$dataTable <- renderDT({
      # Hide secret ingredient
      pinned_cakes <- pinned_cakes()[,c(1:5,7)]
      if (when == "today") {
        pinned_cakes <- pinned_cakes |>
          filter(Date == Sys.Date())
      }
      if (when == "upcoming") {
        pinned_cakes <- pinned_cakes |>
          filter(Date > Sys.Date())
      }
      datatable(
        pinned_cakes,
        # Do not show row names
        rownames = FALSE,
        # Add filters for each column
        filter = "top",
        colnames = c(
          "Date",
          "Hour",
          "Room",
          "Section",
          "Person Name",
          "Cake Description"
        ),
        options = list(
          # order table by date and then time
          order = list(list(0, 'asc'), list(1, 'asc')),
          columnDefs = list(
            list(targets = '_all', className = 'dt-center')
          ))
      )
    })
  })
}