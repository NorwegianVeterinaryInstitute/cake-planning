# Main Panel UI Module
mainPanelUI <- function(id) {
  ns <- NS(id)
  
  tagList(DTOutput(ns('dataTable')))
}

# Main Panel Server Module
mainPanelServer <- function(id, board) {
  moduleServer(id, function(input, output, session) {
    output$dataTable <- renderDT({
      # Read the pinned data frame from the pin board
      pinned_cakes <-
        pin_reactive_read(
          board,
          name = paste0(Sys.getenv("USER_NAME"), '/cake_user_inputs'),
          interval = 1000
        )
      pinned_cakes <- pinned_cakes()[,c(1:5,7)]
      datatable(
        pinned_cakes,
        rownames = FALSE,
        colnames = c(
          "Date",
          "Hour",
          "Room",
          "Section",
          "Person Name",
          "Cake Description"
        ),
        options = list(
          columnDefs = list(
          list(targets = '_all', className = 'dt-center')
        ))
      )
    })
  })
}
