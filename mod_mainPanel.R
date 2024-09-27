# Main Panel UI Module
mainPanelUI <- function(id) {
  ns <- NS(id)
  # We want to output a datatable
  tagList(DTOutput(ns('dataTable')))
}

# Main Panel Server Module
# This module will display the Historic table
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
      # Hide secret ingredient
      pinned_cakes <- pinned_cakes()[,c(1:5,7)]
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

# This module will display the upcoming table
mainPanelServer_up <- function(id, board) {
  moduleServer(id, function(input, output, session) {
    output$dataTable <- renderDT({
      # Read the pinned data frame from the pin board
      pinned_cakes <-
        pin_reactive_read(
          board,
          name = paste0(Sys.getenv("USER_NAME"), '/cake_user_inputs'),
          interval = 1000
        )
      # Hide secret ingredient
      pinned_cakes <- pinned_cakes()[,c(1:5,7)]
      # Filter all the entries that are occurring after today
      pinned_cakes_up <- pinned_cakes |>
        dplyr::filter(Date > Sys.Date())
      datatable(
        pinned_cakes_up,
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

# This module will display the today table
mainPanelServer_today <- function(id, board) {
  moduleServer(id, function(input, output, session) {
    output$dataTable <- renderDT({
      # Read the pinned data frame from the pin board
      pinned_cakes <-
        pin_reactive_read(
          board,
          name = paste0(Sys.getenv("USER_NAME"), '/cake_user_inputs'),
          interval = 1000
        )
      # Hide secret ingredient
      pinned_cakes <- pinned_cakes()[,c(1:5,7)]
      # Filter all the entries that are occurring today
      pinned_cakes_today <- pinned_cakes |>
        dplyr::filter(Date == Sys.Date())
      datatable(
        pinned_cakes_today,
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