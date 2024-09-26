# Main Panel UI Module
mainPanelUI <- function(id) {
  ns <- NS(id)
  
  tagList(DTOutput(ns('dataTable'))) # This is going to be a DT output
}

# Main Panel Server Module
mainPanelServer <- function(id, board) {
  moduleServer(id, function(input, output, session) {
    # Render a datatable
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

# server function for upcoming events
mainPanelServer_up <- function(id, board) {
  moduleServer(id, function(input, output, session) {
    # Render a data table
    output$dataTable <- renderDT({
      # Read the pinned data frame from the pin board
      pinned_cakes <-
        pin_reactive_read(
          board,
          name = paste0(Sys.getenv("USER_NAME"), '/cake_user_inputs'),
          interval = 1000
        )
      # Remove the secret ingredient column
      pinned_cakes <- pinned_cakes()[,c(1:5,7)]
      
      # Filter entries after "today" date
      pinned_cakes_up <- pinned_cakes |>
        dplyr::filter(Date > Sys.Date())
      
      datatable(
        pinned_cakes_up,   
        rownames = FALSE,   # Do not display row names
        colnames = c(
          "Date",
          "Hour",
          "Room",
          "Section",
          "Person Name",
          "Cake Description"
        ),
        filter = "top",     # Add a column filter
        options = list(
          order = list(list(0, 'asc'), list(1, 'asc')),    # Orders the table by ascending order of date and then time
          columnDefs = list(
            list(targets = '_all', className = 'dt-center')
          ))
      )
    })
  })
}

# Main Panel Server Module for today's events
mainPanelServer_today <- function(id, board) {
  moduleServer(id, function(input, output, session) {
    # Render a data table
    output$dataTable <- renderDT({
      # Read the pinned data frame from the pin board
      pinned_cakes <-
        pin_reactive_read(
          board,
          name = paste0(Sys.getenv("USER_NAME"), '/cake_user_inputs'),
          interval = 1000
        )
      # Remove the secret ingredient column
      pinned_cakes <- pinned_cakes()[,c(1:5,7)]
      
      # Filter entries on "today" date
      pinned_cakes_today <- pinned_cakes |>
        dplyr::filter(Date == Sys.Date())
      datatable(
        pinned_cakes_today,   
        rownames = FALSE,   # Do not display row names
        colnames = c(
          "Date",
          "Hour",
          "Room",
          "Section",
          "Person Name",
          "Cake Description"
        ),
        filter = "top",     # Add a column filter
        options = list(
          order = list(list(0, 'asc'), list(1, 'asc')),    # Orders the table by ascending order of date and then time
          columnDefs = list(
            list(targets = '_all', className = 'dt-center')
          ))
      )
    })
  })
}