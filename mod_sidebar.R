library(shiny)

# Sidebar UI Module
sidebarUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    dateInput(ns('date'), 'Select Date:', value = Sys.Date()),
    selectInput(ns('hour'), 'Select Hour:', choices = 9:15),
    selectInput(
      ns('room'),
      'Select Room:',
      choices = c(
        "Adamstuen",
        "Aksjonen",
        "Akutten",
        "Cellen",
        "Inkubatoren",
        "Hensikten",
        "Klekkeriet",
        "Malm",
        "Prionet",
        "Rapporten",
        "Resepten",
        "Rugeriet",
        "Sekvensen",
        "Skalpellen",
        "Utsikten"
      )
    ) |>
      tooltip("Remember to book the room!"),
    selectInput(ns('section'), 'Select Section:', choices = c(
      "Analyser og diagnostikk",               
      "Bakteriologi",                          
      "Byggteknikk og eiendom",                
      "Dyrehelse, dyrevelferd og mattrygghet",
      "Epidemiologi",                          
      "Fiskediagnostikk",                      
      "Fiskehelse og fiskevelferd",            
      "Forskning akvatisk biosikkerhet",       
      "Forskning fiskehelse",                  
      "Forskning kjemi og toksinologi",        
      "Forskning mattrygghet og dyrehelse",    
      "Forskning og internasjonalisering",     
      "Havbruk, villfisk og velferd",          
      "Høyrisikoagens og patologi",            
      "HR og organisasjon",                    
      "Husdyr, vilt og velferd",               
      "Infrastruktur og digitalisering",       
      "Kommunikasjon og samfunnskontakt",      
      "Medieproduksjon og logistikk",          
      "Mikrobiologi",                          
      "Miljø og smittetiltak",                 
      "Molekylærbiologi",                      
      "Økonomi, regnskap og lønn",             
      "Virologi, immunologi og parasittologi" 
    )),
    textInput(ns('person'), 'Person Name:'),
    textAreaInput(ns('cake_desc'), 'Cake Description:', rows = 3),
    actionButton(ns('submit'), 'Submit')
  )
}

# Sidebar Server Module
sidebarServer <- function(id, board) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    input_data <- reactive({
      data.frame(
        `Date` = input$date,
        `Hour` = input$hour,
        `Room` = input$room,
        `Section` = input$section,
        `Person Name` = input$person,
        `Cake Description` = input$cake_desc,
        stringsAsFactors = FALSE
      )
    })
    
    observeEvent(input$submit, {
      req(
        input$date,
        input$hour,
        input$room,
        input$section,
        input$person,
        input$cake_desc
      )
      pinned_cakes <- pin_read(board, name = paste0('cake_user_inputs'))
      
      updated_cakes <- rbind(pinned_cakes, input_data())
      
      # Save the input data frame to the pin board
      pin_write(
        board,
        updated_cakes,
        name = paste0(Sys.getenv("USER_NAME"), '/cake_user_inputs'),
        description = 'Cake app user input data'
      )
      
      # Clear the inputs after submission
      updateDateInput(session, "date", value = Sys.Date())
      updateSelectInput(session, "hour", selected = NULL)
      updateSelectInput(session, "room", selected = NULL)
      updateSelectInput(session, "section", selected = NULL)
      updateTextInput(session, "person", value = "")
      updateTextAreaInput(session, "cake_desc", value = "")
    })
    
    return(input_data)
  })
}
