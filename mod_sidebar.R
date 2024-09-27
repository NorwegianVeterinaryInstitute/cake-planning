# Sidebar UI Module
sidebarUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    # useshinyjs() to expand or collapse input list
    useShinyjs(),
    # Add radio buttons
    radioButtons(ns('select'), 'Select Option:',
                 choices = list("Add a new cake entry" = 1, "Modify an entry" = 2, "Delete an entry" = 3),
                 selected = 1),
    dateInput(ns('date'), 'Select Date:', value = Sys.Date()),
    timeInput(ns("hour"), "Time:", value = strptime("12:00:00", "%T"), minute.steps = 10),
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
    textInput(ns('sec_in'), 'Secret Ingredient:'),
    textAreaInput(ns('cake_desc'), 'Cake Description:', rows = 3),
    actionButton(ns('submit'), 'Submit')
  )
}

# Sidebar Server Module
sidebarServer <- function(id, board) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Create a reactive variable: problem
    problem <- reactiveVal()
    # Add default text for this variable
    problem(paste('I hope you are here to make a new cake entry!',
              'To modify an entry, click \'Modify an entry\'. Use the same \'Date\', \'Name\' and \'Secret Ingredient\' as the original entry.',
              'To delete an entry, click \'Delete an ebtry\'. Use the same \'Date\', \'Name\' and \'Secret Ingredient\' as the original entry.',
              sep="\n"))
    
    # Observe the radio button (select) to expand or collapse input list
    observeEvent(input$select, {
      if(input$select == 3) {
        shinyjs::hide("hour")
        shinyjs::hide("room")
        shinyjs::hide("section")
        shinyjs::hide("cake_desc")
      } else {
        shinyjs::show("hour")
        shinyjs::show("room")
        shinyjs::show("section")
        shinyjs::show("cake_desc")
      }
    })
    
    input_data <- reactive({
      data.frame(
        `Date` = input$date,
        `Hour` = (strftime(input$hour, "%R")),
        `Room` = input$room,
        `Section` = input$section,
        `Person.Name` = input$person,
        `Secret.Ingredient` = input$sec_in,
        `Cake.Description` = input$cake_desc,
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
      pinned_cakes <- pin_read(board,
                               name = paste0(Sys.getenv("USER_NAME"), '/cake_user_inputs')
                               )
      
      # See if the entry already exists
      pinned_cakes_exists <- pinned_cakes |>
        dplyr::filter(Secret.Ingredient == input$sec_in & Person.Name == input$person & Date == input$date)
      
      # If the user chooses to modify
      if(input$select == 2) {
        # If the entry is not found, do not change anything
        if (nrow(pinned_cakes_exists) == 0) {
          # Update the text in reactive variable: problem
          problem(paste('We cannot find your entry!', 
                    'Did you forget your secret ingredient?',
                    'Remember, inputs are case-sensitive', 
                    sep="\n"))
          updated_cakes <- pinned_cakes
        } else { # If entry is found, update the table
          # Update the text in reactive variable: problem
          problem(paste('Entry modified', 
                    'Cannot see your modification? Just refrest the page!',
                    'Thank you for being so acquainted!',
                    sep="\n"))
          # Remove previous entry
          pinned_cakes <- pinned_cakes |>
            dplyr::filter(Secret.Ingredient != input$sec_in | Person.Name != input$person | Date != input$date)
          # Replace with new entry
          updated_cakes <- rbind(pinned_cakes, input_data())
        }
      } else if (input$select == 3) { # If the user chooses to delete
        if (nrow(pinned_cakes_exists) == 0) { # If entry does not exists, do nothing
          # Update the text in reactive variable: problem
          problem(paste('We cannot find your entry!', 
                    'Did you forget your secret ingredient?',
                    'Remember, inputs are case-sensitive',
                    sep="\n"))
          updated_cakes <- pinned_cakes
        } else { # If entry exists, remove the entry
          # Update the text in reactive variable: problem
          problem(paste('Entry deleted! Sad! Bet the cake was awesome!', 
                    'Cannot see your modification? Just refrest the page!',
                    'Please inform your colleagues that you are not bringing cake. Bet they will be disappointed.',
                    sep="\n"))
          pinned_cakes <- pinned_cakes |>
            dplyr::filter(Secret.Ingredient != input$sec_in | Person.Name != input$person | Date != input$date)
          updated_cakes <- pinned_cakes
        }
      } else { # If the user chooses to add an entry
        if (nrow(pinned_cakes_exists) != 0) { # If entry already exists, do not do anything
          # Update the text in reactive variable: problem
          problem(paste('Entry already exists!', 
                    'If you are trying to modify an entry, select \'Modify\'',
                    'You are a G.O.A.T if you plan on bringing two cakes though!',
                    sep="\n"))
          updated_cakes <- pinned_cakes
        } else { # If entry does not exist, add the entry
          # Update the text in reactive variable: problem
          problem(paste('Thank you for cake!', 
                    'I bet everyone can\'t wait to Nom Nom Nom Nom Nom!!!!',
                    'Cannot see your addition? Just refrest the page!',
                    sep="\n"))
          updated_cakes <- rbind(pinned_cakes, input_data())
        }
      }
      
      # Save the input data frame to the pin board
      pin_write(
        board,
        updated_cakes,
        name = paste0(Sys.getenv("USER_NAME"), '/cake_user_inputs'),
        description = 'Cake app user input data'
      )
      
      # Clear the inputs after submission
      updateRadioButtons(session, "select", selected = 1)
      updateDateInput(session, "date", value = Sys.Date())
      updateTimeInput(session, "hour", value = strptime("12:00:00", "%T"))
      updateSelectInput(session, "room", selected = NULL)
      updateSelectInput(session, "section", selected = NULL)
      updateTextInput(session, "person", value = "")
      updateTextInput(session, "sec_in", value = "")
      updateTextAreaInput(session, "cake_desc", value = "")
    })
    
    return(list(input_data, problem))  # Return the input data and reactive variable, problem
  })
}
