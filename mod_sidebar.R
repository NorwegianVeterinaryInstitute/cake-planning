library(shiny)
library(dplyr)
library(shinyjs)
library(shinyTime)

# Sidebar UI Module
sidebarUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    useShinyjs(),
    radioButtons(ns('select'), 'Select Option:',
                 choices = list("Add a new cake entry" = 1, "Modify an entry" = 2, "Delete an entry" = 3),
                 selected = 1),
    dateInput(ns('date'), 'Select Date:', value = Sys.Date()),
    timeInput(ns("hour"), "Time:", value = strptime("12:00:00", "%T"), minute.steps = 10),
    selectInput(ns('room'), 'Select Room:', choices = c("Akutten",
                                                        "Aksjonen", "Klekkeriet", "Utsikten", "Adamstua",
                                                        "Prionet", "BSL3", "Skalpellen", "Rapporten",
                                                        "Rugeriet", "Hensikten", "Sekvensen", "Malm")) |>
      tooltip("Remember to book the room!"),
    selectInput(ns('section'), 'Select Section:', choices = 102:141),
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
    problem <- reactiveVal()
    problem("I hope you are here to make a new cake entry!")
    
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
    
    
    inputData <- reactive({
      data.frame(
        `Date` = input$date,
        `Hour` = (strftime(input$hour, "%R")),
        `Room` = input$room,
        `Section` = input$section,
        `Person Name` = input$person,
        `Secret Ingredient` = input$sec_in,
        `Cake Description` = input$cake_desc,
        stringsAsFactors = FALSE
      )
    })
    
    observeEvent(input$submit, {
      req(input$select, input$date, input$hour, input$room, input$section, input$person, input$sec_in)
      
      pinned_cakes <- pin_read(board, 
                               name = paste0('cake_user_inputs'))
      
      pinned_cakes_exists <- pinned_cakes |>
        dplyr::filter(Secret.Ingredient == input$sec_in & Person.Name == input$person & Date == input$date)
      
      if(input$select == 2) {
        if (nrow(pinned_cakes_exists) == 0) {
          problem("We cannot find your entry! Did you forget your secret ingredient?")
          updated_cakes <- pinned_cakes
        } else {
          problem("Entry modified! If you do not see the modification, kindly refresh the page.")
          pinned_cakes <- pinned_cakes |>
            dplyr::filter(Secret.Ingredient != input$sec_in | Person.Name != input$person | Date != input$date)
          updated_cakes <- rbind(pinned_cakes, inputData())
        }
      } else if (input$select == 3) {
        if (nrow(pinned_cakes_exists) == 0) {
          problem("We cannot find your entry! Did you forget your secret ingredient?")
          updated_cakes <- pinned_cakes
        } else {
          problem("Entry deleted! Sad! Bet the cake was awesome! Anyways, if you do not see the deletion, kindly refresh the page.")
          pinned_cakes <- pinned_cakes |>
            dplyr::filter(Secret.Ingredient != input$sec_in | Person.Name != input$person | Date != input$date)
          updated_cakes <- pinned_cakes
        }
      } else {
        if (nrow(pinned_cakes_exists) != 0) {
          problem("Entry already exists! If you are trying to modify an entry, select Modify")
          updated_cakes <- pinned_cakes
        } else {
          problem("Thank you for cake! Can't wait to Nom Nom Nom Nom Nom!!!!")
          updated_cakes <- rbind(pinned_cakes, inputData())
        }
        }
      
      # Save the input data frame to the pin board
      pin_write(board, updated_cakes, name = paste0('cake_user_inputs'), description = 'Cake app user input data')
      
      # Clear the inputs after submission
      updateRadioButtons(session, "select", selected = 1)
      updateDateInput(session, "date", value = Sys.Date())
      updateTimeInput(session, "hour", value = strptime("12:00:00", "%T"))
      updateSelectInput(session, "room", selected = NULL)
      updateSelectInput(session, "section", selected = NULL)
      updateTextInput(session, "sec_in", value = "")
      updateTextInput(session, "person", value = "")
      updateTextAreaInput(session, "cake_desc", value = "")
    })
    
    return(list(inputData, problem))
  })
}
