library(shiny)
library(shinyjs)
library(plotly)

options(stringsAsFactors = FALSE)

initiative <- reactiveValues(df = data.frame(
  Character = character(),
  Initiative = double(),
  Turn = character()
))

#### UI ####
ui <- fluidPage(
  # Shinyjs switch
  useShinyjs(),

  fluidRow(
    # Initiative table
    column(6,
           tableOutput('table')),
    # Add character UI
    column(6,
      textInput('charName', 'Name:', ''),
      numericInput('initVal', 'Initiative', NaN),
      actionButton('addChar', 'Add')
    )
  ),

  # Buttons
  fluidRow(column(1,
                  actionButton('nextTurn', 'Next')),
           column(1,
                  actionButton('reset', 'Reset'))),

  # Initiative editing UI
  fluidRow(id = 'initEditor')
)

#### Server ####
server <- function(input, output, session) {
  autoUpdate <- reactiveTimer(intervalMs = 1000)

  observe({
    autoUpdate()
    # Save df
    df <- initiative$df

    # Sort table
    df <- df[sort(df$Initiative, index.return = TRUE, decreasing = TRUE)$ix, ]

    # Loop through characters
    for (char in df$Character) {
      # Check if initiative editor exists
      bool <- paste('length(input$', char, 'Init) == 0', sep = '')
      if (eval(parse(text = bool))) {
        # Create a new editor
        insertUI(selector = '#initEditor',
                 where = 'afterEnd',
                 ui = fluidRow(
                   id = paste(char, 'Editor', sep = ''),
                              column(1,
                                numericInput(paste(char, 'Init', sep = ''),
                                  char, df$Initiative[df$Character == char])),
                              column(1,
                                checkboxInput(paste(char, 'Remove', sep = ''),
                                  'Remove')))
                )
      } else { # Editor already exists
        # Check the remove toggle
        removeFinder <- paste('input$', char, 'Remove', sep = '')
        if (eval(parse(text = removeFinder))) {
          # Uncheck box
          updateCheckboxInput(session, paste(char, 'Remove', sep = ''), value = FALSE)

          # Remove character from table
          df <- df[!df$Character == char, ]

          # Hide editor
          runjs(paste0("$('#", char, "Editor').css('display','none')"))
        }
      }

      # Update initiative based on tracker
      tmp <- eval(parse(text = paste0('input$', char, 'Init')))
      try({df$Initiative[df$Character == char] <- tmp}, silent = TRUE)
    }

    # Reapply the table to the global variable
    initiative$df <<- df
  })

  # Next turn button clicked
  observeEvent(input$nextTurn, {
    if ('<-' %in% initiative$df$Turn) { # Turn marker exists
      # Find current marker and remove
      idx <- which(initiative$df$Turn == '<-')
      initiative$df$Turn[idx] <- '*'
      if (idx == length(initiative$df$Turn)) { # Last initiative
        idx <- 1
      } else { # Not the last in initiative
        idx <- idx + 1
      }
    } else { # Turn marker doesn't exist
      idx <- 1
    }
    # Change who's turn it is
    initiative$df$Turn[idx] <<- '<-'
  })

  # Reset button clicked
  observeEvent(input$reset, {
    # Hide every initiative editor
    for (char in initiative$df$Character) {
      runjs(paste0("$('#", char, "Editor').css('display','none')"))
    }

    # Clear initiative table
    initiative$df <<- data.frame(Character = character(),
                                 Initiative = double(),
                                 Turn = integer())
  })

  # Add character button clicked
  observeEvent(input$addChar, {
    # Check if the fields are filled
    if (input$charName != '' & !is.nan(input$initVal)) {
      initiative$df <<- rbind(initiative$df,
                              data.frame(Character = input$charName,
                                         Initiative = input$initVal,
                                         Turn = '*'))

      # Check if initiative editor is available
      bool <- paste0('length(input$', input$charName, 'Init) == 0')
      if (!eval(parse(text = bool))) {
        # Show initiative editor
        runjs(paste0("$('#", input$charName, "Editor').css('display','')" ))

        # Fix initiative in editor
        updateNumericInput(session,
                           paste(input$charName, 'Init', sep = ''),
                           value = input$initVal)
      }

      # Blank the input UI
      updateTextInput(session, 'charName', value = '')
      updateNumericInput(session, 'initVal', value = NaN)
    }
  })

  # Create initiative table
  output$table <- renderTable({
    df <- initiative$df

    # Sort table before showing
    df[sort(df$Initiative, index.return = TRUE, decreasing = TRUE)$ix, ]
  })
}

#### Show it! ####
shinyApp(ui = ui, server = server)