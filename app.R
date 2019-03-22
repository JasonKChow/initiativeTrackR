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
  useShinyjs(),
  fluidRow(
    column(6,
           tableOutput('table')),
    column(6,
      textInput('charName', 'Name:', ''),
      numericInput('initVal', 'Initiative', NaN),
      actionButton('addChar', 'Add')
    )
  ),

  fluidRow(column(1,
                  actionButton('nextTurn', 'Next')),
           column(1,
                  actionButton('reset', 'Reset'))),

  fluidRow(id = 'initEditor')
)

#### Server ####
server <- function(input, output, session) {
  autoUpdate <- reactiveTimer(intervalMs = 1000)

  observe({
    autoUpdate()

    for (char in initiative$df$Character) {
      bool <- paste('length(input$', char, 'Init) == 0', sep = '')
      removeFinder <- paste('input$', char, 'Remove', sep = '')
      if (eval(parse(text = bool))) {
        insertUI(
          selector = '#initEditor',
          where = 'afterEnd',
          session = session,
          ui = fluidRow(
            id = paste(char, 'Editor', sep = ''),
            column(
              1,
              numericInput(
                paste(char, 'Init', sep = ''),
                char,
                initiative$df$Initiative[initiative$df$Character == char]
              )
            ),
            column(1, checkboxInput(
              paste(char, 'Remove', sep = ''), 'Remove'
            ))
          )
        )
      } else {
        removeFinder <- paste('input$', char, 'Remove', sep = '')
        if (eval(parse(text = removeFinder))) {
          updateCheckboxInput(session, paste(char, 'Remove', sep = ''), value = FALSE)
          #updateNumericInput(session, paste(char, 'Init', sep = ''), value = 1996)
          initiative$df <<-
            initiative$df[!initiative$df$Character == char, ]
          runjs(paste0(
            "$('#",
            char,
            "Editor').css('display','none')"
          ))
        }
      }
      tmp <- eval(parse(text = paste0('input$', char, 'Init')))
      try({
        initiative$df$Initiative[initiative$df$Character == char] <<- tmp
      }, silent = TRUE)
    }
  })

  observeEvent(input$nextTurn, {
    if ('<-' %in% initiative$df$Turn) {
      idx <- which(initiative$df$Turn == '<-')
      initiative$df$Turn[idx] <- '*'
      if (idx == length(initiative$df$Turn)) {
        idx <- 1
      } else {
        idx <- idx + 1
      }
    } else {
      idx <- 1
    }
    initiative$df$Turn[idx] <- '<-'
  })

  observeEvent(input$reset, {
    for (char in initiative$df$Character) {
      runjs(paste0("$('#", char, "Editor').css('display','none')"))
    }

    initiative$df <<- data.frame(
      Character = character(),
      Initiative = double(),
      Turn = integer()
    )
  })

  observeEvent(input$addChar, {
    if (input$charName != '' & !is.nan(input$initVal)) {
      tmp <- rbind(
        initiative$df,
        data.frame(
          Character = input$charName,
          Initiative = input$initVal,
          Turn = '*'
        )
      )
      initiative$df <<- tmp

      bool <-
        paste('length(input$', input$charName, 'Init) == 0', sep = '')
      if (!eval(parse(text = bool))) {
        runjs(paste0(
          "$('#",
          input$charName,
          "Editor').css('display','')"
        ))
        updateNumericInput(session,
                           paste(input$charName, 'Init', sep = ''),
                           value = input$initVal)
      }

      updateTextInput(session, 'charName', value = '')
      updateNumericInput(session, 'initVal', value = NaN)
    }
  })

  output$table <- renderTable({
    df <- initiative$df

    df[sort(df$Initiative,
            index.return = TRUE,
            decreasing = TRUE)$ix, ]
  })
}

#### Show it! ####
shinyApp(ui = ui, server = server)