library(shiny)
library(plotly)

if (file.exists('initiative.Rda')) {
  load('initiative.Rda')
} else {
  initiative <- reactiveValues(df = data.frame(Character = character(),
                                               Initiative = double(),
                                               Order = integer()))
}

onStop(function() {
  save(initiative, file = 'initiative.Rda')
})

#### UI ####
ui <- fluidPage(
  fluidRow(
    column(6,
           tableOutput('table')),
    column(6,
           textInput('charName', 'Name:', ''),
           numericInput('initVal', 'Initiative', NaN),
           actionButton('addChar', 'Add'))
  ),

  fluidRow(
    column(1,
           actionButton('next', 'Next')),
    column(1,
           actionButton('reset', 'Reset'))
  ),

  fluidRow(id = 'initEditor')
)

#### Server ####
server <- function(input, output, session) {
  autoUpdate <- reactiveTimer(intervalMs = 1000)

  observe({
    autoUpdate()

    initiative$df <<- initiative$df

    for (char in initiative$df$Character) {
      bool <- paste('length(input$', char, 'Init) == 0', sep = '')
      if (eval(parse(text = bool))) {
        insertUI(selector = '#initEditor',
                 where = 'afterEnd',
                 ui = fluidRow(id = paste(char, 'Editor', sep = ''),
                               column(1, numericInput(paste(char, 'Init', sep = ''), char,
                                            initiative$df$Initiative[initiative$df$Character == char])),
                               column(1, checkboxInput(paste(char, 'Remove', sep = ''), 'Remove'))))
      } else {
        removeFinder <- paste('input$', char, 'Remove', sep = '')
        if (eval(parse(text = removeFinder))) {
          initiative$df <<- initiative$df[initiative$df$char == char,]

          removeUI(selector = paste('#', char, 'Editor', sep = ''))
        } else {
          initFinder <- paste('input$', char, 'Init', sep = '')
          val = eval(parse(text = initFinder))
          initiative$df$Initiative[initiative$df$Character == char] <<- val
        }
      }
    }
  })

  observeEvent(input$reset, {
    initiative$df <<- data.frame(Character = character(),
                                 Initiative = double(),
                                 Order = integer())
  })

  observeEvent(input$addChar, {
    if (input$charName != '' & !is.nan(input$initVal)) {
      initiative$df <<- rbind(initiative$df,
                              data.frame(Character = input$charName,
                                         Initiative = input$initVal,
                                         Order = -1))
      updateTextInput(session, 'charName', value = '')
      updateNumericInput(session, 'initVal', value = NaN)
    }
  })

  output$table <- renderTable({
    df <- initiative$df

    df[sort(df$Initiative, index.return = TRUE, decreasing = TRUE)$ix,]
  })
}

#### Show it! ####
shinyApp(ui = ui, server = server)