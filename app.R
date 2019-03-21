library(shiny)
library(plotly)

if (file.exists('initiative.Rda')) {
  load('initiative.Rda')
} else {
  initiative <- reactiveValues(df = data.frame(Character = character(),
                                               Initiative = double(),
                                               Order = integer(),
                                               needUI = integer()))
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
  )
)

#### Server ####
server <- function(input, output, session) {
  autoUpdate <- reactiveTimer(intervalMs = 1000)

  observe({
    autoUpdate()

    initiative$df <<- initiative$df
  })

  observeEvent(input$reset, {
    initiative$df <<- data.frame(Character = character(),
                                 Initiative = double(),
                                 Order = integer(),
                                 needUI = integer())
  })

  observeEvent(input$addChar, {
    if (input$charName != '' & is.nan(input$initVal)) {
      initiative$df <<- rbind(initiative$df,
                              data.frame(Character = input$charName,
                                         Initiative = input$initVal,
                                         Order = -1,
                                         needUI = 1))
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