df <- data.frame(
  A = 1:10,
  B = c(rep('id1', 4), rep('id2', 6)),
  C = c(rep(c('test1', 'test2', 'test3', 'test4'), 2), 'test5', 'test6'),
  D = c(rep('caseid1', 7), rep('caseid2', 3))
)

txInput <- "B == 'id2' & D == 'caseid2' & A == 10" #works
txInput <- "B == 'id2' & A == 10 & D == 'caseid2'" #works
txInput <- "B == 'id2'" # works
txInput <- "B == 'id2' & A == 10"
eval(txInput)

filter_(df, txInput)

ui <- shinyUI(pageWithSidebar(
  headerPanel("Click the button"),
  sidebarPanel(
    textInput('filter', 'Filter Data'),
    actionButton("goButton", "Go!")
  ),
  mainPanel(
    DT::dataTableOutput('data')
  )
))
server <- shinyServer(function(input, output) {
  df <- data.frame(
    A = 1:10,
    B = c(rep('id1', 4), rep('id2', 6)),
    C = c(rep(c('test1', 'test2', 'test3', 'test4'), 2), 'test5', 'test6'),
    D = c(rep('caseid1', 7), rep('caseid2', 3))
  )
  output$data <- DT::renderDataTable({
    input$goButton
    dat <- isolate(
      if (input$filter == '') {
        df
      } else {
        filter_(df, input$filter)
      }
    )
    datatable(dat)
  })
}) 
shiny::shinyApp(ui, server)
