library(shiny)
library(tercen)
library(dplyr)
library(tidyr)

############################################
#### This part should not be included in ui.R and server.R scripts
# http://localhost:5402/admin/w/6fbaf37bd937904adf2cb1e8df003b15/ds/068d32cd-919b-427b-b432-b4587963af52
getCtx <- function(session) {
  ctx <- tercenCtx(stepId = "068d32cd-919b-427b-b432-b4587963af52",
                   workflowId = "6fbaf37bd937904adf2cb1e8df003b15")
  return(ctx)
}
####
############################################

ui <- shinyUI(fluidPage(
  
  titlePanel("Histogram"),
  
  sidebarPanel(
    sliderInput("plotWidth", "Plot width (px)", 200, 2000, 500),
    sliderInput("plotHeight", "Plot height (px)", 200, 2000, 500),
  ),
  
  mainPanel(
    uiOutput("reacOut")
  )
  
))

server <- shinyServer(function(input, output, session) {
  
  dataInput <- reactive({
    getValues(session)
  })
  
  output$reacOut <- renderUI({
    plotOutput(
      "main.plot",
      height = input$plotHeight,
      width = input$plotWidth
    )
  }) 
  
  output$main.plot <- renderPlot({
    values <- dataInput()
    data <- values$data$.y
    hist(data)
  })
  
})

getValues <- function(session){
  ctx <- getCtx(session)
  browser()
  values <- list()
  
  values$data <- ctx %>% 
    select(.y, .ri, .axisIndex) %>%
    spread(key = .axisIndex, value = .y) %>%
    mutate(rnames = ctx$rselect()[[1]])
  
  colnames(values$data) <- c("rowIndex", 
                      unlist(ctx$yAxis),
                      "rowNames")

  return(values)
}

runApp(shinyApp(ui, server))  
