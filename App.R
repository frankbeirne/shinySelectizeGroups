rm(list=ls())
library(shiny)
library(shinydashboard)
library(shinyjs)

ui<- shinyUI(fluidPage(
  
  titlePanel("Selectize Test"),
  
    sidebarPanel(
    selectizeInput(
      "groupoptions1", "Group 1", choices = NULL, multiple = TRUE
    ),
    selectizeInput(
      "groupoptions2", "Group 2", choices = NULL, multiple = TRUE
    ),
    selectizeInput(
      "groupoptions3", "Group 3", choices = NULL, multiple = TRUE
    ), 
    selectInput("colour", "Colour", choices=list("white"=1, "black"=2
                                                  )),
    uiOutput("colours"), 
    uiOutput("colours2")
  ),
    
    mainPanel(
      htmlOutput("grouplist")
  )
))

server<- shinyServer(function(input, output, session) {
  
  output$colours<-renderUI({
    colourInput("col1", "colour1", value = chosencolour()[1])
    
    
  })
  output$colours2<-renderUI({
  
    colourInput("col2", "colour2", value = chosencolour()[2])
    
  })
  
  groupdata1 <- reactive({
    
      as.vector(1:30)
    
  })
  
  chosencolour<-reactive({
    switch (as.numeric(input$colour),
      c("#FFFFFF", "#030303"), c("#FF0303", "#03FF03")
    )
  })
  
  groupdata2 <- reactive({
    
    as.vector(1:30)
    
  })
  
  observe({
    vals1<-input$groupoptions1
    vals2<-input$groupoptions2
    vals3<-input$groupoptions3
    
    cat("updata input ")
    cat(isolate(vals1))
    cat(" | ")
    cat(isolate(groupdata2()))
    cat("\n")
    
    updateSelectizeInput(session, "groupoptions1",
                         choices =  groupdata1()[! groupdata1() %in% c(vals2,vals3)], 
                         selected=vals1)
    updateSelectizeInput(session, "groupoptions2", 
                         choices = groupdata2()[! groupdata1() %in% c(vals1, vals3)], 
                         selected=vals2)
    updateSelectizeInput(session, "groupoptions3", 
                         choices = groupdata2()[! groupdata1() %in% c(vals1, vals2)], 
                         selected=vals3)


  })
  
  
  output$grouplist <- renderPrint({
    
      list(
        match(input$groupoptions1, groupdata1()),
        match(input$groupoptions2, groupdata2())
        
      )
    list(input$col1, input$col2)
  })
  
})

shinyApp(ui = ui, server = server)