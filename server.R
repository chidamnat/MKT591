## server.R
library(shiny)
library(shinydashboard)

# Define a server for the Shiny app
shinyServer(function(input, output) {
  
  title = reactive({
    source('mainscript.R')
    maintitle(input$inp_text)
   #source('mainscript.R')
   #title = maintitle(input$inp_text)
  })
  
  output$sentiment = renderValueBox({
    valueBox(
      #value = row.names(title())[1],
      value = title(),
      subtitle = "Amazon review sentiment",
      icon = icon("child"),
      color = "aqua"
    )       
  })
  
  
  
  
  
})