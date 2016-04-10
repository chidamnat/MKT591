library(shiny)

## ui.R ##
library(shinydashboard)
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Sentiment Analysis", tabName = "app", icon = icon("play"))
    
  )
)

body <- dashboardBody(
  
  
  tabItem(tabName = 'table',
          fluidRow(
            valueBoxOutput("sentiment")
            
          ),
          
          fluidRow(
            box(
              title = "Inputs", status = "warning",
              width = 5,height = 450,
              collapsible = TRUE,
              textInput("inp_text", "Enter your text here:",width = '100%',value = "Awesome product. very good"),
              br(),
              submitButton(text="Submit",icon = icon('play'))
              
            )
            
          )
          
          
          
          
  )
  
  
  
  
  
  
)

# Put them together into a dashboardPage
dashboardPage(
  skin = 'yellow',
  dashboardHeader(title = "Setiment analyzer"),
  sidebar,
  body
)


