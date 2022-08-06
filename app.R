#
# Group 1 Selection Statistics App
#
# Author: Kunal Mangal, Visiting Fellow, Azim Premji University
#         kunal.mangal@apu.edu.in
#
# Purpose: 
#
# License: XXXXXX
#
# Data: Obtained from TNPSC through the open data policy at
#       https://tnpsc.gov.in/English/OpenDataPolicy.aspx
#


library(shiny)
library(tidyverse)

# Load data
df <- readRDS('data/clean/merged_01_2019.Rds')


# Sever logic
server <- function(input, output) {
  
  output$wrote_prelim <- renderText({ 
    print(input$exservice)
    sum(df$wrote.prelim)
  })
  
  output$wrote_main <- renderText({ 
    sum(df$wrote.main)
  })
  
  output$wrote_main <- renderText({ 
    sum(df$wrote.main)
  })
  
  output$selected <- renderText({ 
    sum(df$selected)
  })
  
}

# Run the application 

html <- read_file("www/template.html")
ui <- htmlTemplate(text_ = html)
shinyApp(ui = ui, server = server)
