####################################################################
# Group 1 Selection Statistics App
#
# Author: Kunal Mangal, Visiting Fellow, Azim Premji University
#         kunal.mangal@apu.edu.in
#
# Purpose: 
#
# Data: Obtained from TNPSC through the open data policy at
#       https://tnpsc.gov.in/English/OpenDataPolicy.aspx
#


library(shiny)
library(tidyverse)


# Helper functions
f.format_input <- function(val){
  return(as.integer((val)))
}

f.filter_count <- function(input, var) {
  
  input.exservice <- f.format_input(input$exservice)
  input.widow <- f.format_input(input$widow)
  input.blind <- f.format_input(input$blind)
  input.deaf <- f.format_input(input$deaf)
  input.ortho <- f.format_input(input$ortho)
  
  # Filter check boxes
  df.filtered <- df %>%
    filter(exservice >= input.exservice, 
           widow >= input.widow, 
           blind >= input.blind, 
           deaf >= input.deaf,
           ortho >= input.ortho)
  
  if (input$qualification != "ANY") {
    df.filtered <- df.filtered %>% filter(highest.qual == input$qualification)
  }
  
  if (input$nativedistrict != "ANY") {
    df.filtered <- df.filtered %>% filter(nativedistrict == input$nativedistrict)
  }
  
  if (input$age != "ANY") {
    agelimits <- case_when(
      input$age == "21_25" ~ c(21, 25),
      input$age == "26_30" ~ c(26, 30),
      input$age == "31_35" ~ c(31, 35),
      input$age == "36+" ~ c(36, Inf)
    )
    df.filtered <- df.filtered %>% filter(age >= agelimits[1], age <= agelimits[2])
  }
  
  return(sum(df.filtered[[var]]))
  
}

f.filter_selection_rate_overall <- function(input){
  wrote.prelim <- f.filter_count(input, 'wrote.prelim')
  selected <- f.filter_count(input, 'selected')
  
  if (selected == 0){
    return("Selection rate too low to estimate")
  }
  else {
    x <- round(wrote.prelim / selected, 0)
    return(paste0("1 in ", x))
  }

}

# TODO: IMPLEMENT TRANSLATION LOGIC
# https://www.r-bloggers.com/2014/11/another-take-on-building-a-multi-lingual-shiny-app/
# https://airbnb.io/polyglot.js/

# Load data
df <- readRDS('data/clean/merged_01_2019.Rds')

# Sever logic
server <- function(input, output) {
  
  output$wrote_prelim <- renderText({ 
    input$submit
    isolate(f.filter_count(input, 'wrote.prelim'))
  })
  
  output$wrote_main <- renderText({ 
    input$submit
    isolate(f.filter_count(input, 'wrote.main'))
  })
  
  output$selected <- renderText({
    input$submit
    isolate(f.filter_count(input, 'selected'))
  })
  
  output$selection_rate_overall <- renderText({
    input$submit
    isolate(f.filter_selection_rate_overall(input))
  })
  
}

# Run the application 

html <- read_file("www/template.html")
ui <- htmlTemplate(text_ = html)
shinyApp(ui = ui, server = server)
