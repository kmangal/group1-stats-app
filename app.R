####################################################################
# Group 1 Selection Statistics App
#
# Author: Kunal Mangal, Visiting Fellow, Azim Premji University
#         kunal.mangal@apu.edu.in
#
# Data: Obtained from TNPSC through the open data policy at
#       https://tnpsc.gov.in/English/OpenDataPolicy.aspx
#
####################################################################
# Import libraries

library(shiny)
library(shiny.i18n)

library(tidyverse)
library(ggiraph)

####################################################################
# Helper functions

f.format_input <- function(val){
  return(as.integer((val)))
}

f.format_number <- function(n) {
  # Source: https://stackoverflow.com/a/71127294
  dec <- round(n %% 1, 2)
  dec <- ifelse(dec < 0.01, "", substr(dec, 2, 4))
  int <- n %/% 1
  ints <- vapply(int, function(x) {
    x <- as.character(x)
    len <- nchar(x)
    if(len <= 3) return(x)
    rev_x <- paste(rev(unlist(strsplit(x, ""))), collapse = "")
    str   <- paste0(substr(rev_x, 1, 3), ",")
    str2  <- substr(rev_x, 4, 100)
    str2  <- gsub("(\\d{2})", "\\1,", str2)
    rev_x <- paste0(str, str2)
    return(paste(rev(unlist(strsplit(rev_x, ""))), collapse = ""))
  }, character(1))
  
  num <- sub("^,", "", paste0(ints, dec))
  
  return(gsub(" ", "", num, fixed = TRUE))
}

f.filter_data <- function(input){
  
  df.filtered <- df
  
  if (input$gender != "ANY"){
    df.filtered <- df.filtered %>% filter(gender == input$gender)
  }
  
  #if (input$community != "ANY"){
  #  df.filtered <- df.filtered %>% filter(community == input$community)
  #}
  
  if (input$qualification != "ANY") {
    df.filtered <- df.filtered %>% filter(highest.qual == input$qualification)
  }
  
  if (input$nativedistrict != "ANY") {
    df.filtered <- df.filtered %>% filter(nativedistrict == input$nativedistrict)
  }
  
  if (input$pstm != "ANY") {
    df.filtered <- df.filtered %>% filter(pstm == as.integer(input$pstm))
  }
  
  if (input$disability != "ANY"){
    df.filtered <- df.filtered %>% filter(disability == input$disability)
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
  
  return(df.filtered)

}

f.filter_count <- function(input, var) {
  df.filtered <- f.filter_data(input)
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
    return(paste0("1 in ", f.format_number(x)))
  }

}

f.prelim_score_dist <- function(input){
  df.filtered <- f.filter_data(input)
  plot <- df.filtered %>%
    filter(!is.na(total.prelim)) %>%
    mutate(score.bin = floor(total.prelim / 10) * 10) %>%
    group_by(score.bin) %>%
    summarise(n = n()) %>%
    arrange(desc(score.bin)) %>%
    mutate(cn = cumsum(n)) %>%
    mutate(cntip = paste0(f.format_number(cn), " candidates scored more than ", score.bin, " marks")) %>%
    ggplot(aes(x = score.bin, y = n)) +
      xlab("") +
      ylab("Number of candidates") +
      geom_col_interactive(aes(tooltip = cntip), color = 'black', fill = "#0eb074") +
      theme_bw() +
      theme(axis.text = element_text(size = 12), 
          axis.title=element_text(size=16))
  return(plot)
}

f.main_score_dist <- function(input){
  df.filtered <- f.filter_data(input)
  plot <- df.filtered %>%
    filter(!is.na(total.main)) %>%
    mutate(score.bin = floor(total.main / 10) * 10) %>%
    group_by(score.bin) %>%
    summarise(n = n()) %>%
    arrange(desc(score.bin)) %>%
    mutate(cn = cumsum(n)) %>%
    mutate(cntip = paste0(f.format_number(cn), " candidates scored more than ", score.bin, " marks")) %>%
    ggplot(aes(x = score.bin, y = n)) +
    xlab("") +
    ylab("Number of candidates") +
    geom_col_interactive(aes(tooltip = cntip), color = 'black', fill = "#0eb074") +
    theme_bw() +
    theme(axis.text = element_text(size = 12), 
          axis.title=element_text(size=16))
  return(plot)
}

####################################################################
# Translation logic

# TODO: IMPLEMENT TRANSLATION LOGIC
# https://www.r-bloggers.com/2014/11/another-take-on-building-a-multi-lingual-shiny-app/
# https://airbnb.io/polyglot.js/
# https://appsilon.com/internationalization-of-shiny-apps-i18n/
#i18n <- Translator$new(translation_csvs_path = "./translation")
#i18n$set_translation_language("en")

####################################################################
# Load data

df <- readRDS('data/clean/merged_01_2019.Rds')

####################################################################
# Sever logic

server <- function(input, output) {
  
  output$wrote_prelim <- renderText({ 
    f.format_number(f.filter_count(input, 'wrote.prelim'))
  })
  
  output$wrote_main <- renderText({ 
    f.format_number(f.filter_count(input, 'wrote.main'))
  })
  
  output$selected <- renderText({
    f.format_number(f.filter_count(input, 'selected'))
  })
  
  output$selection_rate_overall <- renderText({
    f.filter_selection_rate_overall(input)
  })
  
  output$prelim_score_dist <- renderGirafe({
    girafe( ggobj = f.prelim_score_dist(input) )
  })
  
  output$main_score_dist <- renderGirafe({
    girafe( ggobj = f.main_score_dist(input) )
  })
  
}

####################################################################
# Run the application 

html <- read_file("www/template.html")
ui <- htmlTemplate(text_ = html)
shinyApp(ui = ui, server = server)
