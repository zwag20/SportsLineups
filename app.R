#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(jsonlite)
library(shiny)
library(ROI)
library(ROI.plugin.glpk)
library(shinydashboard)
library(DT)
library(shinythemes)
library(rvest)

mycss <- "
.mycheckbox .shiny-input-container {
display: block;
width: 30px;
}
"

# Define UI for application that draws a histogram
ui <- fluidPage(
  theme = shinytheme("cerulean"),
  tags$style(mycss,HTML("input[type='search']:disabled {visibility:hidden}")),
  
  
  tabsetPanel(
    tabPanel("NFL", fluid = TRUE,
             sidebarLayout(
               sidebarPanel(headerPanel("Optimal Lineup"), div(tableOutput("NFLchecked"),style = "font-size:80%; width: 80%"),width = 3),
               mainPanel(
                 headerPanel("Available Players"),
                 selectInput(
                   "NFLmethod",
                   "Choose Method:",
                   c(
                     "Aggressive" = "ceil",
                     "Normal" = "points",
                     "Conservative" = "floor"
                   )
                   ,
                   selected = "points"
                 ),
                 span(class = "mycheckbox", DT::dataTableOutput('NFLtable'), style = "font-size:80%; width: 80%")
               )
             )),
    tabPanel("NBA", fluid = TRUE,
             sidebarLayout(
               sidebarPanel(headerPanel("Optimal Lineup"), div(tableOutput("NBAchecked"),style = "font-size:80%; width: 80%"),width = 3),
               mainPanel(
                 headerPanel("Available Players"),
                 selectInput(
                   "NBAmethod",
                   "Choose Method:",
                   c(
                     "Aggressive" = "ceil",
                     "Normal" = "points",
                     "Conservative" = "floor"
                   )
                   ,
                   selected = "points"
                 ),
                 span(class = "mycheckbox", DT::dataTableOutput('NBAtable'), style = "font-size:80%; width: 80%")
               )
             )),
    tabPanel("MLB", fluid = TRUE,
             sidebarLayout(
               sidebarPanel(headerPanel("Optimal Lineup"),div(tableOutput("MLBchecked"), style = "font-size:80%; width: 80%"),width = 3),
               mainPanel(
                 headerPanel("Available Players"),
                 selectInput(
                   "MLBmethod",
                   "Choose Method:",
                   c(
                     "Aggressive" = "ceil",
                     "Normal" = "points",
                     "Conservative" = "floor"
                   )
                   ,
                   selected = "points"
                 ),
                 span(class = "mycheckbox", DT::dataTableOutput('MLBtable'), style = "font-size:80%; width: 80%")
               )
             ))
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  # helper function for reading checkbox
  shinyValue = function(id, len) {
    unlist(lapply(seq_len(len), function(i) {
      value = input[[paste0(id, i)]]
      if (is.null(value)) NA else value
    }))
  }


  # helper function for making checkbox
  shinyInput = function(FUN, len, id, ...) {
    inputs = character(len)
    for (i in seq_len(len)) {
      inputs[i] = as.character(FUN(paste0(id, i), label = NULL, ...))
    }
    inputs
  }


  source('NFL.R')

  output$NFLtable = DT::renderDataTable({
    NFLData <- data.frame(NFLData, Exclude = shinyInput(checkboxInput, nrow(NFLData), "NFLXcbox_"))
    NFLData <- data.frame(NFLData, Lock = shinyInput(checkboxInput, nrow(NFLData), "NFLLcbox_"))
  }, server = FALSE, escape = FALSE, filter = 'top',rownames = FALSE, options = list(
    colnames = c('Player','Team Name','Position','Salary','Projected','Max Projected','Min Projected','Opponent','Exclude','Lock'),
    columnDefs = list(list(targets = 1:9, class = "dt-head-left"),list(targets = c(8,9), searchable = FALSE),list(width = '60px', targets = c(1:9))),
    paging = FALSE,
    preDrawCallback = JS('function() {
                         Shiny.unbindAll(this.api().table().node()); }'),
    drawCallback = JS('function() {
                      Shiny.bindAll(this.api().table().node()); } ')
    ))

   #output NFL read checkboxes
  output$NFLchecked <- renderTable({
    getNFLLineup(dfnfl,shinyValue("NFLXcbox_",nrow(dfnfl)),shinyValue("NFLLcbox_",nrow(dfnfl)),input$NFLmethod)
    
  })
  
  source('NBA.R')
  
  output$NBAtable = DT::renderDataTable({
    NBAData <- data.frame(NBAData, Exclude = shinyInput(checkboxInput, nrow(NBAData), "NBAXcbox_"))
    NBAData <- data.frame(NBAData, Lock = shinyInput(checkboxInput, nrow(NBAData), "NBALcbox_"))
  }, server = FALSE, escape = FALSE, filter = 'top' , options = list(
    colnames = c('Player','Team Name','Position','Salary','Projected','Max Projected','Min Projected','Opponent','Exclude','Lock'),
    columnDefs = list(list(targets = 1:9, class = "dt-head-left"),list(targets = c(8,9), searchable = FALSE),list(width = '60px', targets = c(1:9))),
    paging = FALSE,rownames= FALSE,
    preDrawCallback = JS('function() {
                         Shiny.unbindAll(this.api().table().node()); }'),
    drawCallback = JS('function() {
                      Shiny.bindAll(this.api().table().node()); } ')
    ))
  
  #output NBA read checkboxes
  output$NBAchecked <- renderTable({
    getNBALineup(dfnba,shinyValue("NBAXcbox_",nrow(dfnba)),shinyValue("NBALcbox_",nrow(dfnba)),input$NBAmethod)
    
  })
  
  
  source('MLB.R')
  
  output$MLBtable = DT::renderDataTable({
    MLBData <- data.frame(MLBData, Exclude = shinyInput(checkboxInput, nrow(MLBData), "MLBXcbox_"))
    MLBData <- data.frame(MLBData, Lock = shinyInput(checkboxInput, nrow(MLBData), "MLBLcbox_"))
  }, server = FALSE, escape = FALSE, filter = 'top' , rownames = FALSE,
    colnames = c('Player','Team Name','Position','Salary','Projected','Max Projected','Min Projected','Opponent','Exclude','Lock'),
    options = list(
    columnDefs = list(list(targets = 1:9, class = "dt-head-left"),list(targets = c(8,9), searchable = FALSE),list(width = '60px', targets = c(1:9))),
    paging = FALSE,
    preDrawCallback = JS('function() {
                         Shiny.unbindAll(this.api().table().node()); }'),
    drawCallback = JS('function() {
                      Shiny.bindAll(this.api().table().node()); } ')
    ))
  
  #output MLB read checkboxes
  output$MLBchecked <- renderTable({
    getMLBLineup(dfmlb,shinyValue("MLBXcbox_",nrow(dfmlb)),shinyValue("MLBLcbox_",nrow(dfmlb)),input$MLBmethod)
    
  })
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)

