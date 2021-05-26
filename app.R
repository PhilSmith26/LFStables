# Shiny app for the LFS
# Started May 23, 2021
library(shiny)
library(tidyverse)
library(rlist)
library(shinyWidgets)
library(lubridate)

source("Tabl_specs.R") # Specifications for tables
source("Make_tablM.R") # monthly table generator
source("Make_chrtM.R") # monthly table generator
source("mtFunc.R")
source("mcFunc.R")

ui <- navbarPage(title = tags$b(tags$span(style="color:red", 
  "Labour force survey")),
  windowTitle = "Canadian labour force survey browser",
  selected = "tabPanel01",
  setBackgroundColor(
    color = c("#d7ebfe", "#6aade7"),
    gradient = "linear",
    direction = "bottom"
  ),
  tabPanel(tags$b(tags$span(style="color:blue", 
    HTML("Introduction<br>and explanation"))),
    value = "tabPanel01",
    htmlOutput("textInfo")
  ),
  #tabPanel(tags$b(tags$span(style="color:blue",HTML("List of<br>tables"))),
  #  value = "tabPanel02",
  #  htmlOutput("tblsInfo")
  #),
  mtUI(id="idmt"),
  mcUI(id="idmc")
)
server <- function(input, output,session) {
  info <- "Info.html"
  output$textInfo <- renderUI(includeHTML(info))
  tbls <- "Tables_list.html"
  output$tblsInfo <- renderUI(includeHTML(tbls))
  mtServer(id="idmt")
  mcServer(id="idmc")
}

shinyApp(ui, server)