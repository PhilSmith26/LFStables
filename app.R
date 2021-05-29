# Shiny app for the LFS
# Started May 26, 2021
library(shiny)
library(tidyverse)
library(rlist)
library(shinyWidgets)
library(lubridate)

source("Tabl_specs.R") 
source("Make_tablM.R") 
source("Make_chrtM.R") 
source("Make_tablM2.R")
source("Make_chrtM2.R")
source("Make_tablM3.R")
source("Make_chrtM3.R")
source("Make_tablM4.R")
source("Make_chrtM4.R")
source("Make_tablM5.R")
source("Make_chrtM5.R")
source("Make_tablM6.R")
source("Make_chrtM6.R")
source("mtFunc.R")
source("mcFunc.R")
source("mt2Func.R")
source("mc2Func.R")
source("mt3Func.R")
source("mc3Func.R")
source("mt4Func.R")
source("mc4Func.R")
source("mt5Func.R")
source("mc5Func.R")
source("mt6Func.R")
source("mc6Func.R")

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
  navbarMenu(tags$b(tags$span(style="color:blue", HTML("Labour force characteristics<br>by demography"))),
    tags$style(type='text/css', ".selectize-input { 
      font-size: 24px; line-height: 24px;} .selectize-dropdown 
      { font-size: 20px; line-height: 20px; }"),
    mtUI(id="idmt"),
    mcUI(id="idmc")
  ),
  navbarMenu(tags$b(tags$span(style="color:blue", HTML("Labour force characteristics<br>by industry"))),
    tags$style(type='text/css', ".selectize-input { 
      font-size: 24px; line-height: 24px;} .selectize-dropdown 
      { font-size: 20px; line-height: 20px; }"),
    mt2UI(id="idmt2"),
    mc2UI(id="idmc2")
  ),
  navbarMenu(tags$b(tags$span(style="color:blue", HTML("Labour force characteristics<br>by occupation"))),
    tags$style(type='text/css', ".selectize-input { 
      font-size: 24px; line-height: 24px;} .selectize-dropdown 
      { font-size: 20px; line-height: 20px; }"),
    mt4UI(id="idmt4"),
    mc4UI(id="idmc4")
  ),
  navbarMenu(tags$b(tags$span(style="color:blue", HTML("Actual hours worked at<br>main job by industry"))),
    tags$style(type='text/css', ".selectize-input { 
      font-size: 24px; line-height: 24px;} .selectize-dropdown 
      { font-size: 20px; line-height: 20px; }"),
    mt3UI(id="idmt3"),
    mc3UI(id="idmc3")
  ),
  navbarMenu(tags$b(tags$span(style="color:blue", HTML("Hourly wage distributions<br>by occupation"))),
    tags$style(type='text/css', ".selectize-input { 
      font-size: 24px; line-height: 24px;} .selectize-dropdown 
      { font-size: 20px; line-height: 20px; }"),
    mt5UI(id="idmt5"),
    mc5UI(id="idmc5")
  ),
  navbarMenu(tags$b(tags$span(style="color:blue", HTML("Average usual hours and wages<br>by selected characteristics"))),
    tags$style(type='text/css', ".selectize-input { 
      font-size: 24px; line-height: 24px;} .selectize-dropdown 
      { font-size: 20px; line-height: 20px; }"),
    mt6UI(id="idmt6"),
    mc6UI(id="idmc6")
  )
)
server <- function(input, output,session) {
  info <- "Info.html"
  output$textInfo <- renderUI(includeHTML(info))
  mtServer(id="idmt")
  mcServer(id="idmc")
  mt2Server(id="idmt2")
  mc2Server(id="idmc2")
  mt4Server(id="idmt4")
  mc4Server(id="idmc4")
  mt3Server(id="idmt3")
  mc3Server(id="idmc3")
  mt5Server(id="idmt5")
  mc5Server(id="idmc5")
  mt6Server(id="idmt6")
  mc6Server(id="idmc6")
}

shinyApp(ui, server)