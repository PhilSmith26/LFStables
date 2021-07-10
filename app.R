# Shiny app for the LFS
# May 28, 2021
library(shiny)
library(tidyverse)
library(rlist)
library(shinyWidgets)
library(lubridate)
#library(shinythemes)

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
source("Make_tablM7.R")
source("Make_chrtM7.R")
source("Make_tablM8.R")
source("Make_chrtM8.R")
source("Make_tablM9.R")
source("Make_chrtM9.R")
source("Make_tablM10.R")
source("Make_chrtM10.R")
source("Make_tablM11.R")
source("Make_chrtM11.R")
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
source("mt7Func.R")
source("mc7Func.R")
source("mt8Func.R")
source("mc8Func.R")
source("mt9Func.R")
source("mc9Func.R")
source("mt10Func.R")
source("mc10Func.R")
source("mt11Func.R")
source("mc11Func.R")

ui <- navbarPage(
  tags$head(tags$style(HTML('* {font-family: "Optima"};'))),
  title = tags$b(tags$span(style="color:red", 
  "Labour force survey")), #shinythemes::themeSelector(),
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
  navbarMenu(tags$b(tags$span(style="color:blue", 
    HTML("Hours worked<br>by province and industry"))),
    tags$style(type='text/css', ".selectize-input { 
      font-size: 24px; line-height: 24px;} .selectize-dropdown 
      { font-size: 20px; line-height: 20px; }"),
    mt8UI(id="idmt8"),
    mc8UI(id="idmc8")
  ),
  navbarMenu(tags$b(tags$span(style="color:blue", HTML("Actual hours worked at<br>main job by industry"))),
    tags$style(type='text/css', ".selectize-input { 
      font-size: 24px; line-height: 24px;} .selectize-dropdown 
      { font-size: 20px; line-height: 20px; }"),
    mt3UI(id="idmt3"),
    mc3UI(id="idmc3")
  ),
  navbarMenu(tags$b(tags$span(style="color:blue", HTML("Employee wage<br>by industry"))),
    tags$style(type='text/css', ".selectize-input { 
      font-size: 24px; line-height: 24px;} .selectize-dropdown 
      { font-size: 20px; line-height: 20px; }"),
    mt10UI(id="idmt10"),
    mc10UI(id="idmc10")
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
  ),
  navbarMenu(tags$b(tags$span(style="color:blue", HTML("Job tenure<br>by occupation"))),
    tags$style(type='text/css', ".selectize-input { 
      font-size: 24px; line-height: 24px;} .selectize-dropdown 
      { font-size: 20px; line-height: 20px; }"),
    mt7UI(id="idmt7"),
    mc7UI(id="idmc7")
  ),
  navbarMenu(tags$b(tags$span(style="color:blue", HTML("Duration of unemployment"))),
    tags$style(type='text/css', ".selectize-input { 
      font-size: 24px; line-height: 24px;} .selectize-dropdown 
      { font-size: 20px; line-height: 20px; }"),
    mt11UI(id="idmt11"),
    mc11UI(id="idmc11")
  ),
  navbarMenu(tags$b(tags$span(style="color:blue", HTML("Long-term LFS data"))),
    tags$style(type='text/css', ".selectize-input { 
      font-size: 24px; line-height: 24px;} .selectize-dropdown 
      { font-size: 20px; line-height: 20px; }"),
    mt9UI(id="idmt9"),
    mc9UI(id="idmc9")
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
  mt7Server(id="idmt7")
  mc7Server(id="idmc7")
  mt8Server(id="idmt8")
  mc8Server(id="idmc8")
  mt9Server(id="idmt9")
  mc9Server(id="idmc9")
  mt10Server(id="idmt10")
  mc10Server(id="idmc10")
  mt11Server(id="idmt11")
  mc11Server(id="idmc11")
}

shinyApp(ui, server)