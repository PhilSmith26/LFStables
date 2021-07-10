# Module for long-term LFS charts
# June 20, 2021

source("Tabl_specs.R")

LFS901 <- c(
  "Population",
  "Labour force population",
  "Labour force",
  "Employment",
  "Unemployment",           
  "Not in the labour force",
  "Participation rate",
  "Unemployment rate",      
  "Employment rate" 
)

trf901 <- c(
  "Original data (no transformation)",
  "Including trend line",
  "Index, first year = 100",
  "Annual percentage change"
)

mc9UI <- function(id) {
  tabPanel(tags$b(tags$span(style="color:blue", HTML("Charts"))),
    tags$style(type='text/css', ".selectize-input { 
      font-size: 24px; line-height: 24px;} .selectize-dropdown 
      { font-size: 20px; line-height: 20px; }"),
    tags$style(HTML(".selectize-input, .option {
      color:black; 
      font-size:26px;
      font-family:Optima
    }")), 
    selectInput(NS(id,"MYtitl"), tags$b(tags$span(style="color:blue;font-size:20px", 
      "Choose a time series:")),choices=LFS901,width = "100%"),
    fluidRow(column(6,
      prettyRadioButtons(NS(id,"trf"),tags$b(tags$span(style="color:blue;font-size:20px", 
        "Choose a transformation:")),choices=trf901,bigger=TRUE,
        outline=TRUE,inline=TRUE,shape="round",animation="pulse")),
      column(4,textInput(NS(id,"altTitl"),label="Choose your own chart title (optional):",
        value="",width="90%")),
      column(2,downloadButton(NS(id,"downloadData1"),label="Download chart"))
    ),
    chooseSliderSkin(skin="Round",color="blue"),
    sliderInput(NS(id,"Dates"),label= 
      "Choose starting and ending dates:",
      min=1921L,max=2020L,
      value=c(1921L,1945L),
      dragRange = TRUE,
      width="100%"),
    plotOutput(NS(id,"chart")) 
  )
}

mc9Server <- function(id) {
  moduleServer(id,function(input,output,session) {
    typedesc <- reactive({input$trf})
    yr1 <- reactive({input$Dates[1]})
    yr2 <- reactive({input$Dates[2]})
    MYtitl1 <- reactive({input$MYtitl})
    type1 <- reactive(case_when(
      input$trf=="Original data (no transformation)"~1,
      input$trf=="Including trend line"~2,
      input$trf=="Index, first year = 100"~3,
      input$trf=="Annual percentage change"~4
    ))
    altTitle <- reactive({input$altTitl})
    chartP <- reactive({Make_chrtM9(MYtitl1(),type1(),yr1(),yr2(),altTitle(),"")})  
    output$chart <- renderPlot({chartP()},height=700)
    output$downloadData1 <- downloadHandler(
      filename=function() {
        paste0(str_replace_all(typedesc()," ","_"),".png")
      },
      content=function(file) {
        ggsave(file,chartP(),height=8,width=14,dpi=300)
      }
    )
    observe({
      updateSliderInput(session,inputId="Dates",
        tags$b(tags$span(style="color:blue", 
        label="Choose starting and ending dates:")),
        min=1921L,max=2020L,
        value=c(1921L,1945L))
    })
  })
}

