# Module for long-term table
# June 19, 2021

source("Tabl_specs.R")

trf900 <- c(
  "Original data (no transformation)",
  "Index, first year = 100",
  "Annual percentage change"
)

mt9UI <- function(id) {
  tabPanel(tags$b(tags$span(style="color:blue", HTML("Tables"))),
    tags$style(type='text/css', ".selectize-input { 
      font-size: 24px; line-height: 24px;} .selectize-dropdown 
      { font-size: 20px; line-height: 20px; }"),
    prettyRadioButtons(NS(id,"trf"),tags$b(tags$span(style="color:blue;font-size:20px", 
        "Choose a transformation:")),choices=trf900,bigger=TRUE,
        outline=TRUE,inline=TRUE,shape="round",animation="pulse"),
    column(2,offset=10,downloadButton(NS(id,"downloadData1"),
      label="Download table")),
    chooseSliderSkin(skin="Round",color="blue"),
    sliderInput(NS(id,"Dates"),label= 
      "Choose starting and ending dates:",
      min=1921L,max=2020L,
      value=c(1921L,1945L),
      dragRange = TRUE,
      width="100%"),
    gt_output(NS(id,"tabl")) 
  )
}

mt9Server <- function(id) {
  moduleServer(id,function(input,output,session) {
    typedesc <- reactive({input$trf})
    yr1 <- reactive({input$Dates[1]})
    yr2 <- reactive({input$Dates[2]})
    type1 <- reactive(case_when(
      input$trf=="Original data (no transformation)"~1,
      input$trf=="Index, first year = 100"~2,
      input$trf=="Annual percentage change"~3
    ))
    expr <- reactive({
      Make_tablM9(type1(),yr1(),yr2())
    })
    output$tabl <- render_gt({
        expr()[[1]]
    })
    output$downloadData1 <- downloadHandler(
      filename=function() {
        paste0(str_replace_all(typedesc()," ","_"),".csv")
      },
      content=function(file) {
        write.csv(expr()[[2]],file)
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
