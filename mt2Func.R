# Module for monthly LFS by industry tables - mt2Func.R
# May 26, 2021

source("Tabl_specs.R")

lfc200 <- c(
  "Labour force",
  "Employment",
  "Unemployment",
  "Unemployment rate"
)

trf200 <- c(
  "Original data (no transformation)",
  "Index, first month = 100",
  "One-month percentage change",
  "Twelve-month percentage change"
)
# Starting conditions for initial monthly table and chart
# First the full sequence of dates in "Date" format
monsD <- seq.Date(TS[[2]]$Strt,TS[[2]]$Endt,by="month")
# Now the corresponding sequence of dates in "character" format
monsSrt <- character()
for (i in 1:length(monsD)) {
  monsSrt[i] <- format(monsD[i],"%b %Y")
}
strtrangT <- c(monsSrt[length(monsSrt)-5],monsSrt[length(monsSrt)])

mt2UI <- function(id) {
  tabPanel(tags$b(tags$span(style="color:blue", HTML("Tables"))),
    tags$style(type='text/css', ".selectize-input { 
      font-size: 24px; line-height: 24px;} .selectize-dropdown 
      { font-size: 20px; line-height: 20px; }"),
    prettyRadioButtons(NS(id,"lfc"), tags$b(tags$span(style="color:blue;font-size:20px", 
        "Choose a labour force characteristic:")),choices=lfc200,bigger=TRUE,
        outline=TRUE,inline=TRUE,shape="round",animation="pulse"),
    prettyRadioButtons(NS(id,"trf111"),tags$b(tags$span(style="color:blue;font-size:20px", 
        "Choose a transformation:")),choices=trf200,bigger=TRUE,
        outline=TRUE,inline=TRUE,shape="round",animation="pulse"),
    column(2,offset=10,downloadButton(NS(id,"downloadData1"),
      label="Download table")),
    chooseSliderSkin(skin="Round",color="blue"),
    sliderTextInput(NS(id,"Dates"),label="Choose starting and ending dates:",
      choices=monsSrt,
      selected=strtrangT,
      dragRange = TRUE,
      width="100%"),
    htmlOutput(NS(id,"notabl")),
    gt_output(NS(id,"tabl")) 
  )
}

mt2Server <- function(id) {
  moduleServer(id,function(input,output,session) {
    lfc2  <- reactive({input$lfc})
    type1 <- reactive(case_when(
      input$trf111=="Original data (no transformation)"~1,
      input$trf111=="Index, first month = 100"~2,
      input$trf111=="One-month percentage change"~3,
      input$trf111=="Twelve-month percentage change"~4
    ))    
    month1 <- reactive({
      as.Date(paste0(substr(input$Dates[1],1,3)," 1, ",
        substr(input$Dates[1],5,8)),format("%b %d, %Y"))
    })
    month2 <- reactive({
      as.Date(paste0(substr(input$Dates[2],1,3)," 1, ",
        substr(input$Dates[2],5,8)),format("%b %d, %Y"))
    })
    expr <- reactive({
      Make_tablM2(lfc2(),type1(),month1(),month2())
    })
    output$tabl <- render_gt({
        expr()[[1]]
    })
    output$downloadData1 <- downloadHandler(
      filename=function() {
        paste0(str_replace_all(lfc2()," ","_"),".csv")
      },
      content=function(file) {
        write.csv(expr()[[2]],file)
      }
    )
    observe({ # monthly table update
      monsRange <- seq.Date(TS[[2]]$Strt,TS[[2]]$Endt,by="month")
      mons <- character()
      for (i in 1:length(monsRange)) {
        mons[i] <- format(monsRange[i],"%b %Y")
      }    
      picks <- mons
      strtrang1 <- c(mons[length(mons)-5],mons[length(mons)])
      updateSliderTextInput(session,inputId="Dates",tags$b(tags$span(style="color:blue", 
        label="Choose starting and ending dates:")),
        choices = picks,
        selected=strtrang1)
    })
  })
}
