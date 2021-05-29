# Module for monthly hourly wage by occupations tables
# May 27, 2021

source("Tabl_specs.R")

hrw500 <- c(
  "Total employees, all wages",
  "Less than $12.00",
  "$12.00 to $19.99",
  "$20.00 to $29.99",
  "$30.00 or more" 
)
tow500 <- c(
  "Both full- and part-time employees",
  "Full-time employees",             
  "Part-time employees" 
)
sex500 <- c(
  "Both sexes",
  "Males",
  "Females" 
)
trf500 <- c(
  "Original data (no transformation)",
  "Index, first month = 100",
  "One-month percentage change",
  "Twelve-month percentage change"
)
# Starting conditions for initial monthly table and chart
# First the full sequence of dates in "Date" format
monsD <- seq.Date(TS[[5]]$Strt,TS[[5]]$Endt,by="month")
# Now the corresponding sequence of dates in "character" format
monsSrt <- character()
for (i in 1:length(monsD)) {
  monsSrt[i] <- format(monsD[i],"%b %Y")
#  monsSrt[i] <- paste0(year(monsD[i])," M",month(monsD[i]))
}
strtrangT <- c(monsSrt[length(monsSrt)-5],monsSrt[length(monsSrt)])

mt5UI <- function(id) {
  tabPanel(tags$b(tags$span(style="color:blue", HTML("Tables"))),
    tags$style(type='text/css', ".selectize-input { 
      font-size: 24px; line-height: 24px;} .selectize-dropdown 
      { font-size: 20px; line-height: 20px; }"),
    prettyRadioButtons(NS(id,"hrw"),tags$b(tags$span(style="color:blue", 
        "Choose an hourly wage range:")),choices=hrw500,bigger=TRUE,
        outline=TRUE,inline=TRUE,shape="round",animation="pulse"),
    prettyRadioButtons(NS(id,"tow"),tags$b(tags$span(style="color:blue", 
        "Choose a type of work:")),choices=tow500,bigger=TRUE,
        outline=TRUE,inline=TRUE,shape="round",animation="pulse"),
    prettyRadioButtons(NS(id,"sex"),tags$b(tags$span(style="color:blue", 
        "Choose a sex:")),choices=sex500,bigger=TRUE,
        outline=TRUE,inline=TRUE,shape="round",animation="pulse"),
    prettyRadioButtons(NS(id,"trf"),tags$b(tags$span(style="color:blue", 
        "Choose a transformation:")),choices=trf500,bigger=TRUE,
        outline=TRUE,inline=TRUE,shape="round",animation="pulse"),
    column(2,offset=10,downloadButton(NS(id,"downloadData1"),
      label="Download table")),
    chooseSliderSkin(skin="Round",color="blue"),
    sliderTextInput(NS(id,"Dates"),label= 
      "Choose starting and ending dates:",
      choices=monsSrt,
      selected=strtrangT,
      dragRange = TRUE,
      width="100%"),
    gt_output(NS(id,"tabl")) 
  )
}

mt5Server <- function(id) {
  moduleServer(id,function(input,output,session) {
    typedesc <- reactive({input$trf})
    type1 <- reactive(case_when(
      input$trf=="Original data (no transformation)"~1,
      input$trf=="Index, first month = 100"~2,
      input$trf=="One-month percentage change"~3,
      input$trf=="Twelve-month percentage change"~4
    ))
    hrw1 <- reactive({input$hrw})
    tow1 <- reactive({input$tow})
    sex1 <- reactive({input$sex})
    month1 <- reactive({
      as.Date(paste0(substr(input$Dates[1],1,3)," 1, ",
        substr(input$Dates[1],5,8)),format("%b %d, %Y"))
    })
    month2 <- reactive({
      as.Date(paste0(substr(input$Dates[2],1,3)," 1, ",
        substr(input$Dates[2],5,8)),format("%b %d, %Y"))
    })
    expr <- reactive({
      Make_tablM5(hrw1(),tow1(),sex1(),type1(),month1(),month2())
    })
    output$tabl <- render_gt({
        expr()[[1]]
    })
    output$downloadData1 <- downloadHandler(
      filename=function() {
        paste0(str_replace_all(typedesc()," ","_"),"_",
          str_replace_all(hrw1()," ","_"),"_",
          str_replace_all(tow1()," ","_"),"_",
          str_replace_all(sex1()," ","_"),"_","occupations.csv")
      },
      content=function(file) {
        write.csv(expr()[[2]],file)
      }
    )
    observe({
      monsRange <- seq.Date(TS[[5]]$Strt,TS[[5]]$Endt,by="month")
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
