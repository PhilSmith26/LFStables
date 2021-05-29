# Module for monthly LFC by occupation tables 
# May 27, 2021

source("Tabl_specs.R")

lfc400 <- c(
  "Labour force",
  "Employment",
  "Full-time employment",
  "Part-time employment",
  "Unemployment",
  "Unemployment rate"
)
geo400 <- c(
  "Canada",
  "Newfoundland and Labrador",
  "Prince Edward Island",
  "Nova Scotia",
  "New Brunswick",
  "Quebec",                   
  "Ontario",
  "Manitoba",
  "Saskatchewan",             
  "Alberta",
  "British Columbia"    
)
sex400 <- c(
  "Both sexes",
  "Males",
  "Females" 
)
trf400 <- c(
  "Original data (no transformation)",
  "Index, first month = 100",
  "One-month percentage change",
  "Twelve-month percentage change"
)
# Starting conditions for initial monthly table and chart
# First the full sequence of dates in "Date" format
monsD <- seq.Date(TS[[4]]$Strt,TS[[4]]$Endt,by="month")
# Now the corresponding sequence of dates in "character" format
monsSrt <- character()
for (i in 1:length(monsD)) {
  monsSrt[i] <- format(monsD[i],"%b %Y")
#  monsSrt[i] <- paste0(year(monsD[i])," M",month(monsD[i]))
}
strtrangT <- c(monsSrt[length(monsSrt)-5],monsSrt[length(monsSrt)])

mt4UI <- function(id) {
  tabPanel(tags$b(tags$span(style="color:blue", HTML("Tables"))),
    tags$style(type='text/css', ".selectize-input { 
      font-size: 24px; line-height: 24px;} .selectize-dropdown 
      { font-size: 20px; line-height: 20px; }"),
    selectInput(NS(id,"lfc"), tags$b(tags$span(style="color:blue", 
      "Choose a labour force characteristic:")),choices=lfc400,
      selectize=FALSE,width = "100%"),
    prettyRadioButtons(NS(id,"geo"),tags$b(tags$span(style="color:blue", 
        "Choose a geography:")),choices=geo400,bigger=TRUE,
        outline=TRUE,inline=TRUE,shape="round",animation="pulse"),
    prettyRadioButtons(NS(id,"sex"),tags$b(tags$span(style="color:blue", 
        "Choose a sex:")),choices=sex400,bigger=TRUE,
        outline=TRUE,inline=TRUE,shape="round",animation="pulse"),
    prettyRadioButtons(NS(id,"trf"),tags$b(tags$span(style="color:blue", 
        "Choose a transformation:")),choices=trf400,bigger=TRUE,
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

mt4Server <- function(id) {
  moduleServer(id,function(input,output,session) {
    typedesc <- reactive({input$trf})
    type1 <- reactive(case_when(
      input$trf=="Original data (no transformation)"~1,
      input$trf=="Index, first month = 100"~2,
      input$trf=="One-month percentage change"~3,
      input$trf=="Twelve-month percentage change"~4
    ))
    lfc1 <- reactive({input$lfc})
    geo1 <- reactive({input$geo})
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
      Make_tablM4(lfc1(),geo1(),sex1(),type1(),month1(),month2())
    })
    output$tabl <- render_gt({
        expr()[[1]]
    })
    output$downloadData1 <- downloadHandler(
      filename=function() {
        paste0(str_replace_all(typedesc()," ","_"),"_",
          str_replace_all(geo1()," ","_"),"_",
          str_replace_all(sex1()," ","_"),"_",
          str_replace_all(lfc1()," ","_"),"_","occupations.csv")
      },
      content=function(file) {
        write.csv(expr()[[2]],file)
      }
    )
    observe({
      monsRange <- seq.Date(TS[[4]]$Strt,TS[[4]]$Endt,by="month")
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
