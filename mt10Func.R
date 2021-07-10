# Module for monthly LFS wages by industry tables - mt10Func.R
# June 22, 2021

source("Tabl_specs.R")

Sex1000 <- c(
  "Both sexes",
  "Males",
  "Females"
)
Geo1000 <- c(
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
Age1000 <- c(
  "15 years and over",
  "15 to 24 years",
  "25 to 54 years",
  "55 years and over"
)
Typ1000 <- c(
  "Both full- and part-time employees",
  "Full-time employees",               
  "Part-time employees"   
)
Wag1000 <- c(
  "Total employees, all wages",
  "Average hourly wage rate",
  "Average weekly wage rate",
  "Median hourly wage rate",
  "Median weekly wage rate"
)
trf1000 <- c(
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

mt10UI <- function(id) {
  tabPanel(tags$b(tags$span(style="color:blue", HTML("Tables"))),
    tags$style(type='text/css', ".selectize-input { 
      font-size: 24px; line-height: 24px;} .selectize-dropdown 
      { font-size: 20px; line-height: 20px; }"),
    prettyRadioButtons(NS(id,"Wag"), tags$b(tags$span(style="color:blue;font-size:20px", 
        "Choose a wage measure:")),choices=Wag1000,selected="Average hourly wage rate",bigger=TRUE,
        outline=TRUE,inline=TRUE,shape="round",animation="pulse"),
    fluidRow(
      column(4,prettyRadioButtons(NS(id,"Typ"), tags$b(tags$span(style="color:blue;font-size:20px", 
          "Choose a type of employee:")),choices=Typ1000,bigger=TRUE,
          outline=TRUE,inline=TRUE,shape="round",animation="pulse")),
      column(3,prettyRadioButtons(NS(id,"Sex"), tags$b(tags$span(style="color:blue;font-size:20px", 
          "Choose a sex:")),choices=Sex1000,bigger=TRUE,
          outline=TRUE,inline=TRUE,shape="round",animation="pulse")),
      column(5,prettyRadioButtons(NS(id,"Age"), tags$b(tags$span(style="color:blue;font-size:20px", 
          "Choose an age group:")),choices=Age1000,bigger=TRUE,
          outline=TRUE,inline=TRUE,shape="round",animation="pulse"))
    ),
    prettyRadioButtons(NS(id,"Geo"), tags$b(tags$span(style="color:blue;font-size:20px", 
        "Choose a geography:")),choices=Geo1000,bigger=TRUE,
        outline=TRUE,inline=TRUE,shape="round",animation="pulse"),
    prettyRadioButtons(NS(id,"trf"),tags$b(tags$span(style="color:blue;font-size:20px", 
        "Choose a transformation:")),choices=trf1000,bigger=TRUE,
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

mt10Server <- function(id) {
  moduleServer(id,function(input,output,session) {
    Wag2  <- reactive({input$Wag})
    Typ2  <- reactive({input$Typ})
    Geo2  <- reactive({input$Geo})
    Sex2  <- reactive({input$Sex})
    Age2  <- reactive({input$Age})
    type1 <- reactive(case_when(
      input$trf=="Original data (no transformation)"~1,
      input$trf=="Index, first month = 100"~2,
      input$trf=="One-month percentage change"~3,
      input$trf=="Twelve-month percentage change"~4
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
      Make_tablM10(Wag2(),Typ2(),Geo2(),Sex2(),Age2(),type1(),month1(),month2())
    })
    output$tabl <- render_gt({
        expr()[[1]]
    })
    output$downloadData1 <- downloadHandler(
      filename=function() {
        paste0("LFS_wages.csv")
      },
      content=function(file) {
        write.csv(expr()[[2]],file)
      }
    )
    observe({ # monthly table update
      monsRange <- seq.Date(TS[[10]]$Strt,TS[[10]]$Endt,by="month")
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
