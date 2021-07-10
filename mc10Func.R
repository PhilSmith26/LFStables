# Module for long-term LFS charts
# June 20, 2021

source("Tabl_specs.R")

Sex1001 <- c(
  "Both sexes",
  "Males",
  "Females"
)
Geo1001 <- c(
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
Age1001 <- c(
  "15 years and over",
  "15 to 24 years",
  "25 to 54 years",
  "55 years and over"
)
Typ1001 <- c(
  "Both full- and part-time employees",
  "Full-time employees",               
  "Part-time employees"   
)
Wag1001 <- c(
  "Total employees, all wages",
  "Average hourly wage rate",
  "Average weekly wage rate",
  "Median hourly wage rate",
  "Median weekly wage rate"
)
naics1001 <- c(
  "Total employees, all industries",                                            
  "Goods-producing sector",                                                     
  "Agriculture [111-112, 1100, 1151-1152]",                                     
  "Forestry, fishing, mining, quarrying, oil and gas [21, 113-114, 1153, 2100]",
  "Utilities [22]",                                                             
  "Construction [23]",                                                          
  "Manufacturing [31-33]",                                                      
  "Services-producing sector",                                                 
  "Wholesale and retail trade [41, 44-45]",                                     
  "Transportation and warehousing [48-49]",                                     
  "Finance, insurance, real estate, rental and leasing [52-53]",                
  "Professional, scientific and technical services [54]",                       
  "Business, building and other support services [55-56]",                      
  "Educational services [61]",                                                  
  "Health care and social assistance [62]",                                     
  "Information, culture and recreation [51, 71]",                               
  "Accommodation and food services [72]",                                       
  "Other services (except public administration) [81]",                         
  "Public administration [91]"  
)
trf1001 <- c(
  "Original data (no transformation)",
  "Including trend line",
  "Index, first month = 100",
  "One-month percentage change",
  "Twelve-month percentage change",
  "Thirteen-month centred moving average (dashed blue line)"
)
# Starting conditions for initial monthly table and chart
# First the full sequence of dates in "Date" format
monsD <- seq.Date(TS[[8]]$Strt,TS[[8]]$Endt,by="month")
# Now the corresponding sequence of dates in "character" format
monsSrt <- character()
for (i in 1:length(monsD)) {
  monsSrt[i] <- format(monsD[i],"%b %Y")
}
strtrangT <- c(monsSrt[length(monsSrt)-25],monsSrt[length(monsSrt)])

mc10UI <- function(id) {
  tabPanel(tags$b(tags$span(style="color:blue", HTML("Charts"))),
    tags$style(type='text/css', ".selectize-input { 
      font-size: 24px; line-height: 24px;} .selectize-dropdown 
      { font-size: 20px; line-height: 20px; }"),
    prettyRadioButtons(NS(id,"Wag"), tags$b(tags$span(style="color:blue;font-size:20px", 
        "Choose a wage measure:")),choices=Wag1001,selected="Average hourly wage rate",bigger=TRUE,
        outline=TRUE,inline=TRUE,shape="round",animation="pulse"),
    fluidRow(column(4,
      prettyRadioButtons(NS(id,"Typ"), tags$b(tags$span(style="color:blue;font-size:20px", 
          "Choose a type of employee:")),choices=Typ1001,bigger=TRUE,
          outline=TRUE,inline=TRUE,shape="round",animation="pulse")),
      column(3, prettyRadioButtons(NS(id,"Sex"), tags$b(tags$span(style="color:blue;font-size:20px", 
        "Choose a sex:")),choices=Sex1001,bigger=TRUE,
        outline=TRUE,inline=TRUE,shape="round",animation="pulse")),
      column(5, prettyRadioButtons(NS(id,"Age"), tags$b(tags$span(style="color:blue;font-size:20px", 
        "Choose an age group:")),choices=Age1001,bigger=TRUE,
        outline=TRUE,inline=TRUE,shape="round",animation="pulse"))
    ),
    prettyRadioButtons(NS(id,"Geo"), tags$b(tags$span(style="color:blue;font-size:20px", 
        "Choose a geography:")),choices=Geo1001,bigger=TRUE,
        outline=TRUE,inline=TRUE,shape="round",animation="pulse"),
    tags$style(HTML(".selectize-input, .option {
      color:black; 
      font-size:26px;
      font-family:Optima
    }")), 
    selectInput(NS(id,"MYtitl"), tags$b(tags$span(style="color:blue;font-size:20px", 
      "Choose an industry:")),choices=naics1001,width = "100%"),
    fluidRow(column(6,
      prettyRadioButtons(NS(id,"trf"),tags$b(tags$span(style="color:blue;font-size:20px", 
        "Choose a transformation:")),choices=trf1001,bigger=TRUE,
        outline=TRUE,inline=TRUE,shape="round",animation="pulse")),
      column(4,textInput(NS(id,"altTitl"),label="Choose your own chart title (optional):",
        value="",width="90%")),
      column(2,downloadButton(NS(id,"downloadData1"),label="Download chart"))
    ),
    chooseSliderSkin(skin="Round",color="blue"),
    sliderTextInput(NS(id,"Dates"),label= 
      "Choose starting and ending dates:",
      choices=monsSrt,
      selected=strtrangT,
      dragRange = TRUE,
      width="100%"),
    plotOutput(NS(id,"chart")) 
  )
}

mc10Server <- function(id) {
  moduleServer(id,function(input,output,session) {
    Wag2  <- reactive({input$Wag})
    Typ2  <- reactive({input$Typ})
    Geo2  <- reactive({input$Geo})
    Sex2  <- reactive({input$Sex})
    Age2  <- reactive({input$Age})
    type2 <- reactive(case_when(
      input$trf=="Original data (no transformation)"~1,
      input$trf=="Including trend line"~2,
      input$trf=="Index, first month = 100"~3,
      input$trf=="One-month percentage change"~4,
      input$trf=="Twelve-month percentage change"~5,
      input$trf=="Thirteen-month centred moving average (dashed blue line)"~6
    ))    
    MYtitl2  <- reactive({input$MYtitl})
    month1 <- reactive({
      as.Date(paste0(substr(input$Dates[1],1,3)," 1, ",
        substr(input$Dates[1],5,8)),format("%b %d, %Y"))
    })
    month2 <- reactive({
      as.Date(paste0(substr(input$Dates[2],1,3)," 1, ",
        substr(input$Dates[2],5,8)),format("%b %d, %Y"))
    })
    altTitle <- reactive({input$altTitl})
    chartP <- reactive({Make_chrtM10(MYtitl2(),Wag2(),Typ2(),Geo2(),Sex2(),
      Age2(),type2(),month1(),month2(),altTitle(),"")})  
    output$chart <- renderPlot({chartP()},height=700)
    output$downloadData1 <- downloadHandler(
      filename=function() {
        paste0("LFS_wages.png")
      },
      content=function(file) {
        ggsave(file,chartP(),height=8,width=14,dpi=300)
      }
    )
    observe({
      monsRange <- seq.Date(TS[[10]]$Strt,TS[[10]]$Endt,by="month")
      mons <- character()
      for (i in 1:length(monsRange)) {
        mons[i] <- format(monsRange[i],"%b %Y")
      }    
      picks <- mons
      strtrang1 <- c(mons[length(mons)-25],mons[length(mons)])
      updateSliderTextInput(session,inputId="Dates",
        tags$b(tags$span(style="color:blue", 
        label="Choose starting and ending dates:")),
        choices = picks,
        selected=strtrang1)
    })
  })
}

