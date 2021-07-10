# Module for monthly hours and wages by selected characteristics charts
# May 28, 2021

source("Tabl_specs.R")

geo801 <- c(
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
ind801 <- c(
 "Total employed, all industries",                                             
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
trf801 <- c(
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

mc8UI <- function(id) {
  tabPanel(tags$b(tags$span(style="color:blue", HTML("Charts"))),
    tags$style(type='text/css', ".selectize-input { 
      font-size: 24px; line-height: 24px;} .selectize-dropdown 
      { font-size: 20px; line-height: 20px; }"),
    tags$style(HTML(".selectize-input, .option {
      color:black; 
      font-size:26px;
      font-family:Optima
    }")), 
    selectInput(NS(id,"geo"), tags$b(tags$span(style="color:blue;font-size:20px", 
      "Choose a geography:")),choices=geo801,width = "100%"),
    tags$style(HTML(".selectize-input, .option {
      color:black; 
      font-size:26px;
      font-family:Optima
    }")), 
    selectInput(NS(id,"ind"), tags$b(tags$span(style="color:blue;font-size:20px", 
      "Choose an industry:")),choices=ind801,width = "100%"),
    fluidRow(column(6,
      prettyRadioButtons(NS(id,"trf"),tags$b(tags$span(style="color:blue;font-size:20px", 
        "Choose a transformation:")),choices=trf801,bigger=TRUE,
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

mc8Server <- function(id) {
  moduleServer(id,function(input,output,session) {
    typedesc <- reactive({input$trf})
    geo1 <- reactive({input$geo})
    ind1 <- reactive({input$ind})
    type1 <- reactive(case_when(
      input$trf=="Original data (no transformation)"~1,
      input$trf=="Including trend line"~2,
      input$trf=="Index, first month = 100"~3,
      input$trf=="One-month percentage change"~4,
      input$trf=="Twelve-month percentage change"~5,
      input$trf=="Thirteen-month centred moving average (dashed blue line)"~6
    ))    
    month1 <- reactive({
      as.Date(paste0(substr(input$Dates[1],1,3)," 1, ",
        substr(input$Dates[1],5,8)),format("%b %d, %Y"))
    })
    month2 <- reactive({
      as.Date(paste0(substr(input$Dates[2],1,3)," 1, ",
        substr(input$Dates[2],5,8)),format("%b %d, %Y"))
    })
    altTitle <- reactive({input$altTitl})
    chartP <- reactive({Make_chrtM8(ind1(),geo1(),type1(),
      month1(),month2(),altTitle(),"")})  
    output$chart <- renderPlot({chartP()},height=700)
    output$downloadData1 <- downloadHandler(
      filename=function() {
        paste0(str_replace_all(geo1()," ","_"),"_",
               str_replace_all(ind1()," ","_"),"_",
               str_replace_all(typedesc()," ","_"),".png")
      },
      content=function(file) {
        ggsave(file,chartP(),height=8,width=14,dpi=300)
      }
    )
    observe({
      monsRange <- seq.Date(TS[[8]]$Strt,TS[[8]]$Endt,by="month")
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

