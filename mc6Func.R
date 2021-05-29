# Module for monthly hours and wages by selected characteristics charts
# May 28, 2021

source("Tabl_specs.R")

haw601 <- c(
  "Employees, total number",                        
  "Employees, average usual weekly hours",          
  "Employees, average weekly wages",                
  "Employees, average hourly wages",                
  "Full-time employees, total number",              
  "Full-time employees, average usual weekly hours",
  "Full-time employees, average weekly wages",      
  "Part-time employees, total number",              
  "Part-time employees, average usual weekly hours",
  "Part-time employees, average weekly wages"
)
chr601 <- c(
  "15 years and over",                                                              
  "15 to 24 years",                                                                 
  "25 years and over",                                                              
  "Males",                                                                          
  "Females",                                                                        
  "Union coverage",                                                                 
  "No union coverage",                                                              
  "Permanent employees",                                                            
  "Temporary employees",                                                            
  "Management occupations [0]",                                                     
  "Business, finance and administration occupations [1]",                           
  "Natural and applied sciences and related occupations [2]",                       
  "Health occupations [3]",                                                         
  "Occupations in education, law and social, community and government services [4]",
  "Occupations in art, culture, recreation and sport [5]",                          
  "Sales and service occupations [6]",                                              
  "Trades, transport and equipment operators and related occupations [7]",          
  "Natural resources, agriculture and related production occupations [8]",          
  "Occupations in manufacturing and utilities [9]"  
)

trf601 <- c(
  "Original data (no transformation)",
  "Including trend line",
  "Index, first month = 100",
  "One-month percentage change",
  "Twelve-month percentage change",
  "Five-month centred moving average (dashed blue line)"
)
# Starting conditions for initial monthly table and chart
# First the full sequence of dates in "Date" format
monsD <- seq.Date(TS[[6]]$Strt,TS[[6]]$Endt,by="month")
# Now the corresponding sequence of dates in "character" format
monsSrt <- character()
for (i in 1:length(monsD)) {
  monsSrt[i] <- format(monsD[i],"%b %Y")
#  monsSrt[i] <- paste0(year(monsD[i])," M",month(monsD[i]))
}
strtrangT <- c(monsSrt[length(monsSrt)-25],monsSrt[length(monsSrt)])

mc6UI <- function(id) {
  tabPanel(tags$b(tags$span(style="color:blue", HTML("Charts"))),
    tags$style(type='text/css', ".selectize-input { 
      font-size: 24px; line-height: 24px;} .selectize-dropdown 
      { font-size: 20px; line-height: 20px; }"),
    selectInput(NS(id,"haw"), tags$b(tags$span(style="color:blue", 
      "Choose a table:")),choices=haw601,
      selectize=FALSE,width = "100%"),
    selectInput(NS(id,"chr"), tags$b(tags$span(style="color:blue", 
      "Choose a characteristic:")),choices=chr601,
      selectize=FALSE,width = "100%"),
    fluidRow(column(6,
      prettyRadioButtons(NS(id,"trf"),tags$b(tags$span(style="color:blue", 
        "Choose a transformation:")),choices=trf601,bigger=TRUE,
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

mc6Server <- function(id) {
  moduleServer(id,function(input,output,session) {
    typedesc <- reactive({input$trf})
    haw1 <- reactive({input$haw})
    chr1 <- reactive({input$chr})
    type1 <- reactive(case_when(
      input$trf=="Original data (no transformation)"~1,
      input$trf=="Including trend line"~2,
      input$trf=="Index, first month = 100"~3,
      input$trf=="One-month percentage change"~4,
      input$trf=="Twelve-month percentage change"~5,
      input$trf=="Five-month centred moving average (dashed blue line)"~6
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
    chartP <- reactive({Make_chrtM6(haw1(),chr1(),type1(),
      month1(),month2(),altTitle(),"")})  
    output$chart <- renderPlot({chartP()},height=700)
    output$downloadData1 <- downloadHandler(
      filename=function() {
        paste0(str_replace_all(haw()," ","_"),"_",
          str_replace_all(chr1()," ","_"),"_",
          str_replace_all(typedesc()," ","_"),".png")
      },
      content=function(file) {
        ggsave(file,chartP(),height=8,width=14,dpi=300)
      }
    )
    observe({
      monsRange <- seq.Date(TS[[6]]$Strt,TS[[6]]$Endt,by="month")
      mons <- character()
      for (i in 1:length(monsRange)) {
        mons[i] <- format(monsRange[i],"%b %Y")
      }    
      picks <- mons
      strtrang1 <- c(mons[length(mons)-25],mons[length(mons)])
      updateSliderTextInput(session,inputId="Dates",tags$b(tags$span(style="color:blue", 
        label="Choose starting and ending dates:")),
        choices = picks,
        selected=strtrang1)
    })
  })
}

