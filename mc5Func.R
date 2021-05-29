# Module for monthly hourly wage by occupations charts
# May 27, 2021

source("Tabl_specs.R")

hrw501 <- c(
  "Total employees, all wages",
  "Less than $12.00",
  "$12.00 to $19.99",
  "$20.00 to $29.99",
  "$30.00 or more" 
)
tow501 <- c(
  "Both full- and part-time employees",
  "Full-time employees",             
  "Part-time employees" 
)
sex501 <- c(
  "Both sexes",
  "Males",
  "Females" 
)
noc501 <- c(
  "Total employees, all occupations",                                                                   
  "Management occupations [0]",                                                                         
  "Senior management occupations [00]",                                                                 
  "Specialized middle management occupations [01-05]",                                                 
  "Middle management occupations in retail and wholesale trade and customer services [06]",             
  "Middle management occupations in trades, transportation, production and utilities [07-09]",          
  "Business, finance and administration occupations [1]",                                               
  "Professional occupations in business and finance [11]",                                              
  "Administrative and financial supervisors and administrative occupations [12]",                       
  "Finance, insurance and related business administrative occupations [13]",                            
  "Office support occupations [14]",                                                                    
  "Distribution, tracking and scheduling co-ordination occupations [15]",                               
  "Natural and applied sciences and related occupations [2]",                                           
  "Professional occupations in natural and applied sciences [21]",                                      
  "Technical occupations related to natural and applied sciences [22]",                                 
  "Health occupations [3]",                                                                             
  "Professional occupations in nursing [30]",                                                           
  "Professional occupations in health (except nursing) [31]",                                           
  "Technical occupations in health [32]",                                                               
  "Assisting occupations in support of health services [34]",                                           
  "Occupations in education, law and social, community and government services [4]",                    
  "Professional occupations in education services [40]",                                                
  "Professional occupations in law and social, community and government services [41]",                 
  "Paraprofessional occupations in legal, social, community and education services [42]",               
  "Occupations in front-line public protection services [43]",                                          
  "Care providers and educational, legal and public protection support occupations [44]",               
  "Occupations in art, culture, recreation and sport [5]",                                              
  "Professional occupations in art and culture [51]",                                                   
  "Technical occupations in art, culture, recreation and sport [52]",                                   
  "Sales and service occupations [6]",                                                                  
  "Retail sales supervisors and specialized sales occupations [62]",                                    
  "Service supervisors and specialized service occupations [63]",                                       
  "Sales representatives and salespersons - wholesale and retail trade [64]",                           
  "Service representatives and other customer and personal services occupations [65]",                  
  "Sales support occupations [66]",                                                                     
  "Service support and other service occupations, n.e.c. [67]",                                         
  "Trades, transport and equipment operators and related occupations [7]",                              
  "Industrial, electrical and construction trades [72]",                                                
  "Maintenance and equipment operation trades [73]",                                                    
  "Other installers, repairers and servicers and material handlers [74]",                               
  "Transport and heavy equipment operation and related maintenance occupations [75]",                   
  "Trades helpers, construction labourers and related occupations [76]",                                
  "Natural resources, agriculture and related production occupations [8]",                              
  "Supervisors and technical occupations in natural resources, agriculture and related production [82]",
  "Workers in natural resources, agriculture and related production [84]",                              
  "Harvesting, landscaping and natural resources labourers [86]",                                       
  "Occupations in manufacturing and utilities [9]",                                                     
  "Processing, manufacturing and utilities supervisors and central control operators [92]",             
  "Processing and manufacturing machine operators and related production workers [94]",                 
  "Assemblers in manufacturing [95]",                                                                   
  "Labourers in processing, manufacturing and utilities [96]"    
)
trf501 <- c(
  "Original data (no transformation)",
  "Including trend line",
  "Index, first month = 100",
  "One-month percentage change",
  "Twelve-month percentage change",
  "Five-month centred moving average (dashed blue line)"
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
strtrangT <- c(monsSrt[length(monsSrt)-25],monsSrt[length(monsSrt)])

mc5UI <- function(id) {
  tabPanel(tags$b(tags$span(style="color:blue", HTML("Charts"))),
    tags$style(type='text/css', ".selectize-input { 
      font-size: 24px; line-height: 24px;} .selectize-dropdown 
      { font-size: 20px; line-height: 20px; }"),
    selectInput(NS(id,"noc"), tags$b(tags$span(style="color:blue", 
      "Choose an occupational group:")),choices=noc501,
      selectize=FALSE,width = "100%"),
    prettyRadioButtons(NS(id,"hrw"),tags$b(tags$span(style="color:blue", 
        "Choose an hourly wage range:")),choices=hrw501,bigger=TRUE,
        outline=TRUE,inline=TRUE,shape="round",animation="pulse"),
    prettyRadioButtons(NS(id,"tow"),tags$b(tags$span(style="color:blue", 
        "Choose a type of worker:")),choices=tow501,bigger=TRUE,
        outline=TRUE,inline=TRUE,shape="round",animation="pulse"),
    prettyRadioButtons(NS(id,"sex"),tags$b(tags$span(style="color:blue", 
        "Choose a sex:")),choices=sex501,bigger=TRUE,
        outline=TRUE,inline=TRUE,shape="round",animation="pulse"),
    fluidRow(column(6,
      prettyRadioButtons(NS(id,"trf"),tags$b(tags$span(style="color:blue", 
        "Choose a transformation:")),choices=trf501,bigger=TRUE,
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

mc5Server <- function(id) {
  moduleServer(id,function(input,output,session) {
    typedesc <- reactive({input$trf})
    hrw1 <- reactive({input$hrw})
    tow1 <- reactive({input$tow})
    sex1 <- reactive({input$sex})
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
    noc1 <- reactive({input$noc})
    altTitle <- reactive({input$altTitl})
    chartP <- reactive({Make_chrtM5(noc1(),hrw1(),tow1(),sex1(),type1(),
      month1(),month2(),altTitle(),"")})  
    output$chart <- renderPlot({chartP()},height=700)
    output$downloadData1 <- downloadHandler(
      filename=function() {
        paste0(str_replace_all(hrw()," ","_"),"_",
          str_replace_all(tow1()," ","_"),"_",
          str_replace_all(sex1()," ","_"),"_",
          str_replace_all(noc1()," ","_"),"_",
          str_replace_all(typedesc()," ","_"),".png")
      },
      content=function(file) {
        ggsave(file,chartP(),height=8,width=14,dpi=300)
      }
    )
    observe({
      monsRange <- seq.Date(TS[[5]]$Strt,TS[[5]]$Endt,by="month")
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

