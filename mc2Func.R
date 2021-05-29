# Module for monthly LFS charts - mcFunc.R
# May 23, 2021

source("Tabl_specs.R")

lfc1 <- c(
  "Labour force",
  "Employment",
  "Unemployment",
  "Unemployment rate"
)

trf1 <- c(
  "Original data (no transformation)",
  "Including trend line",
  "Index, first month = 100",
  "One-month percentage change",
  "Twelve-month percentage change",
  "Five-month centred moving average (dashed blue line)"
)

ts1 <- c(
  "Total, all industries",                                                      
  "Goods-producing sector",                                                     
  "Agriculture",                                     
  "Forestry, fishing, mining, quarrying, oil and gas",
  "Utilities",                                                             
  "Construction",                                                          
  "Manufacturing",                                                      
  "Services-producing sector",                                                  
  "Wholesale and retail trade",                                     
  "Transportation and warehousing",                                     
  "Finance, insurance, real estate, rental and leasing",                
  "Professional, scientific and technical services",                       
  "Business, building and other support services",                      
  "Educational services",                                                  
  "Health care and social assistance",                                     
  "Information, culture and recreation",                               
  "Accommodation and food services",                                       
  "Other services (except public administration)",                         
  "Public administration",                                                 
  "Unclassified industries"    
)

# Starting conditions for initial monthly table and chart
# First the full sequence of dates in "Date" format
monsD <- seq.Date(TS[[2]]$Strt,TS[[2]]$Endt,by="month")
# Now the corresponding sequence of dates in "character" format
monsSrt <- character()
for (i in 1:length(monsD)) {
  monsSrt[i] <- format(monsD[i],"%b %Y")
#  monsSrt[i] <- paste0(year(monsD[i])," M",month(monsD[i]))
}
strtrangT <- c(monsSrt[length(monsSrt)-25],monsSrt[length(monsSrt)])

mc2UI <- function(id) {
  tabPanel(tags$b(tags$span(style="color:blue", HTML("Charts"))),
    tags$style(type='text/css', ".selectize-input { 
      font-size: 24px; line-height: 24px;} .selectize-dropdown 
      { font-size: 20px; line-height: 20px; }"),
    selectInput(NS(id,"lfc"), tags$b(tags$span(style="color:blue", 
      "Choose a labour force characteristic:")),choices = lfc1,selectize=FALSE,width = "100%"),
    selectInput(NS(id,"ts"), tags$b(tags$span(style="color:blue", 
      "Choose an industry:")),choices = ts1,selectize=FALSE,width = "100%"),
    fluidRow(column(6,
      prettyRadioButtons(NS(id,"trf"),tags$b(tags$span(style="color:blue", 
        "Choose a transformation:")),choices=trf1,bigger=TRUE,
        outline=TRUE,inline=TRUE,shape="round",animation="pulse")),
      column(4,textInput(NS(id,"altTitl"),label="Choose your own chart title (optional):",
        value="",width="90%")),
      column(2,downloadButton(NS(id,"downloadData1"),label="Download chart"))
    ),
    chooseSliderSkin(skin="Round",color="blue"),
    sliderTextInput(NS(id,"Dates"),label= #tags$b(tags$span(style="color:blue", 
      "Choose starting and ending dates:",#)),
      choices=monsSrt,
      selected=strtrangT,
      dragRange = TRUE,
      width="100%"),
    htmlOutput(NS(id,"nochrt")),
    plotOutput(NS(id,"chart")) 
  )
}

mc2Server <- function(id) {
  moduleServer(id,function(input,output,session) {
    lfc2  <- reactive({input$lfc})
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
    ts2 <- reactive({input$ts})
    altTitle <- reactive({input$altTitl})
    output$nochrt <- renderUI({
      if ( ts2()=="Unclassified industries" & lfc2()!="Labour force" & 
          lfc2()!="Unemployment" )
        sendSweetAlert(session,
          title=paste0("This combination of choices is unavailable."),
          text=paste0("The LFS allocates unclassified industries ",
            "entirely to unemployment."),
          type="info",
          btn_labels="Okay",
          btn_colors="#3085d6",
          closeOnClickOutside = TRUE,
          showCloseButton = FALSE,
          width = NULL
          )
    })
    chartP <- reactive({
      if ( !(ts2()=="Unclassified industries" & lfc2()!="Labour force" & 
          lfc2()!="Unemployment" ))
        Make_chrtM2(lfc2(),ts2(),type1(),month1(),month2(),altTitle(),"")})  
    output$chart <- renderPlot({chartP()},height=700)
    output$downloadData1 <- downloadHandler(
      filename=function() {
        #paste0(TS[[1]]$STCno,".png")
        paste0(str_replace_all(lfc2()," ","_"),"_",
          str_replace_all(ts2()," ","_"),".png")
      },
      content=function(file) {
        ggsave(file,chartP(),height=8,width=14,dpi=300)
      }
    )
    observe({ # monthly chart update
      monsRange <- seq.Date(TS[[2]]$Strt,TS[[2]]$Endt,by="month")
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
    #observeEvent(input$sex22, {
    #  message(paste0("input$sex22 class is ",class(input$sex22)))
    #  message(paste0("input$age class is ",class(input$age)))
    #  message(paste0("input$geo class is ",class(input$geo)))
    #  message(paste0("input$tab2 class is ",class(input$tab2)))
    #  message(paste0("input$type1 class is ",class(input$type1)))
    #})
    #observeEvent(input$do,{
    #  session$sendCustomMessage(type = 'testmessage',
    #  message = 'Thank you for clicking')
    #})
  })
}

