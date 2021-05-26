# Module for monthly LFS charts - mcFunc.R
# May 23, 2021

source("Tabl_specs.R")

geo1 <- c( # geography options for a monthly chart
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
sex1 <- c( # sex options for a monthly chart
  "Both sexes",
  "Males",
  "Females"
)
age1 <- c( # age group options for a monthly chart
  "15 years and over",
  "15 to 64 years",
  "15 to 24 years",    
  "15 to 19 years",  
  "20 to 24 years",    
  "25 years and over", 
  "25 to 54 years",   
  "55 years and over",
  "55 to 64 years"  
)
trf2 <- c(
  "Original data (no transformation)",
  "Including trend line",
  "Index, first month = 100",
  "One-month percentage change",
  "Twelve-month percentage change",
  "Five-month centred moving average (dashed blue line)"
)
ts <- c(
  "Labour force population",
  "Labour force",
  "Employment",
  "Full-time employment",
  "Part-time employment",
  "Unemployment",
  "Unemployment rate",
  "Participation rate",
  "Employment rate"
)

# Starting conditions for initial monthly table and chart
# First the full sequence of dates in "Date" format
monsD <- seq.Date(TS[[1]]$Strt,TS[[1]]$Endt,by="month")
# Now the corresponding sequence of dates in "character" format
monsSrt <- character()
for (i in 1:length(monsD)) {
  monsSrt[i] <- format(monsD[i],"%b %Y")
#  monsSrt[i] <- paste0(year(monsD[i])," M",month(monsD[i]))
}
strtrangT <- c(monsSrt[length(monsSrt)-5],monsSrt[length(monsSrt)])

mcUI <- function(id) {
  tabPanel(tags$b(tags$span(style="color:blue", HTML("Monthly<br>charts"))),
    tags$style(type='text/css', ".selectize-input { 
      font-size: 24px; line-height: 24px;} .selectize-dropdown 
      { font-size: 20px; line-height: 20px; }"),
    selectInput(NS(id,"MYtitl1"), tags$b(tags$span(style="color:blue", 
      "Choose a time series:")),choices = ts,selectize=FALSE,width = "100%"),
    prettyRadioButtons(NS(id,"geo"), tags$b(tags$span(style="color:blue", 
        "Choose a geography:")),choices=geo1,bigger=TRUE,
        outline=TRUE,inline=TRUE,shape="round",animation="pulse"),
    prettyRadioButtons(NS(id,"sex22"), tags$b(tags$span(style="color:blue", 
        "Choose a sex:")),choices=sex1,bigger=TRUE,
        outline=TRUE,inline=TRUE,shape="round",animation="pulse"),
    prettyRadioButtons(NS(id,"age"), tags$b(tags$span(style="color:blue", 
        "Choose an age group:")),choices=age1,bigger=TRUE,
        outline=TRUE,inline=TRUE,shape="round",animation="pulse"),
    fluidRow(column(6,
      prettyRadioButtons(NS(id,"trf22"),tags$b(tags$span(style="color:blue", 
        "Choose a transformation:")),choices=trf2,bigger=TRUE,
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

mcServer <- function(id) {
  moduleServer(id,function(input,output,session) {
    ts1  <- reactive({input$ts1})
    geo2  <- reactive({input$geo})
    sex2  <- reactive({input$sex22})
    age2  <- reactive({input$age})
    type1 <- reactive(case_when(
      input$trf22=="Original data (no transformation)"~1,
      input$trf22=="Including trend line"~2,
      input$trf22=="Index, first month = 100"~3,
      input$trf22=="One-month percentage change"~4,
      input$trf22=="Twelve-month percentage change"~5,
      input$trf22=="Five-month centred moving average (dashed blue line)"~6
    ))    
    month1 <- reactive({
      as.Date(paste0(substr(input$Dates[1],1,3)," 1, ",
        substr(input$Dates[1],5,8)),format("%b %d, %Y"))
    })
    month2 <- reactive({
      as.Date(paste0(substr(input$Dates[2],1,3)," 1, ",
        substr(input$Dates[2],5,8)),format("%b %d, %Y"))
    })
    MYtitl <- reactive({input$MYtitl1})
    altTitle <- reactive({input$altTitl})
    output$nochrt <- renderUI({
      if ( (geo2()!="Canada") & (age2()=="15 to 19 years" | 
          age2()=="20 to 24 years" | age2()=="55 to 64 years") )
        sendSweetAlert(session,
          title=paste0("This combination of choices is available only ",
            "for Canada as a whole."),
          text=paste0("The labour force survey does not provide ",
            "estimates for this combination of geography, sex and ",
            "age group due to small sample size."),
          type="info",
          btn_labels="Okay",
          btn_colors="#3085d6",
          closeOnClickOutside = TRUE,
          showCloseButton = FALSE,
          width = NULL
          )
    })
    chartP <- reactive({
      if ( !((geo2()!="Canada") & (age2()=="15 to 19 years" | 
          age2()=="20 to 24 years" | age2()=="55 to 64 years")) )
        Make_chrtM(MYtitl(),geo2(),sex2(),age2(),type1(),month1(),month2(),
          altTitle(),"")})   
    output$chart <- renderPlot({chartP()},height=700)
    output$downloadData1 <- downloadHandler(
      filename=function() {
        #paste0(TS[[1]]$STCno,".png")
        paste0(gsub("([ ])","_",geo2()),"_",
          gsub("([ ])","_",sex2()),"_",
          gsub("([ ])","_",age2()),".png")
      },
      content=function(file) {
        ggsave(file,chartP(),height=8,width=14,dpi=300)
      }
    )
    observe({ # monthly chart update
      monsRange <- seq.Date(TS[[1]]$Strt,TS[[1]]$Endt,by="month")
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
