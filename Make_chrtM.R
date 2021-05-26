# LFS monthly charts
# May 25, 2021

pkgs <- c("tidyverse","scales","tibble","stringr","rlang","lubridate")
inst <- lapply(pkgs,library,character.only=TRUE)

source("Tabl_specs.R")

IDX <- function(x) {y <- round(100*x/x[1],1)}
PC <- function(x) {y <- round(100*(x/lag(x)-1),1)}
PC12 <- function(x) {y <- round(100*(x/lag(x,12)-1),1)}
MA5 <- function(x) {
  y <- round((lag(x,2)+lag(x,1)+x+lead(x,1)+lead(x,2))/5,1)
  n <- length(x)
  y[1] <- x[1]; 
  y[2] <- (x[1]+x[2]+x[3])/3
  y[n] <- x[n]
  y[n-1] <- (x[n-2]+x[n-1]+x[n])/3
  return(y)
}
posNeg <- function(x) {sum(x>0,na.rm=TRUE)>0 & sum(x>0,na.rm=TRUE)<length(x)}
datDif <- function(month1,month2) {
  y1 <- year(month1)
  m1 <- month(month1)
  y2 <- year(month2)
  m2 <- month(month2)
  distance <- as.numeric(y2)-as.numeric(y1)
}

# Standard theme for charts
theme_DB <- function(base_size = 11,
  base_family = "",
  base_line_size = base_size / 170,
  base_rect_size = base_size / 170) {
  theme(
    plot.title = element_text(colour="black",size=14,face="bold",hjust=0),
    plot.subtitle = element_text(colour="black",size=14,hjust=0),
    panel.background = element_rect(fill="aliceblue",colour="black"),
    panel.border = element_rect(fill=NA,colour="black"),
    axis.text.x = element_text(angle=45,hjust=1,vjust=0.95,size=10),
    axis.text.y = element_text(size=14),
    complete = TRUE
  )
}

#===============================================================================
# Make_chrt - function to draw an LFS chart
#   tabno - the table number
#   geo - province or Canada
#   sex - Males, Females or Both sexes
#   age - one of 9 age groups
#   type - kind of transformation
#   month1 - first date for the chart
#   month2 - last date for the chart
#   MYtitl - name of a column in the data frame q0
#===============================================================================
Make_chrtM <- function(MYtitl,geo,sex,age,type,month1,month2,altTitl,interv) {
  q0 <- readRDS(paste0("rds/",TS[[1]]$STCno,".rds"))
  q0 <- filter(q0,GEO==geo & SEX==sex & AGE==age)
  q0 <- pivot_wider(q0,names_from=LFC,values_from=VALUE)
  # Check for NA's at start of this series and change start date if necessary
  tmp1 <- mutate(q0,val=.data[[MYtitl]])
  tmp1 <- filter(tmp1,REF_DATE>=month1 & REF_DATE<=month2)
  n <- nrow(tmp1)
  i <- 1
  while (is.na(tmp1$val[i]) & i<n) {i <- i+1}
  month1 <- tmp1$REF_DATE[i]
  
  if (altTitl=="") {ChrtTitl <- MYtitl}
  if (altTitl!="") {ChrtTitl <- altTitl}
  Fmth <- format(month1,"%b %Y")
  Lmth <- format(month2,"%b %Y")
  if (type==1) {
    MYsubtitl=paste0(geo,", ",sex,", ",age,
      "\nMonthly, ",Fmth," to ",Lmth,", ",TS[[1]]$Seas)
    q1 <- mutate(q0,val=.data[[MYtitl]])
    q1 <- filter(q1,REF_DATE>=month1 & REF_DATE<=month2)
    c1 <- ggplot(q1,
      aes(x=REF_DATE,y=val))+
      geom_line(colour="black",size=1.5)+
      scale_y_continuous(labels=scales::"comma")
    if(posNeg(q1$val)) {
      c1 <- c1+ geom_hline(yintercept=0,size=0.4,colour="black",
        linetype="dashed")
    }
  } else if (type==2) {
    MYsubtitl=paste0(geo,", ",sex,", ",age,", including trend line\nMonthly, ",Fmth," to ",
      Lmth,", ",TS[[1]]$Seas)
    q1 <- mutate(q0,val=.data[[MYtitl]])
    q1 <- filter(q1,REF_DATE>=month1 & REF_DATE<=month2)
    c1 <- ggplot(q1,
      aes(x=REF_DATE,y=val))+
      geom_line(colour="black",size=1.5)+
      geom_smooth(method="lm",se=FALSE,linetype="dashed")+
      scale_y_continuous(labels=scales::"comma")
    if(posNeg(q1$val)) {
      c1 <- c1+ geom_hline(yintercept=0,size=0.4,colour="black",
        linetype="dashed")
    }
  } else if (type==3) {
    MYsubtitl=paste0(geo,", ",sex,", ",age,", index with starting quarter = 100\nMonthly, ",Fmth," to ",
      Lmth,", ",TS[[1]]$Seas)
    q0 <- filter(q0,REF_DATE>=month1 & REF_DATE<=month2)
    q1 <- mutate(q0,val=IDX(.data[[MYtitl]]))
    c1 <- ggplot(q1,
      aes(x=REF_DATE,y=val))+
      geom_line(colour="black",size=1.5)+
      scale_y_continuous()
    if(posNeg(q1$val)) {
      c1 <- c1+ geom_hline(yintercept=0,size=0.4,colour="black",
        linetype="dashed")
    }
  } else if (type==4) {
    MYsubtitl=paste0(geo,", ",sex,", ",age,", one-month percentage change\nMonthly, ",Fmth," to ",
      Lmth,", ",TS[[1]]$Seas)
    q1 <- mutate(q0,val=PC(.data[[MYtitl]])/100)
    q1 <- filter(q1,REF_DATE>=month1 & REF_DATE<=month2)
    c1 <- ggplot(q1,
      aes(x=REF_DATE,y=val))+
      geom_col(fill="gold",colour="black",size=0.2)+
      scale_y_continuous(labels=scales::"percent")
    if(posNeg(q1$val)) {
      c1 <- c1+ geom_hline(yintercept=0,size=0.4,colour="black",
        linetype="dashed")
    }
 } else if (type==5) {
    MYsubtitl=paste0(geo,", ",sex,", ",age,", twelve-month percentage change\nMonthly, ",Fmth," to ",
      Lmth,", ",TS[[1]]$Seas)
    q1 <- mutate(q0,val=PC12(.data[[MYtitl]])/100)
    q1 <- filter(q1,REF_DATE>=month1 & REF_DATE<=month2)
    c1 <- ggplot(q1,
      aes(x=REF_DATE,y=val))+
      geom_col(fill="gold",colour="black",size=0.2)+
      scale_y_continuous(labels=scales::"percent")
    if(posNeg(q1$val)) {
      c1 <- c1+ geom_hline(yintercept=0,size=0.4,colour="black",
        linetype="dashed")
    }
  } else if (type==6) {
    MYsubtitl=paste0(geo,", ",sex,", ",age,", five-month centred moving average (dashed blue line)\nMonthly, ",
      Fmth," to ",Lmth,", ",TS[[1]]$Seas)
    q1 <- mutate(q0,val=MA5(.data[[MYtitl]]))
    q1 <- filter(q1,REF_DATE>=month1 & REF_DATE<=month2)
    c1 <- ggplot(q1,
      aes(x=REF_DATE,y=val))+
      geom_line(colour="blue",size=1.5,linetype="dashed")+
      geom_line(aes(x=REF_DATE,y=.data[[MYtitl]]),colour="black",size=1.5)+ 
      scale_y_continuous(labels=scales::"comma")+
    if(posNeg(q1$val)) {
      c1 <- c1+ geom_hline(yintercept=0,size=0.4,colour="black",
        linetype="dashed")
    }
  }
  if (datDif(month1,month2)>20 & interv=="") {
    interv <- "36 months"
  } else if (datDif(month1,month2)>10 & interv=="") {
    interv <- "12 months"
  } else if (datDif(month1,month2)>5 & interv=="") {
    interv <- "3 months"
  } else if (datDif(month1,month2)>2 & interv=="") {
    interv <- "2 months"
  } else if (interv=="") {
    interv <- "1 month"
  }
  c1 <- c1 + scale_x_date(breaks=seq.Date(month1,month2,by=interv))+
    labs(title=ChrtTitl,subtitle=paste0(MYsubtitl),
      caption=TS[[1]]$Ftnt,x="",y="")+
    theme(axis.text.y = element_text(size=18))+
    theme_DB()
  c1
}

#a <- Make_chrtM("Unemployment","Alberta","Both sexes","15 years and over",
#  1,as.Date("2020-01-01"),as.Date("2021-04-01"),"","")
#a
#b <- Make_chrtM()
#b
