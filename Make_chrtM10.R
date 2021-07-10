# LFS monthly wage charts
# June 22, 2021

pkgs <- c("tidyverse","scales","tibble","stringr","rlang","lubridate")
inst <- lapply(pkgs,library,character.only=TRUE)

source("Common_stuff.R")

Make_chrtM10 <- function(MYtitl,Wag,Typ,Geo,Sex,Age,type,month1,month2,
  altTitl,interv) {
  q0 <- readRDS(paste0("rds/",TS[[10]]$STCno,".rds"))
  q0 <- filter(q0,WAG==Wag)
  q0 <- filter(q0,TYP==Typ)
  q0 <- filter(q0,GEO==Geo)
  q0 <- filter(q0,SEX==Sex)
  q0 <- filter(q0,AGE==Age)
  q0 <- select(q0,REF_DATE,NAICS,VALUE)
  q0 <- pivot_wider(q0,names_from=NAICS,values_from=VALUE)
  # Check for NA's at start of this series and change start date if necessary
  tmp1 <- mutate(q0,val=.data[[MYtitl]])
  tmp1 <- filter(tmp1,REF_DATE>=month1 & REF_DATE<=month2)
  n <- nrow(tmp1)
  i <- 1
  while (is.na(tmp1$val[i]) & i<n) {i <- i+1}
  month1 <- tmp1$REF_DATE[i]
  
  if (altTitl=="") {ChrtTitl <- paste0(MYtitl," - ",Wag)}
  if (altTitl!="") {ChrtTitl <- altTitl}
  Fmth <- format(month1,"%b %Y")
  Lmth <- format(month2,"%b %Y")
  subtitle1 <- case_when(
    Wag=="Total employees, all wages"~"Thousands of persons",
    Wag=="Average hourly wage rate"~"Dollars per hour",
    Wag=="Average weekly wage rate"~"Dollars per week",
    Wag=="Median hourly wage rate"~"Dollars per hour",
    Wag=="Median weekly wage rate"~"Dollars per week"
  )
  if (type==1) {
    MYsubtitl=paste0("Monthly, ",Fmth," to ",Lmth,"\n",subtitle1,", ",TS[[10]]$Seas)
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
    MYsubtitl=paste0("Including trend line\nMonthly, ",Fmth," to ",
      Lmth,", ",TS[[10]]$Seas)
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
    MYsubtitl=paste0("Index with starting month = 100\nMonthly, ",Fmth," to ",
      Lmth,", ",TS[[10]]$Seas)
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
    MYsubtitl=paste0("One-month percentage change\nMonthly, ",Fmth," to ",
      Lmth,", ",TS[[10]]$Seas)
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
    MYsubtitl=paste0("Twelve-month percentage change\nMonthly, ",Fmth," to ",
      Lmth,", ",TS[[10]]$Seas)
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
    MYsubtitl=paste0("Thirteen-month centred moving average (dashed blue line)\nMonthly, ",
      Fmth," to ",Lmth,", ",TS[[10]]$Seas)
    q1 <- mutate(q0,val=MA13(.data[[MYtitl]]))
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
      caption=TS[[10]]$Ftnt,x="",y="")+
    theme(axis.text.y = element_text(size=18))+
    theme_DB()
  c1
}

