# Make_chrtM9() long-term LFS charts function
# June 20, 2021

pkgs <- c("tidyverse","scales","tibble","stringr","rlang","lubridate")
inst <- lapply(pkgs,library,character.only=TRUE)

source("Common_stuff.R")

Make_chrtM9 <- function(MYtitl,type,yr1,yr2,altTitl,interv) {
  q0 <- readRDS("rds/99-99-9999-01.rds")
  if (altTitl=="") {ChrtTitl <- paste0(MYtitl)}
  if (altTitl!="") {ChrtTitl <- altTitl}
  if (type==1) {
    MYsubtitl <- case_when(
      MYtitl=="Population"~paste0("Thousands of persons\n",yr1," to ",yr2),
      MYtitl=="Labour force population"~paste0("Thousands of persons\n",yr1," to ",yr2),
      MYtitl=="Labour force"~paste0("Thousands of persons\n",yr1," to ",yr2),
      MYtitl=="Employment"~paste0("Thousands of persons\n",yr1," to ",yr2),
      MYtitl=="Unemployment"~paste0("Thousands of persons\n",yr1," to ",yr2),
      MYtitl=="Not in the labour force"~paste0("Thousands of persons\n",yr1," to ",yr2),
      MYtitl=="Participation rate"~paste0("Per cent\n",yr1," to ",yr2),
      MYtitl=="Unemployment rate"~paste0("Per cent\n",yr1," to ",yr2),
      MYtitl=="Employment rate"~paste0("Per cent\n",yr1," to ",yr2)
    )
    q1 <- mutate(q0,val=.data[[MYtitl]])
    q1 <- filter(q1,REF_DATE>=yr1 & REF_DATE<=yr2)
    c1 <- ggplot(q1,
      aes(x=REF_DATE,y=val))+
      geom_line(colour="black",size=1.5)+
      scale_y_continuous(labels=scales::"comma")
    if(posNeg(q1$val)) {
      c1 <- c1+ geom_hline(yintercept=0,size=0.4,colour="black",
        linetype="dashed")
    }
  } else if (type==2) {
    MYsubtitl <- paste0("Thousands of persons, including trend line\n",yr1," to ",yr2)
    MYsubtitl <- case_when(
      MYtitl=="Population"~paste0("Thousands of persons, including a trend line\n",yr1," to ",yr2),
      MYtitl=="Labour force population"~paste0("Thousands of persons, including a trend line\n",yr1," to ",yr2),
      MYtitl=="Labour force"~paste0("Thousands of persons, including a trend line\n",yr1," to ",yr2),
      MYtitl=="Employment"~paste0("Thousands of persons, including a trend line\n",yr1," to ",yr2),
      MYtitl=="Unemployment"~paste0("Thousands of persons, including a trend line\n",yr1," to ",yr2),
      MYtitl=="Not in the labour force"~paste0("Thousands of persons, including a trend line\n",yr1," to ",yr2),
      MYtitl=="Participation rate"~paste0("Per cent, including a trend line\n",yr1," to ",yr2),
      MYtitl=="Unemployment rate"~paste0("Per cent, including a trend line\n",yr1," to ",yr2),
      MYtitl=="Employment rate"~paste0("Per cent, including a trend line\n",yr1," to ",yr2)
    )
    q1 <- mutate(q0,val=.data[[MYtitl]])
    q1 <- filter(q1,REF_DATE>=yr1 & REF_DATE<=yr2)
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
    MYsubtitl <- paste0("Index with initial year = 100\n",yr1," to ",yr2)
    q0 <- filter(q0,REF_DATE>=yr1 & REF_DATE<=yr2)
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
    MYsubtitl <- paste0("Per cent change\n",yr1," to ",yr2)
    q1 <- mutate(q0,val=PC(.data[[MYtitl]])/100)
    q1 <- filter(q1,REF_DATE>=yr1 & REF_DATE<=yr2)
    c1 <- ggplot(q1,
      aes(x=REF_DATE,y=val))+
      geom_col(fill="gold",colour="black",size=0.2)+
      scale_y_continuous(labels=scales::"percent")
    if(posNeg(q1$val)) {
      c1 <- c1+ geom_hline(yintercept=0,size=0.4,colour="black",
        linetype="dashed")
    }
 } 
  interv <- 1
  if (nrow(q1)>49) interv <- 2
  #if (datDif(yr1,yr2)>80 & interv=="") {
  #  interv <- "5 years"
  #} else if (datDif(yr1,yr2)>40 & interv=="") {
  #  interv <- "3 years"
  #} else if (datDif(yr1,yr2)>20 & interv=="") {
  #  interv <- "2 years"
  #} else if (interv=="") {
  #  interv <- "1 year"
  #}
  c1 <- c1 + scale_x_continuous(breaks=seq(yr1,yr2,by=interv))+
    labs(title=ChrtTitl,subtitle=MYsubtitl,
      caption=TS[[9]]$Ftnt,x="",y="")+
    theme(axis.text.y = element_text(size=18))+
    theme_DB()
  c1
}
