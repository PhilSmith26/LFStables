# Make_tablM6() monthly average usual hours and wages tables function
# May 27, 2021

library(gt)
library(tidyverse)
library(lubridate)

source("Tabl_specs.R")

Make_tablM6 <- function(haw,type,month1,month2) {
  q0 <- readRDS(paste0("rds/",TS[[6]]$STCno,".rds"))
  colnam1 <- seq.Date(month1,month2,by="month")
  colnam2 <- vector()
  for (i in 1:length(colnam1)) {
    colnam2[i] <- format(colnam1[i],"%b %Y")
  }
  units <- case_when(
    haw=="Employees, total number"~"Thousands of persons",                        
    haw=="Employees, average usual weekly hours"~"Hours",          
    haw=="Employees, average weekly wages"~"Dollars",                
    haw=="Employees, average hourly wages"~"Dollars",                
    haw=="Full-time employees, total number"~"Thousands of persons",              
    haw=="Full-time employees, average usual weekly hours"~"Hours",
    haw=="Full-time employees, average weekly wages"~"Dollars",    
    haw=="Part-time employees, total number"~"Thousands of persons",              
    haw=="Part-time employees, average usual weekly hours"~"Hours",
    haw=="Part-time employees, average weekly wages"~"Dollars"
  )
  title <- haw
  subtitle <- case_when(
        type==1 ~ paste0(units,"<br>",
          TS[[6]]$Seas,"<br><br>"),
        type==2 ~ paste0("Indexed to ",colnam2[1]," = 100<br>",
          TS[[6]]$Seas,"<br><br>"),
        type==3 ~ paste0("One-month percentage change<br>",
          TS[[6]]$Seas,"<br><br>"),
        type==4 ~ paste0("Twelve-month percentage change<br>",
          TS[[6]]$Seas,"<br><br>")
  )
  if(type==1) { # Display original data
    q1 <- select(q0,REF_DATE,CHR,all_of(haw))
    q1 <- pivot_wider(q1,names_from=CHR,values_from=haw)
    q1 <- filter(q1,REF_DATE>=month1 & REF_DATE<=month2)
    dec1 <- 3
  } else if (type==2) { # Display indexed data
    q1 <- select(q0,REF_DATE,CHR,all_of(haw))
    q1 <- filter(q1,(REF_DATE>=month1 & REF_DATE<=month2))
    q1 <- pivot_wider(q1,names_from=CHR,values_from=haw)
    q1 <- mutate(q1,across(2:ncol(q1),function(x) 
      {y <- round(100*x/x[1],1)}))
    dec1 <- 1
  } else if (type==3) { # Display 1-month % changes
    q1 <- select(q0,REF_DATE,CHR,all_of(haw))
    q1 <- pivot_wider(q1,names_from=CHR,values_from=haw)
    q1 <- mutate(q1,across(2:ncol(q1),function(x) 
      {y <- round(100*(x/lag(x)-1),1)}))
    q1 <- filter(q1,REF_DATE>=month1 & REF_DATE<=month2)
    dec1 <- 1
  } else if (type==4) { # Display 12-month % changes
    q1 <- select(q0,REF_DATE,CHR,all_of(haw))
    q1 <- pivot_wider(q1,names_from=CHR,values_from=haw)
    q1 <- mutate(q1,across(2:ncol(q1),function(x) 
      {y <- round(100*(x/lag(x,12)-1),1)}))
    q1 <- filter(q1,REF_DATE>=month1 & REF_DATE<=month2)
    dec1 <- 1
  }
  nc <- ncol(q1)-1;
  cnms <- colnames(q1)
  tbl_df <- q1
  tbl_df <- as.data.frame(t(tbl_df))
  tbl_df <- mutate(tbl_df,Components=rownames(tbl_df))
  colnames(tbl_df) <- c(colnam2,"Components")
  tbl_df <- tbl_df[2:nrow(tbl_df),]
  rownames(tbl_df) <- NULL
  tbl_df <- select(tbl_df,Components,everything())
  tbl_df <- mutate(tbl_df,across(2:ncol(tbl_df),as.numeric))
  for (i in 1:nrow(tbl_df)) {
    tbl_df[i,1] <- paste0(i,". ",tbl_df[i,1])
  }
  colnam <- colnames(tbl_df)
  colnamx <- colnam[2:length(colnam)]
  ncols <- length(colnamx)
  nrows <- nrow(tbl_df)

  gt_tbl <- gt(data=tbl_df)
  gt_tbl <- tab_options(gt_tbl,table.font.size=24,
    table.background.color=tcol,
    heading.background.color=tcol)
  gt_tbl <- tab_header(gt_tbl,
    title=md(html(paste0("**",title,"**"))),
    subtitle=md(html(paste0(subtitle,"<br><br>"))))
  gt_tbl <- tab_source_note(gt_tbl,
    source_note=md(html(TS[[6]]$Ftnt))) 
  gt_tbl <- cols_align(gt_tbl,
    align=c("left"),
    columns=c(`Components`))
  gt_tbl <- cols_label(gt_tbl,
    Components="")
  gt_tbl <- tab_style(gt_tbl,
    style=list(cell_text(weight="bold")),
    locations=cells_column_labels(colnamx))
  if (dec1!=3) {
    gt_tbl <- fmt_number(gt_tbl,
      columns=c(2:(ncols+1)),  
      decimals=dec1,
      use_seps=TRUE)
  }
  tbl <- list(gt_tbl,tbl_df)
}
