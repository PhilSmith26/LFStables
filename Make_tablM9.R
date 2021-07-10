# Make_tablM9() long-term LFS tables function
# June 19, 2021

library(gt)
library(tidyverse)
library(lubridate)

source("Tabl_specs.R")
PC1 <- function(x) {
  y <- round(100*(x/lag(x)-1),1)
}

Make_tablM9 <- function(type,yr1,yr2) {
  q0 <- readRDS("rds/99-99-9999-01.rds")
  colnam <- c(yr1:yr2)
  subtitle <- case_when(
        type==1 ~ paste0("Thousands of persons (1-6) or per cent (7-9)<br>"),
        type==2 ~ paste0("Indexed to ",colnam[1]," = 100<br>"),
        type==3 ~ paste0("Annual percentage change<br>")
  )
  if(type==1) { # Display original data
    q1 <- filter(q0,REF_DATE>=yr1 & REF_DATE<=yr2)
    dec1 <- 0
  } else if (type==2) { # Display indexed data
    q1 <- filter(q0,(REF_DATE>=yr1 & REF_DATE<=yr2))
    q1 <- mutate(q1,across(2:ncol(q1),function(x) 
      {y <- round(100*x/x[1],1)}))
    dec1 <- 1
  } else if (type==3) { # Display 1-year % changes
    q1 <- mutate(q0,across(2:ncol(q0),function(x) 
      {y <- round(100*(x/lag(x)-1),1)}))
    q1 <- filter(q1,REF_DATE>=yr1 & REF_DATE<=yr2)
    dec1 <- 1
  }
  nc <- ncol(q1)-1;
  cnms <- colnames(q1)
  tbl_df <- q1
  tbl_df <- as.data.frame(t(tbl_df))
  tbl_df <- mutate(tbl_df,Components=rownames(tbl_df))
  colnames(tbl_df) <- c(colnam,"Components")
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
    title=md(html(paste0("**",TS[[9]]$Titl,"**"))),
    subtitle=md(html(paste0(subtitle,"<br><br>"))))
  gt_tbl <- tab_source_note(gt_tbl,
    source_note=md(html(TS[[9]]$Ftnt))) 
  gt_tbl <- cols_align(gt_tbl,
    align=c("left"),
    columns=c(`Components`))
  gt_tbl <- cols_label(gt_tbl,
    Components="")
  gt_tbl <- tab_style(gt_tbl,
    style=list(cell_text(weight="bold")),
    locations=cells_column_labels(colnamx))
  if (dec1==0) {
    gt_tbl <- fmt_number(gt_tbl,
      columns=c(2:(ncols+1)),  
      rows=c(1:6),
      decimals=dec1,
      use_seps=TRUE)
    gt_tbl <- fmt_number(gt_tbl,
      columns=c(2:(ncols+1)),  
      rows=c(7:9),
      decimals=1,
      use_seps=TRUE)
  } else {
    gt_tbl <- fmt_number(gt_tbl,
      columns=c(2:(ncols+1)),  
      decimals=dec1,
      use_seps=TRUE)    
  }
  tbl <- list(gt_tbl,tbl_df)
}
