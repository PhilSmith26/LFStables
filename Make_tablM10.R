# Make_tablM10() function
# June 22, 2021


# NOTE: The data for tables are stored and must be updated once per month,
# along with the lastdate variable

library(gt)
library(tidyverse)
library(lubridate)

source("Tabl_specs.R")

#===============================================================================
# Make_tablM10 - function to create monthly LFS table
#===============================================================================
Make_tablM10 <- function(Wag,Typ,Geo,Sex,Age,type,month1,month2) {
  q0 <- readRDS(paste0("rds/",TS[[10]]$STCno,".rds"))
  colnam1 <- seq.Date(month1,month2,by="month")
  colnam2 <- vector()
  for (i in 1:length(colnam1)) {
    colnam2[i] <- format(colnam1[i],"%b %Y")
  }
  q0 <- filter(q0,WAG==Wag)
  q0 <- filter(q0,TYP==Typ)
  q0 <- filter(q0,GEO==Geo)
  q0 <- filter(q0,SEX==Sex)
  q0 <- filter(q0,AGE==Age)
  q0 <- select(q0,REF_DATE,NAICS,VALUE)
  q0 <- pivot_wider(q0,names_from=NAICS,values_from=VALUE)
  # set the type variables
  subtitle1 <- case_when(
    Wag=="Total employees, all wages"~"Thousands of persons",
    Wag=="Average hourly wage rate"~"Dollars per hour",
    Wag=="Average weekly wage rate"~"Dollars per week",
    Wag=="Median hourly wage rate"~"Dollars per hour",
    Wag=="Median weekly wage rate"~"Dollars per week"
  )
  subtitle <- case_when(
        type==1 ~ paste0("<br>",subtitle1,"<br>",
          TS[[10]]$Seas,"<br><br>"),
        type==2 ~ paste0("<br>Indexed to ",colnam2[1]," = 100<br>",
          TS[[10]]$Seas,"<br><br>"),
        type==3 ~ paste0("<br>One-month percentage change<br>",
          TS[[10]]$Seas,"<br><br>"),
        type==4 ~ paste0("<br>Twelve-month percentage change<br>",
          TS[[10]]$Seas,"<br><br>")
  )
  if(type==1) { # Display original data
    q1 <- filter(q0,(REF_DATE>=month1 & REF_DATE<=month2))
    if (Wag=="Total employees, all wages") { 
      dec1 <- 0 
    }  else { 
      dec1 <- 2 
    }
  } else if (type==2) { # Display indexed data
    q1 <- filter(q0,REF_DATE>=month1 & REF_DATE<=month2)
    q1 <- mutate(q1,across(2:ncol(q1),function(x) 
      {y <- round(100*x/x[1],1)}))
    dec1 <- 1
  } else if (type==3) { # Display 1-month % changes
    q1 <- mutate(q0,across(2:ncol(q0),function(x) 
      {y <- round(100*(x/lag(x)-1),1)}))
    q1 <- filter(q1,REF_DATE>=month1 & REF_DATE<=month2)
    dec1 <- 1
  } else if (type==4) { # Display 12-month % changes
    q1 <- mutate(q0,across(2:ncol(q0),function(x) 
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
      title=md(html(paste0("**",TS[[10]]$Titl,"**"))),
      subtitle=md(html(paste0(Wag,"\n",subtitle,"<br><br>"))))
  gt_tbl <- tab_source_note(gt_tbl,
      source_note=md(html(TS[[10]]$Ftnt))) 
  gt_tbl <- cols_align(gt_tbl,
      align=c("left"),
      columns=c(`Components`))
  gt_tbl <- cols_label(gt_tbl,
      Components="")
  gt_tbl <- tab_style(gt_tbl,
    style=list(
      cell_text(weight="bold")),
    locations=cells_column_labels(colnamx))
  gt_tbl <- fmt_number(gt_tbl,
    columns=c(2:(ncols+1)),  
    decimals=dec1,
    use_seps=TRUE)
  tbl <- list(gt_tbl,tbl_df)
}

