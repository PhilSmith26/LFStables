# Make_tablM() function
# May 23, 2021

#===============================================================================
# Make_tabl - function to generate a gt() table
# tabno - the number of the table to draw (1,2,...numTabs)
# type - the choice of transformation (1,2,3,4,5)
# mon1 - first quarter for table (a Date variable)
# mon2 - last quarter for table (a Date variable)
#===============================================================================

# NOTE: The data for tables are stored and must be updated once per quarter,
# along with the lastdate variable

library(gt)
library(tidyverse)
library(lubridate)

source("Tabl_specs.R")

#===============================================================================
# Make_tablM - function to create monthly LFS table
# tabno - the number of the table to draw (1,2,...numTabs)
# geo - the chosen geography (prov or Canada)
# sex - the chosen sex
# age - the chosen age group
# type - the choice of transformation
# month1 - first month for table
# month2 - last month for table
#===============================================================================
Make_tablM <- function(geo,sex,age,type,month1,month2) {
  q0 <- readRDS(paste0("rds/",TS[[1]]$STCno,".rds"))
  colnam1 <- seq.Date(month1,month2,by="month")
  colnam2 <- vector()
  for (i in 1:length(colnam1)) {
    colnam2[i] <- format(colnam1[i],"%b %Y")
  }
  subtitle1 <- paste0(geo,", ",sex,", ",age,"\n")
  # There are forbidden age values, unless GEO is Canada: 
  #if ((geo!="Canada") & (age=="15 to 19 years" | age=="20 to 24 years" |
  #    age=="55 to 64 years")) 
  # This is controlled in the server function
  q0 <- filter(q0,SEX==sex,GEO==geo,AGE==age)
  q0 <- select(q0,REF_DATE,LFC,VALUE)
  q0 <- pivot_wider(q0,names_from=LFC,values_from=VALUE)
  # set the type variables
  subtitle2 <- case_when(
        type==1 ~ paste0("<br>",TS[[1]]$Units,"<br>",
          TS[[1]]$Seas,"<br><br>"),
        type==2 ~ paste0("<br>Indexed to ",colnam2[1]," = 100<br>",
          TS[[1]]$Seas,"<br><br>"),
        type==3 ~ paste0("<br>One-month percentage change<br>",
          TS[[1]]$Seas,"<br><br>"),
        type==4 ~ paste0("<br>Twelve-month percentage change<br>",
          TS[[1]]$Seas,"<br><br>")
  )
  if(type==1) { # Display original data
    q1 <- filter(q0,(REF_DATE>=month1 & REF_DATE<=month2))
    dec1 <- 0; dec2 <- 1
  } else if (type==2) { # Display indexed data
    q1 <- filter(q0,REF_DATE>=month1 & REF_DATE<=month2)
    q1 <- mutate(q1,across(2:ncol(q1),function(x) 
      {y <- round(100*x/x[1],1)}))
    dec1 <- 1; dec2 <- 3
  } else if (type==3) { # Display 1-month % changes
    q1 <- mutate(q0,across(2:ncol(q0),function(x) 
      {y <- round(100*(x/lag(x)-1),1)}))
    q1 <- filter(q1,REF_DATE>=month1 & REF_DATE<=month2)
    dec1 <- 1; dec2 <- 3
  } else if (type==4) { # Display 12-month % changes
    q1 <- mutate(q0,across(2:ncol(q0),function(x) 
      {y <- round(100*(x/lag(x,12)-1),1)}))
    q1 <- filter(q1,REF_DATE>=month1 & REF_DATE<=month2)
    dec1 <- 1; dec2 <- 3
  }
  if (dec2==3) {
    q1 <- select(q1,-ends_with(" rate"))
  }
  subtitle3 <- paste0(subtitle1,subtitle2)
  nc <- ncol(q1)-1;
  cnms <- colnames(q1)
  ct2 <- sum(str_count(cnms," rate"))
  ct1 <- nc-ct2
  ftnote <- paste0("Lines 1-",ct1," are thousands of persons. The ",
  "unemployment rate is a percentage of the labour force, the ",
  "participation and employment rates are percentages ",
  "of the labour force population.")
  # Transpose the data frame and make it ready for the gt() function
  tbl_df <- q1 #  filter(q1,REF_DATE>=month1 & REF_DATE<=month2)
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
      #container.width = 1800,
      table.background.color=tcol,
      heading.background.color=tcol)
  gt_tbl <- tab_header(gt_tbl,
      title=md(html(paste0("**",TS[[1]]$Titl,"**"))),
      subtitle=md(html(paste0(subtitle3,"<br><br>"))))
  gt_tbl <- tab_source_note(gt_tbl,
      source_note=md(html(TS[[1]]$Ftnt))) 
  if (type==1) {
    gt_tbl <- tab_footnote(gt_tbl,footnote=ftnote,
      locations=cells_title(groups="title"))
  }
    gt_tbl <- cols_align(gt_tbl,
      align=c("left"),
      columns=vars(`Components`))
  gt_tbl <- cols_label(gt_tbl,
      Components="")
  gt_tbl <- tab_style(gt_tbl,
    style=list(
      cell_text(weight="bold")),
    locations=cells_column_labels(colnamx))
  if (dec2==1) {
    gt_tbl <- fmt_number(gt_tbl,
    columns=c(2:(ncols+1)),  
    rows=c(1:ct1),
      decimals=dec1,
      use_seps=TRUE)
    gt_tbl <- fmt_number(gt_tbl,
      columns=c(2:(ncols+1)),  
      rows=c((ct1+1):nrows),
      decimals=dec2,
      use_seps=TRUE)
  } else {
    gt_tbl <- fmt_number(gt_tbl,
    columns=c(2:(ncols+1)),  
    rows=c(1:nrows),
      decimals=dec1,
      use_seps=TRUE)    
  }
  tbl <- list(gt_tbl,tbl_df)
}

# Tests
#(Make_tablM(1,"British Columbia","Females","55 years and over",
#  1,as.Date("2020-10-01"),as.Date("2021-04-01")))
#(Make_tablM(1,"Quebec","Males","25 years and over",
#  2,as.Date("2020-09-01"),as.Date("2021-04-01")))
#(Make_tablM(1,"Saskatchewan","Females","15 to 64 years",
#  3,as.Date("2020-10-01"),as.Date("2021-02-01")))
#(Make_tablM(1,"Canada","Both sexes","55 to 64 years",
#  4,as.Date("2015-10-01"),as.Date("2021-04-01")))
#(Make_tablM(1,"Canada","Both sexes","15 years and over",
#  1,as.Date("2015-10-01"),as.Date("2021-04-01")))
