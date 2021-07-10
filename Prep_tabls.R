# Code to set up the LFS table data frames and save them as rds files
# June 3, 2021. Updated July 8, 2021.

# Make sure the working directory is where the Table_specs.R file is
# Also change all the Endt values in Tabl_specs.R to the latest month
# Also change file_refresh below to TRUE

setwd("/Users/philipsmith/Documents/R/LFStables")
savespot <- "/Users/philipsmith/Documents/R/LFStables/"

pkgs <- c("cansim","tidyverse","stringr","gt","rlist")
inst <- lapply(pkgs,library,character.only=TRUE)

file_refresh <- TRUE

#(01)===========================================================================
table01_id <- "14-10-0287-01" # LFS basic 9 series by geo/sex/age
table01 <- get_cansim(table01_id,refresh=file_refresh) # takes ~5 min (973 MB)
q0 <- filter(table01,`Data type`=="Seasonally adjusted",Statistics=="Estimate")
q0 <- select(q0,REF_DATE,GEO,"LFC"="Labour force characteristics",
  "SEX"="Sex","AGE"="Age group",VALUE)
q0$LFC <- str_replace_all(q0$LFC,"Population","Labour force population")
q0$REF_DATE <- as.Date(paste0(q0$REF_DATE,"-01"))
saveRDS(q0,paste0(savespot,"rds/",table01_id,".rds"))

#(02)===========================================================================
table01_id <- "14-10-0291-01" # LFC by industry, SA
table01 <- get_cansim(table01_id,refresh=file_refresh) # takes about 10 seconds
q0 <- filter(table01,Statistics=="Estimate")
q0 <- select(q0,REF_DATE,"LFC"="Labour force characteristics",
  "NAICS"="North American Industry Classification System (NAICS)",VALUE)
q0$NAICS <- sub(" \\[.*?\\]","",q0$NAICS)
q0$REF_DATE <- as.Date(paste0(q0$REF_DATE,"-01"))
saveRDS(q0,paste0(savespot,"rds/",table01_id,".rds"))

#(03)===========================================================================
table01_id <- "14-10-0289-01" # LFS actual hours worked
table01 <- get_cansim(table01_id,refresh=file_refresh) # takes about 5 seconds
q0 <- filter(table01,Statistics=="Estimate",REF_DATE>="1987-01")
q0 <- select(q0,REF_DATE,
  "NAICS"="North American Industry Classification System (NAICS)",VALUE)
q0$NAICS <- sub(" \\[.*?\\]","",q0$NAICS)
q0$REF_DATE <- as.Date(paste0(q0$REF_DATE,"-01"))
q0 <- pivot_wider(q0,names_from=NAICS,values_from=VALUE)
saveRDS(q0,paste0(savespot,"rds/",table01_id,".rds"))

#(04)===========================================================================
table01_id <- "14-10-0296-01" # LFC by occupation, NSA
table01 <- get_cansim(table01_id,refresh=file_refresh) # takes about 4 min (828 MB)
q0 <- select(table01,REF_DATE,GEO,"SEX"="Sex",
  "LFC"="Labour force characteristics",
  "NOC"="National Occupational Classification (NOC)",VALUE)
q0$REF_DATE <- as.Date(paste0(q0$REF_DATE,"-01"))
saveRDS(q0,paste0(savespot,"rds/",table01_id,".rds"))

#(05)===========================================================================
table01_id <- "14-10-0317-01" # Hourly wage distributions by occ, NSA
table01 <- get_cansim(table01_id,refresh=file_refresh) # takes about 1 min (150 MB)
q0 <- select(table01,REF_DATE,"HRW"="Hourly wages","TOW"="Type of work",
  "NOC"="National Occupational Classification (NOC)","SEX"="Sex",VALUE)
q0$REF_DATE <- as.Date(paste0(q0$REF_DATE,"-01"))
saveRDS(q0,paste0(savespot,"rds/",table01_id,".rds"))

#(06)===========================================================================
table01_id <- "14-10-0320-01" # Avg usual hours & wages by characteristics, NSA
table01 <- get_cansim(table01_id,refresh=file_refresh) # takes about 5 seconds
q0 <- select(table01,REF_DATE,"CHR"="Characteristics",
  "HAW"="Hours and wages",VALUE)
q0 <- pivot_wider(q0,names_from=HAW, values_from=VALUE)
q0$REF_DATE <- as.Date(paste0(q0$REF_DATE,"-01"))
saveRDS(q0,paste0(savespot,"rds/",table01_id,".rds"))

#(08)===========================================================================
# Hours worked by province by industry
library(seasonal)
SEASADJ <- function(x) { 
  x1 <- ts(data=x,start=1987,frequency=12)
  x1_seas <- seas(x1)
  x1_sa <- final(x1_seas)
  y <- as.numeric(x1_sa)
}
IDX <- function(x) {y <- 100*x/x[1]}
table01_id <- "14-10-0036-01" 
file_refresh <- TRUE
save_file <- paste0(table01_id,".rds")
if(!file.exists(save_file)|file_refresh){ # takes about 10 min (1789 MB)
  table01 <- get_cansim(table01_id,refresh=file_refresh)
  saveRDS(table01,file=save_file)
} else {
  table01 <- readRDS(save_file)
}

q0 <- select(table01,REF_DATE,GEO,"SEX"="Sex","COW"="Class of worker",
  "AHW"="Actual hours worked",
  "NAICS"="North American Industry Classification System (NAICS)",VALUE)
q0$REF_DATE <- as.Date(paste0(q0$REF_DATE,"-01"))

# GEO x NAICS = 11x19 seasonal adjustments
q1 <- filter(q0,SEX=="Both sexes",COW=="Total employed",
  AHW=="Total employed, all hours")
q1CA <- pivot_wider(filter(q1,GEO=="Canada"),names_from=NAICS,
  values_from=VALUE)
q1CA <- select(q1CA,-c(2,3,4,5))
q1CA <- mutate(q1CA,across(2:ncol(q1CA),SEASADJ))
q1NL <- pivot_wider(filter(q1,GEO=="Newfoundland and Labrador"),
  names_from=NAICS,values_from=VALUE)
q1NL <- select(q1NL,-c(2,3,4,5))
tmp <- q1NL$`Agriculture [111-112, 1100, 1151-1152]`
q1NL <- select(q1NL,-`Agriculture [111-112, 1100, 1151-1152]`) # has NAs
q1NL <- mutate(q1NL,across(2:ncol(q1NL),SEASADJ))
q1NL <- mutate(q1NL,`Agriculture [111-112, 1100, 1151-1152]`=tmp)
q1NL <- select(q1NL,REF_DATE,`Total employed, all industries`,
  `Goods-producing sector`,`Agriculture [111-112, 1100, 1151-1152]`,everything())
q1PE <- pivot_wider(filter(q1,GEO=="Prince Edward Island"),
  names_from=NAICS,values_from=VALUE)
q1PE <- select(q1PE,-c(2,3,4,5))
tmp <- q1PE$`Utilities [22]`
q1PE <- select(q1PE,-`Utilities [22]`) # has NAs
q1PE <- mutate(q1PE,across(2:ncol(q1PE),SEASADJ))
q1PE <- mutate(q1PE,`Utilities [22]`=tmp)
q1PE <- select(q1PE,REF_DATE,`Total employed, all industries`,
  `Goods-producing sector`,
  `Agriculture [111-112, 1100, 1151-1152]`,
  `Forestry, fishing, mining, quarrying, oil and gas [21, 113-114, 1153, 2100]`,
  `Utilities [22]`,
  everything())
q1NS <- pivot_wider(filter(q1,GEO=="Nova Scotia"),
  names_from=NAICS,values_from=VALUE)
q1NS <- select(q1NS,-c(2,3,4,5))
q1NS <- mutate(q1NS,across(2:ncol(q1NS),SEASADJ))
q1NB <- pivot_wider(filter(q1,GEO=="New Brunswick"),
  names_from=NAICS,values_from=VALUE)
q1NB <- select(q1NB,-c(2,3,4,5))
q1NB <- mutate(q1NB,across(2:ncol(q1NB),SEASADJ))
q1QC <- pivot_wider(filter(q1,GEO=="Quebec"),
  names_from=NAICS,values_from=VALUE)
q1QC <- select(q1QC,-c(2,3,4,5))
q1QC <- mutate(q1QC,across(2:ncol(q1QC),SEASADJ))
q1ON <- pivot_wider(filter(q1,GEO=="Ontario"),
  names_from=NAICS,values_from=VALUE)
q1ON <- select(q1ON,-c(2,3,4,5))
q1ON <- mutate(q1ON,across(2:ncol(q1ON),SEASADJ))
q1MB <- pivot_wider(filter(q1,GEO=="Manitoba"),
  names_from=NAICS,values_from=VALUE)
q1MB <- select(q1MB,-c(2,3,4,5))
q1MB <- mutate(q1MB,across(2:ncol(q1MB),SEASADJ))
q1SK <- pivot_wider(filter(q1,GEO=="Saskatchewan"),
  names_from=NAICS,values_from=VALUE)
q1SK <- select(q1SK,-c(2,3,4,5))
q1SK <- mutate(q1SK,across(2:ncol(q1SK),SEASADJ))
q1AB <- pivot_wider(filter(q1,GEO=="Alberta"),
  names_from=NAICS,values_from=VALUE)
q1AB <- select(q1AB,-c(2,3,4,5))
q1AB <- mutate(q1AB,across(2:ncol(q1AB),SEASADJ))
q1BC <- pivot_wider(filter(q1,GEO=="British Columbia"),
  names_from=NAICS,values_from=VALUE)
q1BC <- select(q1BC,-c(2,3,4,5))
q1BC <- mutate(q1BC,across(2:ncol(q1BC),SEASADJ))
saveRDS(q1CA,"rds/q1CA.rds")
saveRDS(q1NL,"rds/q1NL.rds")
saveRDS(q1PE,"rds/q1PE.rds")
saveRDS(q1NS,"rds/q1NS.rds")
saveRDS(q1NB,"rds/q1NB.rds")
saveRDS(q1QC,"rds/q1QC.rds")
saveRDS(q1ON,"rds/q1ON.rds")
saveRDS(q1MB,"rds/q1MB.rds")
saveRDS(q1SK,"rds/q1SK.rds")
saveRDS(q1AB,"rds/q1AB.rds")
saveRDS(q1BC,"rds/q1BC.rds")

#(07)===========================================================================
table01_id <- "14-10-0304-01" # Job tenure, NSA
table01 <- get_cansim(table01_id,refresh=file_refresh) # takes about 6 min (1227 MB)
q0 <- select(table01,REF_DATE,GEO,"JTN"="Job tenure","SEX"="Sex",
  "NOC"="National Occupational Classification (NOC)",VALUE)
q0$REF_DATE <- as.Date(paste0(q0$REF_DATE,"-01"))
saveRDS(q0,paste0(savespot,"rds/",table01_id,".rds"))

#(11)===========================================================================
table01_id <- "14-10-0342-01" # Duration of unemployment
table01 <- get_cansim(table01_id,refresh=TRUE)
q0 <- filter(table01,Statistics=="Estimate",
  `Data type`=="Seasonally adjusted")
q0 <- select(q0,REF_DATE,GEO,`Age group`,Sex,
  "DOU"="Duration of unemployment",VALUE)
q0$REF_DATE <- as.Date(paste0(q0$REF_DATE,"-01"))
saveRDS(q0,paste0(savespot,"rds/",table01_id,".rds"))

#(10)===========================================================================
table01_id <- "99-99-9999-01" # Long annual LFS time series
#q0 <- readRDS("/Users/philipsmith/Documents/R/MASTER.rds")
q0$REF_DATE <- as.numeric(q0$REF_DATE)
q0 <- select(q0,REF_DATE,
  "Population"="POP",
  "Labour force population"="LPOP",
  "Labour force"="LF",
  "Employment"="E",
  "Unemployment"="U",
  "Not in the labour force"="NLF",
  "Participation rate"="PR",
  "Unemployment rate"="UR",
  "Employment rate"="ER",
  "Gross domestic product at market prices"="GDP",
  "Gross domestic product at constant 2012 prices"="KGDP",
  "Gross domestic product price index"="PGDP",
  "Labour productivity"="PROD"
  )
q0$Population=q0$Population/1000
q0$`Gross domestic product at market prices`[1:40] <- 1000*q0$`Gross domestic product at market prices`[1:40]
q0 <- filter(q0,REF_DATE>=1921)
q0 <- q0[,1:10]
saveRDS(q0,paste0(savespot,"rds/",table01_id,".rds"))







