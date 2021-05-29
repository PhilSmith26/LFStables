# Code to set up the LFS table data frames and save them as rds files
# May 24, 2021

# Make sure the working directory is where the Table_specs.R file is
# Also change all the Endt values in Tabl_specs.R to the latest month

setwd("/Users/philipsmith/Documents/R/LFStables")
savespot <- "/Users/philipsmith/Documents/R/LFStables/"

pkgs <- c("cansim","tidyverse","stringr","gt","rlist")
inst <- lapply(pkgs,library,character.only=TRUE)

file_refresh <- FALSE

#(01)===========================================================================
table01_id <- "14-10-0287-01" # LFS basic 9 series by geo/sex/age
table01 <- get_cansim(table01_id,refresh=file_refresh)
q0 <- filter(table01,`Data type`=="Seasonally adjusted",Statistics=="Estimate")
q0 <- select(q0,REF_DATE,GEO,"LFC"="Labour force characteristics",
  "SEX"="Sex","AGE"="Age group",VALUE)
q0$LFC <- str_replace_all(q0$LFC,"Population","Labour force population")
q0$REF_DATE <- as.Date(paste0(q0$REF_DATE,"-01"))
saveRDS(q0,paste0(savespot,"rds/",table01_id,".rds"))

#(02)===========================================================================
table01_id <- "14-10-0291-01" # LFC by industry, SA
table01 <- get_cansim(table01_id,refresh=file_refresh)
q0 <- filter(table01,Statistics=="Estimate")
q0 <- select(q0,REF_DATE,"LFC"="Labour force characteristics",
  "NAICS"="North American Industry Classification System (NAICS)",VALUE)
q0$NAICS <- sub(" \\[.*?\\]","",q0$NAICS)
q0$REF_DATE <- as.Date(paste0(q0$REF_DATE,"-01"))
saveRDS(q0,paste0(savespot,"rds/",table01_id,".rds"))

#(03)===========================================================================
table01_id <- "14-10-0289-01" # LFS actual hours worked
table01 <- get_cansim(table01_id,refresh=file_refresh)
q0 <- filter(table01,Statistics=="Estimate",REF_DATE>="1987-01")
q0 <- select(q0,REF_DATE,
  "NAICS"="North American Industry Classification System (NAICS)",VALUE)
q0$NAICS <- sub(" \\[.*?\\]","",q0$NAICS)
q0$REF_DATE <- as.Date(paste0(q0$REF_DATE,"-01"))
q0 <- pivot_wider(q0,names_from=NAICS,values_from=VALUE)
saveRDS(q0,paste0(savespot,"rds/",table01_id,".rds"))

#(04)===========================================================================
table01_id <- "14-10-0296-01" # LFC by occupation, NSA
table01 <- get_cansim(table01_id,refresh=file_refresh)
q0 <- select(table01,REF_DATE,GEO,"SEX"="Sex",
  "LFC"="Labour force characteristics",
  "NOC"="National Occupational Classification (NOC)",VALUE)
q0$REF_DATE <- as.Date(paste0(q0$REF_DATE,"-01"))
saveRDS(q0,paste0(savespot,"rds/",table01_id,".rds"))

#(05)===========================================================================
table01_id <- "14-10-0317-01" # Hourly wage distributions by occ, NSA
table01 <- get_cansim(table01_id,refresh=file_refresh)
q0 <- select(table01,REF_DATE,"HRW"="Hourly wages","TOW"="Type of work",
  "NOC"="National Occupational Classification (NOC)","SEX"="Sex",VALUE)
q0$REF_DATE <- as.Date(paste0(q0$REF_DATE,"-01"))
saveRDS(q0,paste0(savespot,"rds/",table01_id,".rds"))

#(06)===========================================================================
table01_id <- "14-10-0320-01" # Avg usual hours & wages by characteristics, NSA
table01 <- get_cansim(table01_id,refresh=file_refresh)
q0 <- select(table01,REF_DATE,"CHR"="Characteristics",
  "HAW"="Hours and wages",VALUE)
q0 <- pivot_wider(q0,names_from=HAW, values_from=VALUE)
q0$REF_DATE <- as.Date(paste0(q0$REF_DATE,"-01"))
saveRDS(q0,paste0(savespot,"rds/",table01_id,".rds"))

#(07)===========================================================================
table01_id <- "14-10-0304-01" # Job tenure, NSA
table01 <- get_cansim(table01_id,refresh=file_refresh)
q0 <- select(table01,REF_DATE,GEO,"JTN"="Job tenure","SEX"="Sex",
  "NOC"="National Occupational Classification (NOC)",VALUE)
q0$REF_DATE <- as.Date(paste0(q0$REF_DATE,"-01"))
saveRDS(q0,paste0(savespot,"rds/",table01_id,".rds"))















#(00)===========================================================================
table01_id <- "14-10-0306-01" # LFC wages by occupation, NSA
# Too big, so download partial:
#  GEO = CA, NL, PE, NS, NB, QC
#  Wages = Median hourly wage rate
#  Type of work = "Full-time employees"
#  NOC = all
#  Sex = Both sexes
#  Age group = 15 years and over
#  REF_DATE = all
table01 <- get_cansim(table01_id,refresh=file_refresh)
q0 <- select(table01,REF_DATE,GEO,"SEX"="Sex",
  "LFC"="Labour force characteristics",
  "NOC"="National Occupational Classification (NOC)",VALUE)
q0$REF_DATE <- as.Date(paste0(q0$REF_DATE,"-01"))
saveRDS(q0,paste0(savespot,"rds/",table01_id,".rds"))

