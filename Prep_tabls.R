# Code to set up the LFS table data frames and save them as rds files
# May 24, 2021

# Make sure the working directory is where the Table_specs.R file and
# the Series_lists.R files are
# Also change all the Endt values in Tabl_specs.R to the latest quarter

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

