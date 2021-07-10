# Tabl_specs.R
# Contains the specifications or metadata for the LFS tables
# June 22, 2021. Updated July 8, 2021.

# NOTE: The data for tables are stored and must be updated 
# once per month.

tcol <- "#E3ECF6"
endDate0 <- as.Date("2021-06-01")

t01 <- list(
  Num = 1,
  Strt = as.Date("1976-01-01"),
  Endt = endDate0,
  STCno = "14-10-0287-01",
  Units = "Thousands of persons",
  Titl = "Labour force characteristics",
  Ftnt = "Source: Statistics Canada table 14-10-0287-01.",
  Seas = "Seasonally adjusted",
  Size = 979744
)

t02 <- list(
  Num = 2,
  Strt = as.Date("1987-01-01"),
  Endt = endDate0,
  Units = "Thousands of persons",
  STCno = "14-10-0291-01",
  Titl = "Labour force characteristics by industry",
  Ftnt = "Source: Statistics Canada table 14-10-0291-01.",
  Seas = "Seasonally adjusted",
  Size = 412
)

t03 <- list(
  Num = 3,
  Strt = as.Date("1987-01-01"),
  Endt = endDate0,
  Units = "Thousands of hours",
  STCno = "14-10-0289-01",
  Titl = "Actual hours worked at main job by industry",
  Ftnt = "Source: Statistics Canada table 14-10-0289-01.",
  Seas = "Seasonally adjusted",
  Size = 32136
)

t04 <- list(
  Num = 4,
  Strt = as.Date("1987-01-01"),
  Endt = endDate0,
  Units = "Thousands of persons",
  STCno = "14-10-0296-01",
  Titl = "Labour force characteristics by occupation",
  Ftnt = "Source: Statistics Canada table 14-10-0296-01.",
  Seas = "Not seasonally adjusted",
  Size = 4241952
)

t05 <- list(
  Num = 5,
  Strt = as.Date("1997-01-01"),
  Endt = endDate0,
  Units = "Thousands of persons",
  STCno = "14-10-0317-01",
  Titl = "Hourly wage distributions by occupation",
  Ftnt = "Source: Statistics Canada table 14-10-0317-01.",
  Seas = "Not seasonally adjusted",
  Size = 670140
)

t06 <- list(
  Num = 6,
  Strt = as.Date("2001-01-01"),
  Endt = endDate0,
  Units = "",
  STCno = "14-10-0320-01",
  Titl = "Average usual hours and wages by selected characteristics",
  Ftnt = "Source: Statistics Canada table 14-10-0320-01.",
  Seas = "Not seasonally adjusted",
  Size = 4636
)

t07 <- list(
  Num = 7,
  Strt = as.Date("1987-01-01"),
  Endt = endDate0,
  Units = "",
  STCno = "14-10-0304-01",
  Titl = "Job tenure by occupation",
  Ftnt = "Source: Statistics Canada table 14-10-0304-01.",
  Seas = "Not seasonally adjusted",
  Size = 6240564
)

t08 <- list(
  Num = 8,
  Strt = as.Date("1987-01-01"),
  Endt = endDate0,
  Units = "",
  STCno = "14-10-0036-01",
  Titl = "Hours worked by province by industry",
  Ftnt = paste0("Source: Statistics Canada table 14-10-0036-01. ",
    "Seasonal adjustment by P. Smith."),
  Seas = "Seasonally adjusted",
  Size = 0
)

t09 <- list(
  Num = 9,
  Strt = as.Date("1997-01-01"),
  Endt = 2020,
  Units = "",
  STCno = "99-99-9999-01",
  Titl = "Long-term labour force statistics",
  Ftnt = paste0("Source: Linked statistics from various ",
    "Statistics Canada LFS and other publications. There are statistical breaks in ",
    "these long time series and they should be used with a great deal of caution."),
  Seas = "",
  Size = 0
)

t10 <- list(
  Num = 10,
  Strt = as.Date("1997-01-01"),
  Endt = endDate0,
  Units = "",
  STCno = "14-10-0063-01",
  Titl = "Employee wages by industry",
  Ftnt = paste0("Source: Statistics Canada table 14-10-0063-01."),
  Seas = "Not seasonally adjusted",
  Size = 2548
)

t11 <- list(
  Num = 11,
  Strt = as.Date("1976-01-01"),
  Endt = endDate0,
  Units = "",
  STCno = "14-10-0342-01",
  Titl = "Duration of unemployment",
  Ftnt = paste0("Source: Statistics Canada table 14-10-0342-01."),
  Seas = "Seasonally adjusted",
  Size = 1 # unknown
)

TS <- list(t01,t02,t03,t04,t05,t06,t07,t08,t09,t10,t11)

tord <- c(1,2,3,4,5,6,7,8,10,11,9) # the order of the tables
numTabs <- length(tord) # Number of tables available in the app
tordR <- rep(NA,numTabs)
for (i in 1:numTabs) {
  for (j in 1:numTabs) {
    if (tord[i]==j) tordR[j] <- i
  }
}
tbl <- character()
tabn <- numeric()
for (i in 1:length(tord)) {
  tbl[i] <- TS[[tord[i]]]$Titl # a vector of table names
  tabn[i] <- TS[[tord[i]]]$Num # a vector of table numbers
}
tn <- setNames(tabn,tbl) # a vector of named table numbers

