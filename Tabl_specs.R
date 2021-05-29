# Tabl_specs.R
# Contains the specifications or metadata for the LFS tables
# May 26, 2021

# NOTE: The data for tables are stored and must be updated once per month.

tcol <- "#E3ECF6"

t01 <- list(
  Num = 1,
  Strt = as.Date("1976-01-01"),
  Endt = as.Date("2021-04-01"),
  STCno = "14-10-0287-01",
  Units = "Thousands of persons",
  Titl = "Labour force characteristics",
  Ftnt = "Source: Statistics Canada table 14-10-0287-01.",
  Seas = "Seasonally adjusted"
)

t02 <- list(
  Num = 2,
  Strt = as.Date("1987-01-01"),
  Endt = as.Date("2021-04-01"),
  Units = "Thousands of persons",
  STCno = "14-10-0291-01",
  Titl = "Labour force characteristics by industry",
  Ftnt = "Source: Statistics Canada table 14-10-0291-01.",
  Seas = "Seasonally adjusted"
)

t03 <- list(
  Num = 3,
  Strt = as.Date("1987-01-01"),
  Endt = as.Date("2021-04-01"),
  Units = "Thousands of hours",
  STCno = "14-10-0289-01",
  Titl = "Actual hours worked at main job by industry",
  Ftnt = "Source: Statistics Canada table 14-10-0289-01.",
  Seas = "Seasonally adjusted"
)

t04 <- list(
  Num = 4,
  Strt = as.Date("1987-01-01"),
  Endt = as.Date("2021-04-01"),
  Units = "Thousands of persons",
  STCno = "14-10-0296-01",
  Titl = "Labour force characteristics by occupation",
  Ftnt = "Source: Statistics Canada table 14-10-0296-01.",
  Seas = "Not seasonally adjusted"
)

t05 <- list(
  Num = 5,
  Strt = as.Date("1997-01-01"),
  Endt = as.Date("2021-04-01"),
  Units = "Thousands of persons",
  STCno = "14-10-0317-01",
  Titl = "Hourly wage distributions by occupation",
  Ftnt = "Source: Statistics Canada table 14-10-0317-01.",
  Seas = "Not seasonally adjusted"
)

t06 <- list(
  Num = 6,
  Strt = as.Date("2001-01-01"),
  Endt = as.Date("2021-04-01"),
  Units = "Thousands of persons or Persons or dollars",
  STCno = "14-10-0320-01",
  Titl = "Average usual hours and wages by selected characteristics",
  Ftnt = "Source: Statistics Canada table 14-10-0320-01.",
  Seas = "Not seasonally adjusted"
)

TS <- list(t01,t02,t03,t04,t05,t06)

tord <- c(1,2,3,4,5,6) # the order of the tables
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

