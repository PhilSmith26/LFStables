# Common specs and functions

source("Tabl_specs.R")

IDX <- function(x) {y <- round(100*x/x[1],1)}
PC <- function(x) {y <- round(100*(x/lag(x)-1),1)}
PC12 <- function(x) {y <- round(100*(x/lag(x,12)-1),1)}
MA5 <- function(x) {
  y <- round((lag(x,2)+lag(x,1)+x+lead(x,1)+lead(x,2))/5,1)
  n <- length(x)
  y[1] <- x[1]; 
  y[2] <- (x[1]+x[2]+x[3])/3
  y[n] <- x[n]
  y[n-1] <- (x[n-2]+x[n-1]+x[n])/3
  return(y)
}
MA13 <- function(x) {
  n <- length(x)
  y <- round((lag(x,6)+lag(x,5)+lag(x,4)+lag(x,3)+lag(x,2)+lag(x,1)+x+
      lead(x,1)+lead(x,2)+lead(x,3)+lead(x,4)+lead(x,5)+lead(x,6))/13,1)
  y[1] <- round((7*x[1]+x[2]+x[3]+x[4]+x[5]+x[6]+x[7])/13,1)
  y[2] <- round((x[1]+6*x[2]+x[3]+x[4]+x[5]+x[6]+x[7]+x[8])/13,1)
  y[3] <- round((x[1]+x[2]+5*x[3]+x[4]+x[5]+x[6]+x[7]+x[8]+x[9])/13,1)
  y[4] <- round((x[1]+x[2]+x[3]+4*x[4]+x[5]+x[6]+x[7]+x[8]+x[9]+x[10])/13,1)
  y[5] <- round((x[1]+x[2]+x[3]+x[4]+3*x[5]+x[6]+x[7]+x[8]+x[9]+x[10]+x[11])/13,1)
  y[6] <- round((x[1]+x[2]+x[3]+x[4]+x[5]+2*x[6]+x[7]+x[8]+x[9]+x[10]+x[11]+x[12])/13,1)
  y[n-6] <- round((x[n-12]+x[n-11]+x[n-10]+x[n-9]+x[n-8]+x[n-7]+x[n-6]+x[n-5]+x[n-4]+x[n-3]+x[n-2]+x[n-1]+x[n])/13,1)
  y[n-5] <- round((x[n-11]+x[n-10]+x[n-9]+x[n-8]+x[n-7]+x[n-6]+x[n-5]+x[n-4]+x[n-3]+x[n-2]+x[n-1]+2*x[n])/13,1)
  y[n-4] <- round((x[n-10]+x[n-9]+x[n-8]+x[n-7]+x[n-6]+x[n-5]+x[n-4]+x[n-3]+x[n-2]+x[n-1]+3*x[n])/13,1)
  y[n-3] <- round((x[n-9]+x[n-8]+x[n-7]+x[n-6]+x[n-5]+x[n-4]+x[n-3]+x[n-2]+x[n-1]+4*x[n])/13,1)
  y[n-2] <- round((x[n-8]+x[n-7]+x[n-6]+x[n-5]+x[n-4]+x[n-3]+x[n-2]+x[n-1]+5*x[n])/13,1)
  y[n-1] <- round((x[n-7]+x[n-6]+x[n-5]+x[n-4]+x[n-3]+x[n-2]+x[n-1]+6*x[n])/13,1)
  y[n] <- round((x[n-6]+x[n-5]+x[n-4]+x[n-3]+x[n-2]+x[n-1]+7*x[n])/13,1)
  return(y)
}
posNeg <- function(x) {sum(x>0,na.rm=TRUE)>0 & sum(x>0,na.rm=TRUE)<length(x)}
datDif <- function(month1,month2) {
  y1 <- year(month1)
  m1 <- month(month1)
  y2 <- year(month2)
  m2 <- month(month2)
  distance <- as.numeric(y2)-as.numeric(y1)
}

# Standard theme for charts
theme_DB <- function(base_size = 11,
  base_family = "",
  base_line_size = base_size / 170,
  base_rect_size = base_size / 170) {
  theme(
    plot.title = element_text(colour="black",size=14,face="bold",hjust=0),
    plot.subtitle = element_text(colour="black",size=14,hjust=0),
    panel.background = element_rect(fill="aliceblue",colour="black"),
    panel.border = element_rect(fill=NA,colour="black"),
    axis.text.x = element_text(angle=45,hjust=1,vjust=0.95,size=10),
    axis.text.y = element_text(size=14),
    complete = TRUE
  )
}

