

#Download & Import
setwd("~/GitHub/RepData_PeerAssessment2")


data_raw <- read.csv(bzfile("repdata-data-StormData.csv.bz2"))

#Load Libraries
library(car)
#Summerize Data
unique(data.raw$PROPDMG)
unique(data.raw$PROPDMGEXP)
unique(data.raw$EVTYPE)

#recode
SchoolData$Grade<-
  recode(SchoolData$Grade,
         "c(1,2,3,4,5) ='Five or Less'")

SchoolData$Grade<-
  recode(SchoolData$Grade,"
    1:5='Elementary';
    6:8='Middle;
    else='High'
  ")

recodetest = as.data.frame(unique(data.raw$PROPDMGEXP))
colnames(recodetest) = c("PROPDMGEXP")

data.raw$modPROPDMGEXP = as.numeric(
  recode(data.raw$PROPDMGEXP, "
    c('h','H') = 1e+02;
    c('k','K') = 1e+03;
    c('m','M') = 1e+06;
    c('b','B') = 1e+09;
    '0' = 1; 
    '1' = 10; 
    '2' = 1e+02; 
    '3' = 1e+03; 
    '4' = 1e+04; 
    '5' = 1e+05;
    '6' = 1e+06; 
    '7' = 1e+07; 
    '8' = 1e+08;
    '-' = 0;
    '?' = 0;
    '+' = 0;
    '' = 0;
    ", as.factor.result = FALSE
  )
)



recodetest$modPROPDMGEXP = as.numeric(
  recode(recodetest$PROPDMGEXP, "
    c('h','H') = 1e+02;
  ", as.factor.result = FALSE
  )
)


data.raw$mod2PROPDMGEXP = as.numeric(
  recode(data.raw$PROPDMGEXP, "
 '0' = 1; 
 '1' = 10; 
 '2' = 1e+02; 
 '3'= 1e+03; 
 '4'= 1e+04; 
 '5' = 1e+05; 
 '6' = 1e+06; 
 '7' = 1e+07; 
 '8' = 1e+08; 
 'B' = 1e+09; 
 'h' = 1e+02; 
 'H' = 1e+02; 
 'K' = 1e+03; 
 'm' = 1e+06; 
 'M' = 1e+06; 
 '-' = 0; 
 '?' = 0; 
 '+' = 0; 
 '' = 0;
 ", as.factor.result = FALSE)
)

levels(dt$CROPDMGEXP)

