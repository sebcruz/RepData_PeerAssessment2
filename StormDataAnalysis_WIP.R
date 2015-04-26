

#Download & Import
setwd("~/GitHub/RepData_PeerAssessment2")


data_raw <- read.csv(bzfile("repdata-data-StormData.csv.bz2"))

#Load Libraries
library(car)
#Summerize Data
unique(data.raw$PROPDMG)
unique(data.raw$CROPDMGEXP)
unique(data.raw$EVTYPE)

#recode

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
    c('-','?','+','') = 0;
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
    ", as.factor.result = FALSE
  )
)

head(data.analysis[data.analysis$PROPDMGEXP == '4',])

#Summerize
library(dplyr)

data.analysis.health.sum = 
  data.analysis.health %>%
  group_by(EVTYPE) %>%
  summarise_each(funs(sum)) %>%
  arrange(desc(TOTALDEATHINJ))

#Question 1
data.analysis.health.sum[1:10,4]


AllUp.DeathInj = sum(data.analysis.health.sum$TOTALDEATHINJ)
Top10.DeathInj = sum(data.analysis.health.sum[1:10,4])
PrctTop10.DeathInj = Top10.DeathInj/AllUp.DeathInj
TopEVTYPE.DeathInj = sum(data.analysis.health.sum[1,4])


ag.mtc<-aggregate(data.analysis.health$TOTALDEATHINJ, by=list(data.analysis.health$EVTYPE), FUN=sum)

library(ggplot2)

ggplot(data.analysis.health.sum[1:10, ]) +
  geom_bar(aes(reorder(EVTYPE, TOTALDEATHINJ, sum), y=TOTALDEATHINJ), width=0.5, stat="identity") +
  coord_flip() +
  ggtitle("Top 10 Weather Events by Total Deaths & Injuries") +
  ylab("Weather Events") +
  xlab("Total Deaths & Injuries")
)

reorder(EVTYPE, TOTALDEATHINJ, sum)

ggplot(df) + 
  geom_point(aes(reorder(Names, Proportion, mean), y=Proportion)) +
  coord_flip()




ggplot(top.harm, aes(y=Prop.Death.and.Inj)) +
  geom_bar(aes(x=harm.order),
           stat="identity", fill="white", colour="darkgreen", width=0.5) + 
  coord_flip() +
  ggtitle("Top 15 events accounting\n for > 90% of Total Harm") +
  ylab("Proportion of Total Deaths and Injuries") +
  xlab("") +
  theme(axis.text=element_text(size=rel(3)),
        axis.title=element_text(size=rel(2.25)),
        plot.title = element_text(size = rel(2.75)))

