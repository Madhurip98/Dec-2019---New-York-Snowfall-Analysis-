dirdata <- "C:/Documents/MY R/PorankiMadhuri_mp1286_ProjectR.R/";
dirOUT <- "C:/Documents/MY R/PorankiMadhuri_mp1286_ProjectR_output.txt/";
sink("PorankiMadhuri_mp1286_ProjectR_Output.txt");

#Data cleaning of Rochester snowfall dataset#
###q1###
cat("q1\n");
###q1a###
cat("q1a\n");
library(dplyr)
#Here nrows = N, where N is any sample value
snow1<-read.csv("Pr1SnowFall1.csv", skip=2,nrows=200,as.is=TRUE,na.strings=c(""," ","NA"))
snow1$Year<-substring(snow1$Season,1,4)
snow1 <- snow1 %>% select(Year, everything())
snow1<-snow1[-1,-length(names(snow1))]
snow1<-na.omit(snow1)
snow1[snow1=="T"]<-0
str(snow1)
print(filter(snow1, Year != "MAX",Year != "MIN" ,Year != "Mont" ))

###q1b###
cat("q1b\n");
sprintf("%d full years of snowfall data", nrow(snow1) )

###q1c###
cat("q1c\n");
library(dplyr)
#Here nrows = N, where N is any sample value
snow2<-read.csv("Pr1SnowFall2.csv", skip=2,nrows=200,as.is=TRUE,na.strings=c(""," ","NA"))
snow2$Year<-substring(snow2$Season,1,4)
snow2 <- snow2 %>% select(Year, everything())
snow2<-snow2[-1,-length(names(snow2))]
snow2<-na.omit(snow2)
snow2[snow2=="T"]<-0
str(snow2)
print(filter(snow2, Year != "MAX",Year != "MIN" ,Year != "Mont" ))

#Data cleaning and data augmentation for Buffalo snowfall dataset#
      
###q3###
cat("q3\n");
###q3a###
cat("q3a\n");

wbuff1<-read.csv("Weather_Buffalo.csv")
wbuff1$STATION<-as.character(wbuff1$STATION)
wbuff1$STATION_NAME<-as.character(wbuff1$STATION_NAME)
wbuff1[wbuff1==-9999] <- NA
library(lubridate)
wbuff1$DATE<-ymd(wbuff1$DATE)
str(wbuff1)

###q3b###
cat("q3b\n");

wbuff11 <- wbuff1[ -c(1:7,10,13,16,19,22) ]

wbuff11<-rowsum(+(wbuff11[-2]==0), group = wbuff11$Missing)

wbuff11<-wbuff11[ -c(2,3), ]

library(data.table)
as.data.table(wbuff11)[, lapply(.SD, function(x) sum(x==0)) , Missing]

library(reshape)
wbuff11 <- melt(wbuff11)
wbuff11<-wbuff11[ -c(2,4,6,8,10,12,14,16,18,20,22), ]

wbuff11 <- wbuff11[ -c(1) ]
colnames(wbuff11) <- c("Variable name", "Total no. of missing days")
###q3c###
cat("q3c\n");

wbuff1<-read.csv("Weather_Buffalo.csv")
wbuff1$STATION<-as.character(wbuff1$STATION)
wbuff1$STATION_NAME<-as.character(wbuff1$STATION_NAME)
wbuff1[wbuff1==-9999] <- NA
library(lubridate)
wbuff1$DATE<-ymd(wbuff1$DATE)

wbuff2 <- wbuff1[ -c(1,8,9,11,12,14,15,17,18,20,21,23,24) ]
wbuff2 <- wbuff2[ -c(2,3,4) ]
colnames(wbuff2) <- c("Station", "Date","MaxSnow","Precip","Snowfall","MeanMaxTemp","MeanMinTemp","MeanTemp")

str(wbuff2)
unique(unlist(wbuff2$Station))


###q3d###
cat("q3d\n");
wbuff3<-wbuff2
wbuff3$Station <- gsub("BUFFALO NY US", "Buffalo City", wbuff3$Station)

wbuff3$Station <- gsub("BUFFALO NIAGARA INTERNATIONAL AIRPORT NY US", "Buffalo Airport", wbuff3$Station)

wbuff3$City<-substring(wbuff3$Station,1,7)

library(stringr)
a1 <- str_split(wbuff3$Station, " ")
a2 <- str_split(wbuff3$City, " ")
wbuff3$Site<-do.call("rbind",Map(function(x,y) paste(x[!grepl(paste(y, collapse="|"), x)], collapse=" "), a1, a2))
wbuff3$City<-as.character(wbuff3$City)
wbuff3$Site<-as.character(wbuff3$Site)

str(wbuff3)

###q3e###
cat("q3e\n");
wbuff4<-wbuff3
wbuff4$MonthN<-month(ymd(wbuff4$Date))
wbuff4$Month<-month.name[wbuff4$MonthN]
wbuff4$Month<-substring(wbuff4$Month,1,3)
wbuff4$Year<-substring(wbuff4$Date,1,4)

library(dplyr)
wbuff4$SnowSeasonLong <- ifelse(wbuff4$MonthN %in% c(10,11,12),wbuff4$Year, wbuff4$SnowSeasonLong==1 )
wbuff4$SnowSeasonLong <- ifelse(wbuff4$MonthN %in% c(1,2,3,4),as.numeric(wbuff4$Year)-1 , wbuff4$SnowSeasonLong )

str(wbuff4)
options(max.print=290)
print(wbuff4, nrows=20)

###q3f###
cat("q3f\n");
wbuff5<-wbuff4
library(measurements)
wbuff5$MaxSnow<-as.integer(wbuff5$MaxSnow)
wbuff5$MaxSnow<-conv_unit(wbuff5$MaxSnow,"mm","inch")
wbuff5$Snowfall<-conv_unit(wbuff5$Snowfall,"mm","inch")
wbuff5$Precip<-conv_unit(wbuff5$Precip,"mm","inch")
library(plyr)
wbuff5$MaxSnow<-round_any(wbuff5$MaxSnow,0.1)
wbuff5$Snowfall<-round_any(wbuff5$Snowfall,0.1)
wbuff5$Precip<-round_any(wbuff5$Precip,0.1)

wbuff5$MeanMaxTemp<-conv_unit(wbuff5$MeanMaxTemp,"C","F")
wbuff5$MeanMinTemp<-conv_unit(wbuff5$MeanMinTemp,"C","F")
wbuff5$MeanTemp<-conv_unit(wbuff5$MeanTemp,"C","F")

wbuff5$MeanMaxTemp<-round_any(wbuff5$MeanMaxTemp,0.1)
wbuff5$MeanMinTemp<-round_any(wbuff5$MeanMinTemp,0.1)
wbuff5$MeanTemp<-round_any(wbuff5$MeanTemp,0.1)

str(wbuff5)


###q4###
cat("q4\n");
###q4a###
cat("q4a\n");

wbuff6<-wbuff5[c(2,4,5,10)]

wbuff7<-wbuff6[c(1:533),]
colnames(wbuff7) <- c("Date", "Precip.City","Snowfall.City","Site.x")
wbuff8<-wbuff6[c(534:1438),]
colnames(wbuff8) <- c("Date", "Precip.Air","Snowfall.Air","Site.y")

wbuff9<-merge(wbuff8,wbuff7, by  = "Date")
wbuff9<-wbuff9[-c(4,7)]

###q4b###
cat("q4b\n");

library(dplyr)
wbuff<-(wbuff5 %>%
  arrange(Date, is.na(MaxSnow)) %>% 
  distinct(Date, .keep_all = TRUE))

str(wbuff)


#merging of Rochester, Buffalo, Syracuse and Albanydata sets and keeping common columns#
###q5###
cat("q5\n");
###q5a###
cat("q5a\n");
wbuff$Year<-as.numeric(wbuff$Year)
wbuff$SnowSeasonLong<-as.numeric(wbuff$SnowSeasonLong)

load("~/MY R/Pr2RSA.Rdata")
library(dplyr)

w<-inner_join(wbuff, wRoch, by = c("Date", "Year","Month","MonthN","SnowSeasonLong"))
w<-w[-c(1,3,4,6:10,15,16,17,19:23)]
w<-w[,c(1,6,5,4,3,2,7)]
colnames(w)[6] <- "Snowfall.B"
colnames(w)[7] <- "Snowfall.R"

library(dplyr)

w1<-inner_join(wSyr, wAlb, by = c("Date", "Year","Month","MonthN","SnowSeasonLong"))

w1<-w1[-c(1,3,4,6:10,15,16,17,19:23)]
w1<-w1[,c(1,3,4,5,6,2,7)]
colnames(w1)[6] <- "Snowfall.S"
colnames(w1)[7] <- "Snowfall.A"

library(dplyr)

wAll<-inner_join(w, w1, by = c("Date", "Year","Month","MonthN","SnowSeasonLong"))

str(wAll)
options(max.print=60)
print(wAll,1:6)

###q6b###
cat("q6b\n");

as.numeric(as.character(wAll$Snowfall.R))
wAll1<-wAll [-c(1,3,4,5)]
wAllYearSumsSL  <- aggregate(. ~  SnowSeasonLong, wAll1, sum)
wAllYearSumsSL<-wAllYearSumsSL[-c(1),]

wAll2<-wAllYearSumsSL[-c(1)]

FUN<-  function(x) c( "mean"= mean(x,na.rm=TRUE),"std" = sd(x),  "CV" = sd(x)/mean(x,na.rm=TRUE)
)

sapply(wAll2, FUN)
library(plyr)
round_any(sapply(wAll2, FUN),0.1)

str(wAllYearSumsSL)

#plotting and comparision of snowfalls in 4 cities#
###q6###
cat("q6\n");
###q6a###
cat("q6a\n");

winds(2,2,7)
par(mfrow = c(2, 2))


p1<-plot(wAllYearSumsSL$SnowSeasonLong,wAllYearSumsSL$Snowfall.B,pch = 16, cex = 1.3,main="For Snowfall at Buffalo",ylab="Buffalo, Snowfall (in)",xlab="Season", type="p",col="black")

abline(h = 50,  col = "gray75")
abline(h = 100,  col = "gray75")
abline(h = 150,  col = "gray75")
abline(v = 1943,  col = "lightblue")

p2<-plot(wAllYearSumsSL$SnowSeasonLong,wAllYearSumsSL$Snowfall.R,pch = 16, cex = 1.3,main="For Snowfall at Rochester",ylab="Rochester, Snowfall (in)",xlab="Season", type="p",col="black",ylim=c(0,150))

abline(h = 50,  col = "gray75")
abline(h = 100,  col = "gray75")
abline(h = 150,  col = "gray75")

p3<-plot(wAllYearSumsSL$SnowSeasonLong,wAllYearSumsSL$Snowfall.S,pch = 16, cex = 1.3,main="For Snowfall at Syracuse",ylab="Syracuse, Snowfall (in)",xlab="Season", type="p",col="black",ylim=c(0,150))

abline(h = 50,  col = "gray75")
abline(h = 100,  col = "gray75")
abline(h = 150,  col = "gray75")
abline(v = 1940,  col = "lightblue")

p4<-plot(wAllYearSumsSL$SnowSeasonLong,wAllYearSumsSL$Snowfall.A,pch = 16, cex = 1.3,main="For Snowfall at Albany",ylab="Albany, Snowfall (in)",xlab="Season", type="p",col="black",ylim=c(0,150))

abline(h = 50,  col = "gray75")
abline(h = 100,  col = "gray75")
abline(h = 150,  col = "gray75")
abline(v = 1938,  col = "lightblue")

###q6b###
cat("q6b\n");

winds(2,2,7)

par(mfrow = c(2, 2))
  
plot(wAllYearSumsSL$Snowfall.R,wAllYearSumsSL$Snowfall.B,pch = 16, cex = 1.3,main="Buffalo vs Rochester",ylab="Buffalo, Snowfall (in)",xlab="Rochester,Snowfall (in)", type="p",col=ifelse (wAllYearSumsSL$Snowfall.B[1:19],'red','black'),ylim=c(0,150),xlim=c(0,150))

abline(lm(Snowfall.B ~ Snowfall.R,data=wAllYearSumsSL),col='grey75')

plot(wAllYearSumsSL$Snowfall.R,wAllYearSumsSL$Snowfall.S,pch = 16, cex = 1.3,main="Syracuse vs Rochester",ylab="Syracuse, Snowfall (in)",xlab="Rochester,Snowfall (in)", type="p",col=ifelse (wAllYearSumsSL$Snowfall.S[1:16],'red','black') ,ylim=c(0,150),xlim=c(0,150))

abline(lm(Snowfall.S ~ Snowfall.R,data=wAllYearSumsSL),col='grey75')

plot(wAllYearSumsSL$Snowfall.R,wAllYearSumsSL$Snowfall.A,pch = 16, cex = 1.3,main="Albany vs Rochester",ylab="Albany, Snowfall (in)",xlab="Rochester,Snowfall (in)", type="p",col='black'  ,ylim=c(0,150),xlim=c(0,150))

abline(lm(Snowfall.A ~ Snowfall.R,data=wAllYearSumsSL),col='grey75')




sink()