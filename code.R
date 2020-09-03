library(dplyr)
library(ggplot2)
fileUrl <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
download.file(fileUrl, destfile = "~/Desktop/R_Files/Storm_Analysis/storm.csv", method = "curl")
##check if the file is downloaded
list.files("~/Desktop/R_Files/Storm_Analysis")
##collect the infomation on when the file is downloaded
dateDownloaded <- date()

all_data <- read.csv("storm.csv")
storm <- select(all_data, 'EVTYPE','FATALITIES','INJURIES', 'PROPDMG', 'PROPDMGEXP', 'CROPDMG', 'CROPDMGEXP')

storm$PROPEXP[storm$PROPDMGEXP == "K"] <- 1000
storm$PROPEXP[storm$PROPDMGEXP == "M"] <- 1e+06
storm$PROPEXP[storm$PROPDMGEXP == ""] <- 1
storm$PROPEXP[storm$PROPDMGEXP == "B"] <- 1e+09
storm$PROPEXP[storm$PROPDMGEXP == "m"] <- 1e+06
storm$PROPEXP[storm$PROPDMGEXP == "0"] <- 1
storm$PROPEXP[storm$PROPDMGEXP == "5"] <- 1e+05
storm$PROPEXP[storm$PROPDMGEXP == "6"] <- 1e+06
storm$PROPEXP[storm$PROPDMGEXP == "4"] <- 10000
storm$PROPEXP[storm$PROPDMGEXP == "2"] <- 100
storm$PROPEXP[storm$PROPDMGEXP == "3"] <- 1000
storm$PROPEXP[storm$PROPDMGEXP == "h"] <- 100
storm$PROPEXP[storm$PROPDMGEXP == "7"] <- 1e+07
storm$PROPEXP[storm$PROPDMGEXP == "H"] <- 100
storm$PROPEXP[storm$PROPDMGEXP == "1"] <- 10
storm$PROPEXP[storm$PROPDMGEXP == "8"] <- 1e+08

storm$PROPEXP[storm$PROPDMGEXP == "+"] <- 0
storm$PROPEXP[storm$PROPDMGEXP == "-"] <- 0
storm$PROPEXP[storm$PROPDMGEXP == "?"] <- 0

storm$PROPDMGVAL <- storm$PROPDMG * storm$PROPEXP

storm$CROPEXP[storm$CROPDMGEXP == "M"] <- 1e+06
storm$CROPEXP[storm$CROPDMGEXP == "K"] <- 1000
storm$CROPEXP[storm$CROPDMGEXP == "m"] <- 1e+06
storm$CROPEXP[storm$CROPDMGEXP == "B"] <- 1e+09
storm$CROPEXP[storm$CROPDMGEXP == "0"] <- 1
storm$CROPEXP[storm$CROPDMGEXP == "k"] <- 1000
storm$CROPEXP[storm$CROPDMGEXP == "2"] <- 100
storm$CROPEXP[storm$CROPDMGEXP == ""] <- 1
storm$CROPEXP[storm$CROPDMGEXP == "?"] <- 0
storm$CROPDMGVAL <- storm$CROPDMG * storm$CROPEXP


storm_evtype_1 <- aggregate(FATALITIES ~EVTYPE, data = storm, FUN = sum ) 
fatalities <- arrange(storm_evtype_1, desc(FATALITIES))
top_10_1 <- fatalities[1:10,]
top_10_1$EVTYPE <- factor(top_10_1$EVTYPE, levels = fatalities$EVTYPE)

ggplot(top_10_1, aes(x = EVTYPE, y = FATALITIES)) + 
        geom_bar(stat = "identity") + 
        theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
        xlab("Event Type") + ylab("Fatalities") + ggtitle("Number of fatalities by top 10 Weather Events")

storm_evtype_2 <- aggregate(INJURIES ~EVTYPE, data = storm, FUN = sum ) 
injuries <- arrange(storm_evtype_2, desc(INJURIES))
top_10_2 <- injuries[1:10,]
top_10_2$EVTYPE <- factor(top_10_2$EVTYPE, levels = injuries$EVTYPE)

ggplot(top_10_2, aes(x = EVTYPE, y = INJURIES)) + 
        geom_bar(stat = "identity") + 
        theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
        xlab("Event Type") + ylab("Injuries") + ggtitle("Number of injuries by top 10 Weather Events")

storm_evtype_3 <- aggregate(PROPDMG ~EVTYPE, data = storm, FUN = sum )
property<- arrange(storm_evtype_3, desc(PROPDMG))
top_10_3 <- property[1:10,]
top_10_3$EVTYPE <- factor(top_10_3$EVTYPE, levels = property$EVTYPE)

ggplot(top_10_3, aes(x = EVTYPE, y = PROPDMG)) + 
        geom_bar(stat = "identity") + 
        theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
        xlab("Event Type") + ylab("Property Damage") + ggtitle("Property damage by top 10 Weather Events")

storm_evtype_4 <- aggregate(CROPDMG ~EVTYPE, data = storm, FUN = sum )
crop<- arrange(storm_evtype_4, desc(CROPDMG))
top_10_4 <- crop[1:10,]
top_10_4$EVTYPE <- factor(top_10_4$EVTYPE, levels = crop$EVTYPE)

ggplot(top_10_4, aes(x = EVTYPE, y = CROPDMG )) + 
        geom_bar(stat = "identity") + 
        theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
        xlab("Event Type") + ylab("Crop Damage") + ggtitle("Crop damage by top 10 Weather Events")



