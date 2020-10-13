#Read data and transform.

#Packages
#install.packages("ggplot2")
library(ggplot2)


storm.data <- read.csv("repdata-data-StormData.csv.bz2")
storm.data$PROPDMGEXP <- tolower(storm.data$PROPDMGEXP)
storm.data$CROPDMGEXP <- tolower(storm.data$CROPDMGEXP)
remove.list <- c("-", "?", "+", "0", "1", "2", "3", "4", "5", "6", "7", "8")
for(s in remove.list){
        storm.data <- storm.data[!(storm.data$CROPDMGEXP == s),]
}
for(s in remove.list){
        storm.data <- storm.data[!(storm.data$PROPDMGEXP == s),]
}

storm.data$CROPDMGEXP[storm.data$CROPDMGEXP == "h"] <- "100"
storm.data$CROPDMGEXP[storm.data$CROPDMGEXP == "k"] <- "1000"
storm.data$CROPDMGEXP[storm.data$CROPDMGEXP == "m"] <- "1000000"
storm.data$CROPDMGEXP[storm.data$CROPDMGEXP == "b"] <- "1000000000"
storm.data$CROPDMGEXP <- as.numeric(storm.data$CROPDMGEXP)
storm.data$CROPDMGEXP[is.na(storm.data$CROPDMGEXP)] <- 0

storm.data$PROPDMGEXP[storm.data$PROPDMGEXP == "h"] <- "100"
storm.data$PROPDMGEXP[storm.data$PROPDMGEXP == "k"] <- "1000"
storm.data$PROPDMGEXP[storm.data$PROPDMGEXP == "m"] <- "1000000"
storm.data$PROPDMGEXP[storm.data$PROPDMGEXP == "b"] <- "1000000000"
storm.data$PROPDMGEXP <- as.numeric(storm.data$PROPDMGEXP)
storm.data$PROPDMGEXP[is.na(storm.data$PROPDMGEXP)] <- 0

storm.data$crop.dmg.usd <- storm.data$CROPDMG * storm.data$CROPDMGEXP
storm.data$prop.dmg.usd <- storm.data$PROPDMG * storm.data$PROPDMGEXP

event.list <- storm.data$EVTYPE
event.list <- event.list[!duplicated(event.list)]
#sum(is.na(storm.data$EVTYPE))


#Sum of fatalities and injuries by EVTYPE (ph = population health) Winner: Tornado
sum.event.ph <- data.frame(EventType = character(), 
                          FatalitiesTotal = numeric(), InjuriesTotal = numeric())
for(e in event.list) {
      df1 <- subset(storm.data, EVTYPE == e)
      sum1 <- sum(df1$FATALITIES)
      sum2 <- sum(df1$INJURIES)
      df2 <- data.frame(EventType = e, 
                        FatalitiesTotal = sum1, InjuriesTotal = sum2)
      sum.event.ph <- rbind(sum.event.ph, df2)
}

sum.event.ph.f <- sum.event.ph[order(-sum.event.ph$FatalitiesTotal),]#ordered by fatalities
sum.event.ph.f <- sum.event.ph.f[1:5,]
barplot(sum.event.ph.f$FatalitiesTotal, ylim = c(0, 6000), xlab = "Type of Event", 
        names.arg = c("Tornado", "Excessive Heat", "Flash Flood", "Heat", "Lightning"), 
        cex.names = 0.5, ylab = "Total Fatalities", cex.label = 0.5, 
        main = "Total Fatalities by Event Type Between 1950-2011", cex.main = 0.5)

sum.event.ph.i <- sum.event.ph[order(-sum.event.ph$InjuriesTotal),]#ordered by injuries
sum.event.ph.i <- sum.event.ph.i[1:5,]
barplot(sum.event.ph.i$InjuriesTotal, ylim = c(0, 100000), xlab = "Type of Event", 
        names.arg = c("Tornado", "TSTM Wind", "Flood", "Excessive Heat", "Lightning"), 
        cex.names = 0.5, ylab = "Total Injuries", cex.lab = 0.5, 
        main = "Total Injuries by Event Type Between 1950-2011", cex.main = 0.5)

sum.event.ph$CombinedTotal <- sum.event.ph$FatalitiesTotal + sum.event.ph$InjuriesTotal
sum.event.ph.c <- sum.event.ph[order(-sum.event.ph$CombinedTotal),]
sum.event.ph.c <- sum.event.ph.c[1:5,]
barplot(sum.event.ph.c$CombinedTotal, ylim = c(0, 100000), xlab = "Type of Event", 
        names.arg = c("Tornado", "Excessive Heat", "TSTM Wind", "Flood", "Lightning"), 
        cex.names = 0.5, ylab = "Combined Total", cex.lab = 0.5, 
        main = "Total Fatalities and Injuries by Event Type Between 1950-2011", cex.main = 0.5)

#Sum of economic consequences by EVTYPE (ec = economic consequences) Winner:***
#Crop damage - Drought; Property damage - Flood
sum.event.ec <- data.frame(EventType = character(), 
                           CropDMGTotal = numeric(), PropDMGTotal = numeric())
for(e in event.list) {
        df3 <- subset(storm.data, EVTYPE == e)
        sum3 <- sum(df3$crop.dmg.usd)
        sum4 <- sum(df3$prop.dmg.usd)
        df4 <- data.frame(EventType = e, 
                          CropDMGTotal = sum3, PropDMGTotal = sum4)
        sum.event.ec <- rbind(sum.event.ec, df4)
}

sum.event.ec.c <- sum.event.ec[order(-sum.event.ec$CropDMGTotal),]#ordered by crop damage
sum.event.ec.c <- sum.event.ec.c[1:5,]
barplot(sum.event.ec.c$CropDMGTotal, ylim = c(0, 14000000000), xlab = "Type of Event", 
        names.arg = c("Drought", "Flood", "River Flood", "Ice Storm", "Hail"), 
        cex.names = 0.5, ylab = "Total Crop Damage in USD", cex.lab = 0.5, 
        main = "Total Crop Damage by Event Type Between 1950-2011", cex.main = 0.5)


sum.event.ec.p <- sum.event.ec[order(-sum.event.ec$PropDMGTotal),]#ordered by property damage
sum.event.ec.p <- sum.event.ec.p[1:5,]
barplot(sum.event.ec.p$PropDMGTotal, ylim = c(0, 150000000000), xlab = "Type of Event", 
        names.arg = c("Flood", "Hurricane/Tycoon", "Tornado", "Storm Surge", "Flash Flood"), 
        cex.names = 0.5, ylab = "Total Property Damage in USD", cex.lab = 0.5, 
        main = "Total Property Damage by Event Type Between 1950-2011", cex.main = 0.5)


sum.event.ec$CombinedTotal <- sum.event.ec$CropDMGTotal + sum.event.ec$PropDMGTotal
sum.event.ec.b <- sum.event.ec[order(-sum.event.ec$CombinedTotal),]
sum.event.ec.b <- sum.event.ec.b[1:5,]
barplot(sum.event.ec.b$PropDMGTotal, ylim = c(0, 155000000000), xlab = "Type of Event", 
        names.arg = c("Flood", "Hurricane/Tycoon", "Tornado", "Storm Surge", "Hail"), 
        cex.names = 0.5, ylab = "Total Crop and Property Damage in USD", cex.lab = 0.5, 
        main = "Total Crop and Property Damage by Event Type Between 1950-2011", cex.main = 0.5)
