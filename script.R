storm_data <- read.csv("repdata_data_Stormdata.csv", sep = ",", header = TRUE)
str(storm_data)
head(storm_data)
## Grabbing only the necessary and relevant data
storm_data1 <- storm_data[, c("EVTYPE", "FATALITIES", "INJURIES", "PROPDMG", "PROPDMGEXP", "CROPDMG", "CROPDMGEXP")]

## Creating new coloumns of the actual numbers for exponents of prop dmg and crop dmg
storm_data1$REALPROP = 0
storm_data1[storm_data1$PROPDMGEXP == "H", ]$REALPROP = storm_data1[storm_data1$PROPDMGEXP == "H", ]$PROPDMG * 100
storm_data1[storm_data1$PROPDMGEXP == "K", ]$REALPROP = storm_data1[storm_data1$PROPDMGEXP == "K", ]$PROPDMG * 1000
storm_data1[storm_data1$PROPDMGEXP == "M", ]$REALPROP = storm_data1[storm_data1$PROPDMGEXP == "M", ]$PROPDMG * 10^6
storm_data1[storm_data1$PROPDMGEXP == "B", ]$REALPROP = storm_data1[storm_data1$PROPDMGEXP == "B", ]$PROPDMG * 10^9

storm_data1$REALCROP = 0
storm_data1[storm_data1$CROPDMGEXP == "H", ]$REALCROP = storm_data1[storm_data1$CROPDMGEXP == "H", ]$CROPDMG * 100
storm_data1[storm_data1$CROPDMGEXP == "K", ]$REALCROP = storm_data1[storm_data1$CROPDMGEXP == "K", ]$CROPDMG * 1000
storm_data1[storm_data1$CROPDMGEXP == "M", ]$REALCROP = storm_data1[storm_data1$CROPDMGEXP == "M", ]$CROPDMG * 10^6
storm_data1[storm_data1$CROPDMGEXP == "B", ]$REALCROP = storm_data1[storm_data1$CROPDMGEXP == "B", ]$CROPDMG * 10^9

## Finding the fatalities by event type
fatalities <- aggregate(FATALITIES ~ EVTYPE, data = storm_data1, sum)
fatalities <- fatalities[order(fatalities$FATALITIES, decreasing = TRUE), ][1:15, ] ## In decreasing order by top 15 events
g <- ggplot(fatalities, aes(x = reorder(EVTYPE, -FATALITIES), y = FATALITIES, fill = EVTYPE))
g + geom_bar(stat = "identity") + theme(axis.text.x = element_text(angle = 90)) + xlab("Weather Event") + ylab("Fatalities") + ggtitle("Number of Fatalities by Top 15 Weather Events")

## Finding the injuries by event type
injuries <- aggregate(INJURIES ~ EVTYPE, data = storm_data1, sum)
injuries <- injuries[order(injuries$INJURIES, decreasing = TRUE), ][1:15, ]
g1 <- ggplot(injuries, aes(x = reorder(EVTYPE, -INJURIES), y = INJURIES, fill = EVTYPE))
g1 + geom_bar(stat = "identity") + theme(axis.text.x = element_text(angle = 90)) + xlab("Weather Event") + ylab("Injuries") + ggtitle("Number of Injuries by Top 15 Weather Events")

## Finding property and crop damages by event type
propcrop <- aggregate(REALPROP+REALCROP ~ EVTYPE, data = storm_data1, sum)
names(propcrop) <- c("EVTYPE", "TOTALDMG")
propcrop <- propcrop[order(propcrop$TOTALDMG, decreasing = TRUE), ][1:15, ]
g2 <- ggplot(propcrop, aes(x = reorder(EVTYPE, -TOTALDMG), y = TOTALDMG, fill = EVTYPE))
g2 + geom_bar(stat = "identity") + theme(axis.text.x = element_text(angle = 90)) + xlab("Weather Event") + ylab("Property + Crop Damage Costs") + ggtitle("Property and Damage Costs by Top 15 Weather Events")

