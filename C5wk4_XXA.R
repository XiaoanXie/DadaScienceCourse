


getwd()

##download data and read into R (read.csv can directly read csv.bz2)
url<-"https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"

if(!dir.exists(("./C5wk4xxa"))) dir.create("./C5wk4xxa")

if(!file.exists("./C5wk4xxa/StormData.csv.bz2")) download.file(url, destfile = "./C5wk4xxa/StormData.csv.bz2")

library("dplyr"); library("ggplot2"); library("Rmisc")

StormData<-read.csv("./C5wk4xxa/StormData.csv.bz2")
#StormData1<-StormData

# thought to clean the raw data:
# 1. Subset only needed columns for that two questions, which are: BGN_DATE, STATE,
#   EVTYPE, FATALITIES, INJURIES, PROPDMG, PROPDMGEXP, CROPDMG, CROPDMGEXP, REMARKS
# 2. Extract Year from BGN_DATE which contains both YMD and time, we only need
#   the year that an event happened
# 3. Merge same events in EVTYPE, 
#   - TSTM WIND, THUNDERSTORM WIND, THUNDERSTORM WINDS, TSTM WIND/HAIL, THUNDERSTORM WINDS HAIL, THUNDERSTORM WINDSS, THUNDERSTORM, TSTM WIND (G45) into THUNDERSTORM
#   - MARINE TSTM WIND, MARINE THUNDERSTORM WIND in MARINE THUNDERSTORM
#   - WILD/FOREST FIRE, WILD FIRE into WILD/FOREST FIRE
#   - FLASH FLOOD, FLASH FLOODING, FLOOD, FLOOD/FLASH FLOOD, URBAN FLOOD, RIVER FLOOD, COASTAL FLOODING, FLOODING, URBAN FLOODING, COASTAL FLOOD, Coastal Flooding, URBAN/SML STREAM FLD into FLOOD
#   - WINDS, WIND into WIND
#   - WATERSPOUT, WATERSPOUTS into WATERSPOUT
# 4. Tornados may be counted seperately if it crossed counties or lifted up
#   above 5 minutes, I just keep the original record in this research, 
#   didn't do any merging
# 5. Combine PROPDMG and PROPDMGEXP to one column using the same unit, similarlly
#   for CROPDMG.

Subset<-subset(StormData, select = c(BGN_DATE, STATE, EVTYPE, FATALITIES,
                                   INJURIES, PROPDMG, PROPDMGEXP, CROPDMG, 
                                   CROPDMGEXP, REMARKS))

## convert date/time format to date only
Subset$BGN_DATE<-strptime(Subset$BGN_DATE, format = "%m/%d/%Y %H:%M:%S")

png(filename = "./C5wk4xxa/plot1.png")
p<-ggplot(Subset, aes(BGN_DATE, FATALITIES))
p+geom_point(aes(color=EVTYPE))+
    scale_x_discrete(breaks = c("1950", "1960", "1970", "1980", 
                                "1990", "2000", "2010"))+
    theme(legend.position="bottom")
dev.off()


summary(StormData$FATALITIES)
# too much event types, makes ploting very difficult
# extract those sevious event

Data_fatalities<-filter(StormData, FATALITIES > 10)


png(filename = "./C5wk4xxa/plot1.png")
p_event_death<-ggplot(Data_fatalities, aes(BGN_DATE, FATALITIES))
p_event_death+geom_point(aes(color=EVTYPE))
dev.off()






