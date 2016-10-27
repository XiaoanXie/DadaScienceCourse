##put the script in the working directory first
getwd()

##create project directory under working directory
if(!file.exists("./wk1xxa")) dir.create("./wk1xxa")

##download and unzip data
url<-"https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip"

if(!file.exists("./wk1xxa/data.zip")) download.file(url, destfile = "./wk1xxa/data.zip")

if(!file.exists("./wk1xxa/household_power_consumption.txt")) unzip("./wk1xxa/data.zip", exdir = "./wk1xxa")

#read txt into R, seperated by ";", "NA" for "?"
wk1data<-read.table("./wk1xxa/household_power_consumption.txt", sep = ";", na.strings = "?", header = TRUE)

wk1data$Date<-as.Date(wk1data$Date, "%d/%m/%Y")

library("dplyr")

#dataf<-filter(wk1data, wk1data$Date == "2007-02-01"|"2007-02-02")
#cause R to be terminated, seperate into two action to avoid

dataf1<-filter(wk1data, wk1data$Date=="2007-02-01")
dataf2<-filter(wk1data, wk1data$Date=="2007-02-02")
dataf<-rbind(dataf1, dataf2)

#remove the huge data to save memory
wk1data<-"NA"

dataf<-mutate(dataf, DateTime=with(dataf, paste(Date, Time)))

#don't know why but this "as.POSIXct" will help the system to know this is date
#and time, otherwise below plotting 2 will report error.
dataf$DateTime<-as.POSIXct(dataf$DateTime)

#************************************************************************
#plot 1
png(filename = "./wk1xxa/plot1.png", width = 480, height = 480, units = "px")

hist(dataf$Global_active_power, xlab = "Global Active Power (kilowatts)", 
     main = "Global Active Power", col = "red")
dev.off()

#************************************************************************
#plot 2
png(filename = "./wk1xxa/plot2.png", width = 480, height = 480, units = "px")

plot(dataf$DateTime, dataf$Global_active_power, type = "l", xlab = "", ylab = "Global Active Power (kilowatts)")
dev.off()

#*************************************************************************
#plot 3
png(filename = "./wk1xxa/plot3.png", width = 480, height = 480, units = "px")

with(dataf, {plot(DateTime, Sub_metering_1, type = "l", xlab = "", ylab = "Energy sub metering", col = "black")
            lines(DateTime, Sub_metering_2, col = "red")
            lines(DateTime, Sub_metering_3, col = "blue")
}
)
legend("topright", lwd = 2, col = c("black", "red", "blue"), legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"))

dev.off()

#*************************************************************************
#plot 4
png(filename = "./wk1xxa/plot4.png", width = 480, height = 480, units = "px")

par(mfrow = c(2,2))
with(dataf, {
  plot(DateTime, Global_active_power, type = "l", xlab="", ylab = "Global Active Power")
  plot(DateTime, Voltage, type = "l", xlab="datetime", ylab = "Voltage")
  plot(DateTime, Sub_metering_1, type = "l", xlab = "", ylab = "Energy sub metering", col = "black")
  lines(DateTime, Sub_metering_2, col = "red")
  lines(DateTime, Sub_metering_3, col = "blue")
  legend("topright", lwd = 2, col = c("black", "red", "blue"), legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"), cex = 1, bty = "n")
  plot(DateTime, Global_reactive_power, type = "l", xlab="datetime", ylab="Global_reactive_power")
})

dev.off()





