##put the script in the working directory first
getwd()

##download and unzip data
url<-"https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip"

if(!file.exists("./data.zip")) download.file(url, destfile = "./data.zip")

if(!file.exists("./summarySCC_PM25.rds")&!file.exists("./Source_Classification_Code.rds")) unzip("./data.zip")

SCC <- readRDS("Source_Classification_Code.rds")
NEI <- readRDS("summarySCC_PM25.rds")

##########################################################################
## Question 1
## create total emission number of each year and store in sum, add name row to the sum for plotting
sum <- as.table(tapply(NEI$Emissions, NEI$year, sum))
names(sum) <- c("1999","2002","2005","2008")

## create png file, then plot a line chart with the total emission number at each year to compare
png(filename = "./plot1.png")
plot(sum, type = "b", pch = 19, cex = 2, main = "Total PM2.5 Emission in 1999, 2002, 2005 & 2008", xlab = "Year", ylab = "Total PM2.5 Emission (ton)",col = "blue")
dev.off()

#########################################################################
## Question 2
## Subset Baltimore data from the dataset, create total emission number from it, then add name row for plotting
sumB <- subset(NEI, NEI$fips == "24510")
sumB1<-as.table(tapply(sumB$Emissions, sumB$year, sum))
names(sumB1)<-c("1999", "2002", "2005", "2008")

## create png file, then plot a line chart to show the trend of total emission over years
png(filename = "./plot2.png")
plot(sumB1, type = "b", cex = 2, pch = 19, main = "Total PM2.5 Emission in Baltimore in 1999, 2002, 2005 & 2008", xlab = "Year", ylab = "Total PM2.5 Emission (ton)", col = "blue")
dev.off()

##########################################################################
## Question 3
## subset the four types of sources from the subset of Baltimore data
sumB <- subset(NEI, NEI$fips=="24510")
sumP <- subset(sumB, sumB$type == "POINT")
sumNP <- subset(sumB, sumB$type=="NONPOINT")
sumO <- subset(sumB, sumB$type=="ON-ROAD")
sumNO <- subset(sumB, sumB$type == "NON-ROAD")

## create total emission number by years in each source type
sumP1 <- as.data.frame.table(tapply(sumP$Emissions, sumP$year, sum))
sumNP1 <- as.data.frame.table(tapply(sumNP$Emissions, sumNP$year, sum))
sumO1 <- as.data.frame.table(tapply(sumO$Emissions, sumO$year, sum))
sumNO1 <- as.data.frame.table(tapply(sumNO$Emissions, sumNO$year, sum))

## combine the 4 table into 1
sumA<-rbind(sumP1, sumNP1, sumO1, sumNO1)

## add source type column to the combined table
sum3<-cbind(sumA, c(rep("POINT", times = 4), rep("NONPOINT", times = 4), rep("ON-ROAD", times = 4), rep("NON-ROAD", times = 4)))

## name the variables
names(sum3)<-c("Year", "Emission", "Source")

library("ggplot2")
#qplot(Year, Emission, data = sum3, geom = "point", facets = .~Source)
## qplot is ok to do this, but qqplot looks fancy

png(filename = "./plot3.png")
g<-ggplot(sum3, aes(Year, Emission))
g+geom_bar(stat = "identity")+facet_grid(.~Source)+labs(y="Total PM2.5 Emission (ton)")+labs(title="Total PM2.5 Emission by Years for Each Data Source")
dev.off()

#########################################################################
## Question 4

index1 <- grepl("comb", SCC$Short.Name, ignore.case = TRUE)
index2 <- grepl("coal$", SCC$SCC.Level.Three, ignore.case = TRUE)
index <- (index1&index2)

coal_combustionSCC <- SCC[index, ]$SCC
coal_combustionNEI <- NEI[NEI$SCC %in% coal_combustionSCC, ]

library("ggplot2")
png(filename = "./plot4.png")
g<-ggplot(coal_combustionNEI, aes(year, Emissions/10^5))
g+geom_bar(stat = "identity", width = 1.75)+scale_x_continuous(breaks=c(1999, 2002, 2005, 2008), labels=c("1999", "2002", "2005", "2008"))+labs(y="US Total PM2.5 Emission by Coal Combustion (10^5 tons)")+labs(x="Year")+labs(title="US PM2.5 Emission by Coal Combustion by Years")
dev.off()
## http://www.cookbook-r.com/Graphs/Axes_(ggplot2)/ for details of parameter in ggplot

#########################################################################
## Question 5
## subset the four types of sources from the subset of Baltimore data
sumB <- subset(NEI, NEI$fips=="24510")
indexM <-grepl("motorcycle", SCC$SCC.Level.Three, ignore.case = TRUE)
## motor vehicle in SCC is solvent for motor vehicle manufacturing, I think the question require data from motor vehicles on the road, which is motorcycles in the SCC
MotorVSCC<- SCC[indexM, ]$SCC
MotorVNEI<- sumB[sumB$SCC %in% MotorVSCC, ]

library("ggplot2")

png(filename = "./plot5.png")
g<-ggplot(MotorVNEI, aes(year, Emissions))
g+geom_bar(stat = "identity", width = 1.75)+scale_x_continuous(breaks=c(1999,2002,2005,2008))+labs(x="Year")+labs(y="Total Emission by Motor Vehicles (ton)")+labs(title="PM2.5 Emission by Motor Vehicles in Baltimore City from 1999-2008")
dev.off()

#########################################################################
## Question 6
sumB <-subset(NEI, NEI$fips=="24510") #Baltimore data
sumA <-subset(NEI, NEI$fips=="06037") #Los Angeles data
indexM <-grepl("motorcycle", SCC$SCC.Level.Three, ignore.case = TRUE)
## motor vehicle in SCC is solvent for motor vehicle manufacturing, I think the question require data from motor vehicles on the road, which is motorcycles in the SCC
MotorVSCC<- SCC[indexM, ]$SCC
MotorVNEI2<-rbind(sumB[sumB$SCC %in% MotorVSCC, ], sumA[sumA$SCC %in% MotorVSCC, ])

library("ggplot2")

png(filename = "./plot6.png")
g<-ggplot(MotorVNEI2, aes(year, Emissions))
labels<-c("24510" = "Baltimore City", "06037" = "Los Angeles County")
g+geom_bar(stat = "identity")+geom_smooth(method = "lm", col = "red")+scale_x_continuous(breaks=c(1999,2002,2005,2008))+facet_grid(.~fips, labeller = labeller(fips=labels))+labs(x="Year")+labs(y="PM2.5 Emission by Motor Vehicles (ton)")+labs(title="PM2.5 Emissions by Motor Vehicles in Baltimore and Los Angeles")
dev.off()
## http://www.cookbook-r.com/Graphs/Facets_(ggplot2)/