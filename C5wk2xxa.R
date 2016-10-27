##put the script in the working directory first
getwd()

##download and unzip data
url<-"https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"

if(!dir.exists(("./wk2"))) dir.create("./wk2")

if(!file.exists("./wk2/data.zip")) download.file(url, destfile = "./wk2/data.zip")

if(!file.exists("./wk2/activity.csv")) unzip("./wk2/data.zip", exdir="./wk2")

activity<-read.csv("./wk2/activity.csv")

############################################################################
## Question 1: total steps per day

steps_per_day_raw <- as.data.frame.table(tapply(activity$steps, 
                                                  activity$date, sum, na.rm=FALSE))
names(steps_per_day_raw)<-c("date", "total_steps")

library("ggplot2")
qplot(total_steps/1000, data=steps_per_day_raw, xlab="Total Steps per Day (10^3)")

## calculate mean and median steps per day excluding NA and report
mean_step_per_day_raw<-mean(steps_per_day_raw$total_steps, na.rm = TRUE)
median_step_per_day_raw<-median(steps_per_day_raw$total_steps, na.rm = TRUE)

mean_step_per_day_raw
median_step_per_day_raw

########################################################################
## Question 2: mean steps in 5 minutes interval
## calculate mean steps in each 5 minutes interval 
steps_mean_5.min_raw <- as.data.frame.table(
    tapply(activity$steps, activity$interval, mean, na.rm = TRUE))
names(steps_mean_5.min_raw)<-c("time", "average_steps")

library("ggplot2")
g<-ggplot(steps_mean_5.min_raw, aes(time, average_steps))
g+geom_point()+
    labs(x="Time")+
    labs(y="Average Steps")+
    labs(title="Average Steps in 5 min Interval Across All Day")+
    scale_x_discrete(breaks=c(0000, 0200, 0400, 0600, 0800, 1000, 1200,
                              1400, 1600, 1800, 2000, 2200, 2355))
    
                                                                
## find maxium average steps across all day in each 5 minutes intervlas and report the time of it
max_step_5.min_index_raw<-(grep(max(steps_mean_5.min_raw$average_steps), steps_mean_5.min_raw$average_steps))
max_time_raw<-steps_mean_5.min_raw[max_step_5.min_index_raw,1]
max_time_raw

########################################################################
## Question 3: imputing NA
## calculate total missing value in the data set
total_NA<-sum(is.na(activity))
total_NA

colSums(is.na(activity))

activity_rmNA<-activity

## replace NA with the average steps in that time interval which are stored in "steps_mean_5.min_raw" dataset calculated previously
activity_rmNA[is.na(activity_rmNA$steps), "steps"]<-
    steps_mean_5.min_raw[, "average_steps"]
## this code is amazing! it has the same result from codes below
# merge<-merge(activity_rmNA, steps_mean_5.min_raw, by.x="interval", by.y = "time")
# merge[is.na(merge$steps), "steps"]<-merge[is.na(merge$steps), "average_steps"]
# merge<-arrange(merge, date)
# merge<-merge[,1:3]

## calculate total steps per day after imputing NA
steps_per_day_rmNA <- as.data.frame.table(tapply(activity_rmNA$steps, 
                                                activity_rmNA$date, sum))
names(steps_per_day_rmNA)<-c("date", "total_steps")

## calculate mean and median after imputing NA and repost
mean_step_per_day_rmNA<-mean(steps_per_day_rmNA$total_steps)
median_step_per_day_rmNA<-median(steps_per_day_rmNA$total_steps)

mean_step_per_day_rmNA
median_step_per_day_rmNA

## plot histogram chart of both data with or without NA to compare the impact of imputing NA
g1<-qplot(total_steps/1000, data=steps_per_day_raw, 
          xlab=NULL, main="Total Steps per Day with NA")

g2<-qplot(total_steps/1000, data=steps_per_day_rmNA, 
          xlab="Total Steps per Day (10^3)", main = "Total Steps per Day with Imputed NA")

library("Rmisc")
multiplot(g1, g2)
# this function requires Rmisc package

#########################################################################
## Question 4: weekday vs. weekend
library("dplyr")

activity_rmNA$date<-as.Date(activity_rmNA$date)
activity_rmNA_WD<-mutate(activity_rmNA, 
                         weekdays=weekdays(activity_rmNA$date))
# above code generate detailed weekdays ("monday"...)
activity_rmNA_WD<-mutate(activity_rmNA_WD, 
                         weekday=ifelse(
                             activity_rmNA_WD$weekdays=="Saturday"|
                                 activity_rmNA_WD$weekdays=="Sunday",
                             "weekend", "weekday"))
activity_rmNA_WD$weekdays<-NULL
# combine weekdays into "Weekday" and "Weekend"

## calculate mean in each interval/weekday
WDdata<-aggregate(activity_rmNA_WD$steps, by = list(activity_rmNA_WD$interval,
                                                  activity_rmNA_WD$weekday), 
                mean)
names(WDdata)<-c("interval", "weekday", "average_steps")

## plot comparing weekdays vs. weekends
g3<-ggplot(WDdata, aes(interval, average_steps))
g3+geom_line(aes(color=weekday))+
    labs(x="Time")+
    labs(y="Average Steps")+
    labs(title="Average steps in Weekdays vs. Weekends")+
    theme_bw()
    
