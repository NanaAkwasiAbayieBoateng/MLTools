library(scales)

comma_format()(c(1, 1e3, 2000, 1e6))
comma_format(digits = 9)(c(1, 1e3, 2000, 1e6))
comma(c(1, 1e3, 2000, 1e6))

# If you're European you can switch . and , with the more general
# format_format
point <- format_format(big.mark = ".", decimal.mark = ",", scientific = FALSE)
point(c(1, 1e3, 2000, 1e6))
point(c(1, 1.021, 1000.01))

# [1] "1"         "1,000"     "2,000"     "1,000,000"
# [1] "1"         "1,000"     "2,000"     "1,000,000"
# [1] "1"         "1,000"     "2,000"     "1,000,000"
# [1] "1"         "1.000"     "2.000"     "1.000.000"
# [1] "1,000"     "1,021"     "1.000,010"
# 
# 



#Remember it is good coding technique to add additional packages to the top of
# your script 
library(lubridate) # for working with dates
library(ggplot2)  # for creating graphs
library(scales)   # to access breaks/formatting functions
library(gridExtra) # for arranging plots

# set working directory to ensure R can find the file we wish to import
# setwd("working-dir-path-here")

# daily HARV met data, 2009-2011
harMetDaily.09.11 <- read.csv(
  file="NEON-DS-Met-Time-Series/HARV/FisherTower-Met/Met_HARV_Daily_2009_2011.csv",
  stringsAsFactors = FALSE)

# covert date to Date class
harMetDaily.09.11$date <- as.Date(harMetDaily.09.11$date)

# monthly HARV temperature data, 2009-2011
harTemp.monthly.09.11<-read.csv(
  file="NEON-DS-Met-Time-Series/HARV/FisherTower-Met/Temp_HARV_Monthly_09_11.csv",
  stringsAsFactors=FALSE
)

# datetime field is actually just a date 
#str(harTemp.monthly.09.11) 

# convert datetime from chr to date class & rename date for clarification
harTemp.monthly.09.11$date <- as.Date(harTemp.monthly.09.11$datetime)


# plot Air Temperature Data across 2009-2011 using daily data
ggplot(harMetDaily.09.11, aes(date, airt)) +
geom_point(na.rm=TRUE, color="blue", size=3, pch=18)



# Customize A Scatterplot
# 
# We can customize our plot in many ways. For instance, we can change the size and color of the points using size=, shape pch=, and color= in the geom_point element.
# 
# geom_point(na.rm=TRUE, color="blue", size=1)

# plot Air Temperature Data across 2009-2011 using daily data
ggplot(harMetDaily.09.11, aes(date, airt)) +
  geom_point(na.rm=TRUE, color="blue", size=1) + 
  ggtitle("Air Temperature 2009-2011\n NEON Harvard Forest Field Site") +
  xlab("Date") + ylab("Air Temperature (C)")






#Modify Title & Axis Labels

#We can modify plot attributes by adding elements using the + symbol. For example, we 
#can add a title by using + ggtitle="TEXT", and axis labels using + xlab("TEXT") + ylab("TEXT")
#.

# plot Air Temperature Data across 2009-2011 using daily data
ggplot(harMetDaily.09.11, aes(date, airt)) +geom_point(na.rm=TRUE, color="blue", size=1) + 
  ggtitle("Air Temperature 2009-2011\n NEON Harvard Forest Field Site") +
  xlab("Date") + ylab("Air Temperature (C)")+scale_x_date(labels=date_format("%b %y")



# Format Dates in Axis Labels
# 
# We can adjust the date display format (e.g. 2009-07 vs. Jul 09) and the number of major 
# and minor ticks for axis date values using scale_x_date. Letís format the axis ticks so they 
# read ìmonth yearî (%b %y). To do this, we will use the syntax:
#   

  
#Rather than re-coding the entire plot, we can add the scale_x_date element to the plot
#object AirTempDaily that we just created.
               
               # format x-axis: dates
AirTempDailyb <- AirTempDaily + 
(scale_x_date(labels=date_format("%b %y")))
               
AirTempDailyb
               
               
#Adjust Date Ticks
               
#We can adjust the date ticks too. In this instance, having 1 tick per year may be 
#enough. If we have the scales package loaded, we can use breaks=date_breaks("1 year") 
#within the scale_x_date element to create a tick for every year. We can adjust this as 
#needed (e.g. 10 days, 30 days, 1 month).
               
               
               
# format x-axis: dates
AirTempDaily_6mo <- AirTempDaily + 
(scale_x_date(breaks=date_breaks("6 months"),
labels=date_format("%b %y")))
               
AirTempDaily_6mo
               
               
 # format x-axis: dates
AirTempDaily_1y<-AirTempDaily + 
(scale_x_date(breaks=date_breaks("1 year"),labels=date_format("%b %y")))
AirTempDaily_1y
               
               
               
               
# ggplot - Subset by Time
               
# Sometimes we want to scale the x- or y-axis to a particular time subset without 
# subsetting the entire data_frame. To do this, we can define start and end times. 
# We can then define the limits in the scale_x_date object as follows:
#                  
  scale_x_date(limits=start.end) +
                 
    
    # Define Start and end times for the subset as R objects that are the time class
    startTime <- as.Date("2011-01-01")
  endTime <- as.Date("2012-01-01")
  
  # create a start and end time R object
  start.end <- c(startTime,endTime)
  start.end
  
  ## [1] "2011-01-01" "2012-01-01"
  
  # View data for 2011 only
  # We will replot the entire plot as the title has now changed.
  AirTempDaily_2011 <- ggplot(harMetDaily.09.11, aes(date, airt)) +
    geom_point(na.rm=TRUE, color="purple", size=1) + 
    ggtitle("Air Temperature\n 2011\n NEON Harvard Forest") +
    xlab("Date") + ylab("Air Temperature (C)")+ 
    (scale_x_date(limits=start.end,
                  breaks=date_breaks("1 year"),
                  labels=date_format("%b %y")))
  
  AirTempDaily_2011
  
               
               
               
            