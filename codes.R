cat("set working directory")
getwd()
setwd("~/project")

cat("import required library")
library(stringr)
library(lubridate)
library(dplyr)
library(plotly)

cat("import the project file(download address : https://data.cityofnewyork.us/api/views/h9gi-nx95/rows.csv?accessType=DOWNLOAD)")
df <- read.csv("NYPD_Motor_Vehicle_Collisions.csv")

cat("create a backup")
df_backup <- df
df <- df_backup

cat("Create the  calculated field <easy to modify if required <modify if required>")
df$TOTAL.COLLISION <- df$NUMBER.OF.PERSONS.INJURED  +  df$NUMBER.OF.PERSONS.KILLED  +  df$NUMBER.OF.PEDESTRIANS.INJURED  +  df$NUMBER.OF.PEDESTRIANS.KILLED  +  df$NUMBER.OF.CYCLIST.INJURED  +  df$NUMBER.OF.CYCLIST.KILLED  +  df$NUMBER.OF.MOTORIST.INJURED  +  df$NUMBER.OF.MOTORIST.KILLED

cat("transform string to date and time")
df$DATE.1 <- as.Date( as.character(df$DATE), "%m/%d/%Y")
df$TIME.HOUR <- as.numeric(sapply(strsplit(as.character(df$TIME),':'), "[", 1))
df$TIME.MIN <- as.numeric(sapply(strsplit(as.character(df$TIME),':'), "[", 2))
df$MONTH.OF.YEAR <- month(df$DATE.1)

cat("Question 1a")
cat("Filter this data to all collisions on your birthday. Display a histogram of the
number of collisions that occur each hour. That is, your x-axis will have the
hours from 0 to 23 and the y-axis will be the number of collisions. Make sure
to include the date in the title of your plot.")

cat("solution 1a")
cat("Enter the date for which analysis is required in the filter")
df.1a <- subset(df, DATE.1 == as.Date("2016-08-17"))
dt <- max(df.1a$DATE.1)

cat("Aggregation of data to view histogram")
df.1a <- df.1a %>%   group_by(TIME.HOUR) %>%   summarize(HOURLY.TOTAL.COLLISION = sum(TOTAL.COLLISION))

cat("Select required columns")
df.1a <- df.1a[,c("TIME.HOUR","HOURLY.TOTAL.COLLISION")] 

cat("plot solution 1a graph")
plot_ly(df.1a, x = df.1a$TIME.HOUR, y = df.1a$HOURLY.TOTAL.COLLISION, type = 'bar', 
            text = text, marker = list(color = 'rgb(158,202,225)',
            line = list(color = 'rgb(8,48,107)',width = 1.5))) %>%
            layout(title = paste("Number of collisions of each hours on ",dt),
            xaxis = list(title = "Hour marker"),
            yaxis = list(title = "Number of Collisions"))

cat("plot and save solution 1a graph")
p <- plot_ly(df.1a, x = df.1a$TIME.HOUR, y = df.1a$HOURLY.TOTAL.COLLISION, type = 'bar', 
            text = text, marker = list(color = 'rgb(158,202,225)',
            line = list(color = 'rgb(8,48,107)',width = 1.5))) %>%
            layout(title = paste("Number of collisions of each hours on ",dt),
            xaxis = list(title = "Hour marker"),
            yaxis = list(title = "Number of Collisions"))
htmlwidgets::saveWidget(p, "Solution_1a.html")
  
cat("Question 1b")
cat("Pick one borough (such as Bronx, Manhattan, etc). Display the fraction of
collisions that occur in that borough each hour. That is, for 0 (midnight to
just before 1am), you should have as your y-value the fraction of: collisions
that occurred in that borough at hour 0 over collisions across the whole city
that occurred at hour 0.")
cat("solution 1b")

cat("Enter the BOROUGH for which analysis is required in the filter")
REQUIRED_BOROUGH = "MANHATTAN"
df.1b <- subset(df, DATE.1 == as.Date("2016-08-17") & BOROUGH == REQUIRED_BOROUGH)
brg <- REQUIRED_BOROUGH

cat("Aggregation of data to view histogram")
df.1b <- df.1b %>%   group_by(TIME.HOUR) %>%   summarize(HOURLY.TOTAL.COLLISION.AREAWISE = sum(TOTAL.COLLISION))

cat("Select required columns")
df.1b <- df.1b[,c("TIME.HOUR","HOURLY.TOTAL.COLLISION.AREAWISE")] 

cat("get results from solution 1a")
df.1b <- merge(x = df.1a, y = df.1b, by = "TIME.HOUR", all = TRUE)

cat("create a calculated field to calculat the ratio")
df.1b$ratio <- ifelse(df.1b$HOURLY.TOTAL.COLLISION.AREAWISE == 0 | df.1b$HOURLY.TOTAL.COLLISION == 0 , 0,df.1b$HOURLY.TOTAL.COLLISION.AREAWISE/df.1b$HOURLY.TOTAL.COLLISION)
df.1b$ratio <- ifelse(is.na(df.1b$ratio) , 0 ,df.1b$ratio)

cat("plot solution 1b graph")
plot_ly(df.1b, x = df.1b$TIME.HOUR, y = df.1b$ratio, type = 'bar', 
      text = text, marker = list(color = 'rgb(158,202,225)',
      line = list(color = 'rgb(8,48,107)',width = 1.5))) %>%
      layout(title = paste("Ratio of collisions of each hours in area BOROUGH to the entire estate = ",brg),
      xaxis = list(title = "Hour marker"),
      yaxis = list(title = "Number of Collisions"))

cat("plot and save solution 1b graph")
p <- plot_ly(df.1b, x = df.1b$TIME.HOUR, y = df.1b$ratio, type = 'bar', 
      text = text, marker = list(color = 'rgb(158,202,225)',
      line = list(color = 'rgb(8,48,107)',width = 1.5))) %>%
      layout(title = paste("Ratio of collisions of each hours in area BOROUGH to the entire estate= ",brg),
      xaxis = list(title = "Hour marker"),
      yaxis = list(title = "Number of Collisions"))
htmlwidgets::saveWidget(p, "Solution_1b.html")


cat("Question 1c")
cat("Display the number of collisions that occur in the your birthday binned by
    minute. That is, for 0 (zero minutes after the hour), you should have as your
    y-value the fraction of: collisions that occurred on your birthday at 0 minutes
    after the hour over collisions. The x-axis of your plot should be the minutes
    from 0 to 59 (minutes after the hour), and the y-axis should be the sum of
    the accidents that occur at each minute after the hour.")
cat("solution 1c")

cat("Taking the date from question 1 part a")
df.1c <- subset(df, DATE.1 == as.Date(dt))

cat("Aggregation of data to view histogram")
df.1c <- df.1c %>%   group_by(TIME.MIN) %>%   summarize(TOTAL.COLLISION.MINUTEWISE = sum(TOTAL.COLLISION))

cat("Select required columns")
df.1c <- df.1c[,c("TIME.MIN","TOTAL.COLLISION.MINUTEWISE")] 

cat("plot solution 1c graph")
plot_ly(df.1c, x = df.1c$TIME.MIN, y = df.1c$TOTAL.COLLISION.MINUTEWISE, type = 'bar', 
      text = text, marker = list(color = 'rgb(158,202,225)',
      line = list(color = 'rgb(8,48,107)',width = 1.5))) %>%
      layout(title = paste("Number of collisions of each minute on ",dt),
      xaxis = list(title = "Minute marker"),
      yaxis = list(title = "Number of Collisions"))

cat("plot and save solution 1c graph")
p <- plot_ly(df.1c, x = df.1c$TIME.MIN, y = df.1c$TOTAL.COLLISION.MINUTEWISE, type = 'bar', 
      text = text, marker = list(color = 'rgb(158,202,225)',
      line = list(color = 'rgb(8,48,107)',width = 1.5))) %>%
      layout(title = paste("Number of collisions of each minute on ",dt),
      xaxis = list(title = "Minute marker"),
      yaxis = list(title = "Number of Collisions"))
      htmlwidgets::saveWidget(p, "Solution_1c.html")


cat("Question 2")
cat("Pick a zip code in New York. You can look in the dataset, or you want a specific location, check http://www.zipmap.net/New York.htm.
Filter the dataset for that zipcode .Display a histogram of the number of
collisions that occur each month. That is, your x-axis will have the numbers
from 1 to 12 representing the months of the year and the y-axis will be the
number of collisions. Make sure to include in the title of your plot the zip
code plotted")
cat("solution 2")


cat("Enter the zipcode for which analysis is required in the filter")
df.2 <- subset(df, DATE.1 == as.Date(dt) & ZIP.CODE == 10003)
zpc <- as.factor(max(df.2$ZIP.CODE))

cat("create blank months")
MONTH.OF.YEAR = c(1,2,3,4,5,6,7,8,9,10,11,12) 
TOTAL.COLLISION = c(0,0,0,0,0,0,0,0,0,0,0,0) 
df_dummy = data.frame(MONTH.OF.YEAR, TOTAL.COLLISION) 
df.2 <- df.2[,c("MONTH.OF.YEAR","TOTAL.COLLISION")] 

cat("bind both data frams")
df.2 = rbind(df.2,df_dummy)

cat("Aggregation of data to view histogram")
df.2 <- df.2 %>%   group_by(MONTH.OF.YEAR) %>%   summarize(MONTHLY.COLLISIONS = sum(TOTAL.COLLISION))


cat("plot solution 2 graph")
plot_ly(df.2, x = df.2$MONTH.OF.YEAR, y = df.2$MONTHLY.COLLISIONS, type = 'bar', 
      text = text, marker = list(color = 'rgb(158,202,225)',
      line = list(color = 'rgb(8,48,107)',width = 1.5))) %>%
      layout(title = paste("Number of collisions of each month in area zipcode = ",zpc),
      xaxis = list(title = "Month of year marker"),
      yaxis = list(title = "Number of Collisions"))

cat("plot and save solution 2 graph")
p <- plot_ly(df.2, x = df.2$MONTH.OF.YEAR, y = df.2$MONTHLY.COLLISIONS, type = 'bar', 
        text = text, marker = list(color = 'rgb(158,202,225)',
        line = list(color = 'rgb(8,48,107)',width = 1.5))) %>%
        layout(title = paste("Number of collisions of each month in area zipcode = ",zpc),
        xaxis = list(title = "Month of year marker"),
        yaxis = list(title = "Number of Collisions"))
        htmlwidgets::saveWidget(p, "Solution_2.html")
