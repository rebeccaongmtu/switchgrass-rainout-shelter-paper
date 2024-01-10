#FIELD DATA - SOIL PROPERTIES AND WEATHER
#This code generates plots and statistics for soil properties and weather data  

library(readr)
library(tidyverse)
library(lubridate)
library(ggh4x)
library(reshape2)
library(viridisLite)
library(viridis)


# SOIL MOISTURE AND TEMPERATURE -------------------------------------------

#IMPORT DATA
#Set the working directory to the folder containing the data file
setwd("/Directory") #Replace "/Directory" with the specific pathname for the folder where the CSV files are located - Changes the current working directory

data2 <- 
  read_csv("Field_Data.csv", 
           comment ="#") 
#Reads the CSV that contains MLE data. 
#The comment = '#' removes any cells that begin with # and removes header explanatory text

#FORMAT DATA
unique(data2$site) #determines unique values for the "site" column in the dataframe

#Recode Labels in Dataset
data2$site[data2$site == "Michigan North - Escanaba"] <- "MI-North \n Escanaba"
data2$site[data2$site == "Michigan Central - Lake City Exp. Station"] <- "MI-Central \n Lake City"
data2$site[data2$site == "Michigan South - Lux Arbor Reserve"] <- "MI-South \n Lux Arbor"
data2$site[data2$site == "Wisconsin North - Rhinelander"] <- "WI-North \n Rhinelander"
data2$site[data2$site == "Wisconsin Central - Hancock"] <- "WI-Central \n Hancock"
data2$depth[data2$depth == "10"] <- "Shallow"
data2$depth[data2$depth == "25"] <- "Deep"
data2$treatment[data2$treatment == "ambient"] <- "Ambient"
data2$treatment[data2$treatment == "rainout"] <- "Rainout"

#Specify format of columns
data2 $ site <-as.factor( data2 $ site ) #change variable class to factor
data2 $ treatment <-as.factor( data2 $ treatment ) #change variable class to factor
data2 $ depth = factor(data2$depth, c("Shallow", "Deep"))

#Specify ordering of site factor
data2$site = factor(data2$site, c('MI-South \n Lux Arbor','MI-Central \n Lake City','MI-North \n Escanaba','WI-Central \n Hancock', 'WI-North \n Rhinelander'))


#COLOR SCHEME
mycolors = c("#444e86", "#ff6e54")


#SOIL MOISTURE DATA
#Processing soil moisture data (volumetric water content) to only include data for the five field sites, the ambient and rainout treatments, and the growing season (April-October from 2018-2021), averaged across field plot - data is converted to an average daily value
data_vwc_avg<- data2 %>%
  mutate(day = day(datetime), month = month(datetime), year = year(datetime), fakedate = make_date(2018, month, day)) %>%
  filter(month > 4 & month < 11) %>%
  group_by(site, depth, treatment, year, fakedate) %>%
  summarize(avg_vwc = mean(vwc, na.rm =T))

#Plot of Soil Moisture Data, Faceted by Year, Location and Depth (size = 11" x 3.66")
  ggplot(data_vwc_avg, aes(fakedate, avg_vwc, color=treatment)) +
  geom_point(size = 0.3) + scale_color_manual(values = mycolors) +
  theme_bw() +
  facet_nested(depth~site+year) + 
  theme(legend.position = "bottom", axis.text.x=element_text(angle=90, hjust=1, vjust = 0.5)) +
  ylab('Soil Volumetric Water Content') + xlab("Date") +
  scale_x_date(date_breaks = '2 month', date_minor_breaks = '1 month', date_labels = "%b")

  
#SOIL TEMPERATURE DATA
  #Processing soil temperature data to only include data for the five field sites, the ambient and rainout treatments, and the growing season (April-October from 2018-2021), averaged across field plot - data is converted to an average daily value
  data_temp_avg <- data2 %>%
  mutate(day = day(datetime), month = month(datetime), year = year(datetime), fakedate = make_date(2018, month, day)) %>%
  filter(month > 4 & month < 11) %>%
  group_by(site, depth, treatment, year, fakedate) %>%
  summarize(avg_temp = mean(temperature, na.rm =T))

#Plot of Soil Moisture Data, Faceted by Year, Location and Depth (Plot = 11" x 3.66")
  ggplot(data_temp_avg, aes(fakedate, avg_temp, color=treatment)) +
  geom_point(size = 0.3) + scale_color_manual(values = mycolors) +
  geom_hline(yintercept=25, linetype = "dashed") +
  theme_bw() +
  facet_nested(depth~site+year) + 
  theme(legend.position = "bottom", axis.text.x=element_text(angle=90, hjust=1, vjust = 0.5)) +
  ylab(expression("Soil Temperature " (degree*C))) + xlab("Date") +
  scale_x_date(date_breaks = '2 month', date_minor_breaks = '1 month', date_labels = "%b")

#Max & Min Temp Dot Plots  
data_temp_avg %>% 
    group_by(site, depth, treatment, year) %>%
    summarize(max_value = max(avg_temp)) %>%
    ggplot(aes(year, max_value, color = treatment)) +
    geom_point()+
    scale_color_manual(values = mycolors) +
    facet_grid(depth~site)+
    theme_bw() +
    xlab("Year")+
    ylab("Maximum Soil Temperature (°C)")+
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1), legend.position = "bottom")



# AMBIENT TEMPERATURE -----------------------------------------------------

#IMPORT DATA
setwd("/Directory") #Replace "/Directory" with the specific pathname for the folder where the CSV files are located - Changes the current working directory
TempData <- read_csv("TempHeatMap.csv", comment ="#") 

#FORMAT DATA
#Relabel Data
TempData$Location[TempData$Location == "Escanaba"] <- "MI-North\nEscanaba"
TempData$Location[TempData$Location == "Lake City"] <- "MI-Central\nLake City"
TempData$Location[TempData$Location == "Lux Arbor"] <- "MI-South\nLux Arbor"
TempData$Location[TempData$Location == "Rhinelander"] <- "WI-North\nRhinelander"
TempData$Location[TempData$Location == "Hancock"] <- "WI-Central\nHancock"

#Specify column formats
TempData$Year <- as.factor(TempData$Year)
TempData$Location <- as.factor(TempData$Location)
TempData$Latitude <- as.factor(TempData$Latitude)
TempData$State <- as.factor(TempData$State)

#Reformat Dataframe
TempData2<- TempData %>%
  pivot_longer(!c(Location, Year, Latitude, State), names_to = "Date", values_to = "Temp")

#Specify additional formats
TempData2$Date <- mdy(TempData2$Date)
TempData2$Location <- factor(TempData2$Location, c("MI-North\nEscanaba", "WI-North\nRhinelander", "MI-Central\nLake City", "WI-Central\nHancock", "MI-South\nLux Arbor"))


#TEMPERATURE HEAT PLOT (3" x 5")
ggplot(TempData2, aes(Date,Location)) + geom_tile(aes(fill = Temp)) +
  scale_fill_viridis(option="magma") +
  theme_bw() + theme_minimal() + 
  facet_grid(~Year)+   
  xlab("Month")+
  scale_x_date(date_breaks = '1 month', date_labels = "%b") +
  theme(panel.grid.major.y = element_blank(), panel.grid.minor = element_blank(), axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), legend.title=element_text(size=9))+
  labs(fill = "7-Day\nAvg\nMax\nTemp\n(°C)")
