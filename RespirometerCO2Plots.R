#RESPIROMETER FERMENTATION CO2 TIMECOURSE PLOTS
#This code generates faceted line plots of CO2 production from fermentation experiments

library(tidyverse)
library(ggnewscale)
library(ggh4x)

#IMPORT DATA
#Set the working directory to the folder containing the data file
setwd("/Directory") #Replace "/Directory" with the specific pathname for the folder where the CSV files are located - Changes the current working directory

#Pull all CSV files from directory and put them in a list of dataframes
temp = list.files(pattern="*.csv")
myfiles = lapply(temp, read.csv, row.names = 1, skip = 0, header = F, comment = "#")

data <- map(myfiles, 
            ~t(.x) %>%
              as_tibble(.x) %>%
              pivot_longer(names_to = "Time", 
                           values_to = "CO2", 
                           cols = -c("HydrolysateBarcode", "FermentationBarcode", "Location", "HarvestYr", "Plot", "Treatment", "Date", "Replicate")))

data<- do.call("rbind", data)


#PROCESS DATA
#Relabel Location and Plot to be more descriptive
data$Location[data$Location == 'Escanaba'] <- "MI-North \n Escanaba"
data$Location[data$Location == 'Rhinelander'] <- "WI-North \n Rhinelander"
data$Location[data$Location == 'Hancock'] <- "WI-Central \n Hancock"
data$Location[data$Location == 'Lake City'] <- "MI-Central \n Lake City"
data$Location[data$Location == 'Lux Arbor'] <- "MI-South \n Lux Arbor"

data$Plot[data$Plot == '1'] <- "Plot 1"
data$Plot[data$Plot == '2'] <- "Plot 2"
data$Plot[data$Plot == '3'] <- "Plot 3"
data$Plot[data$Plot == '4'] <- "Plot 4"

#Ensure factors and variables are in proper formats
data $ Location <-as.factor(data $ Location ) #change variable class to factor
data $ Plot <- as.factor (data $ Plot)
data $ Treatment <- as.factor (data $ Treatment)
data $ Replicate <- as.factor(data $ Replicate)
data $ Time <- as.numeric(data $ Time)
data $ CO2 <- as.numeric(data $ CO2)

#Filter to only include ambient and rainout samples (no control data) and filter to exclude any times > 40 hr so all lines end at the same point
nocontrols <- filter(data, (Treatment == "Ambient" | Treatment == "Rainout"), Time <=40) 

#Reorganize locations so facets are in a specific order
nocontrols$Location = factor(nocontrols$Location, c('MI-South \n Lux Arbor','MI-Central \n Lake City','MI-North \n Escanaba','WI-Central \n Hancock', 'WI-North \n Rhinelander'))


#SET UP FORMATTING
#Define color scheme
mycolors = c("#003f5c", "#ff6e54")
mycolors_muted = c("#b2c5ce", "#ffd3cb")


#GENERATE LINE PLOTS
#Grid of Fermentation CO2 Production by Treatment and faceted by Year, Plot, and Location - Saved as 8" x 5"
ggplot(nocontrols, aes(Time, CO2, color = Treatment)) +
  geom_point(size = 0.01) +
  scale_color_manual(values = mycolors_muted) +
  stat_smooth(linewidth = 0.01) +
  new_scale_colour() +
  geom_smooth(se = F, show.legend = TRUE, aes(color = Treatment)) + 
  scale_color_manual(values = mycolors) +
  facet_nested(Location~HarvestYr+Plot) +
  theme_bw() + scale_x_continuous(breaks = seq(0,50,12)) +
  ylab(expression("CO"[2]*" (mL)")) + xlab("Time (hr)") +
  theme(strip.text.y.right = element_text(angle = 0), 
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(size =  0.25))
