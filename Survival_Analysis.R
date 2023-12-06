#FERMENTATION SURVIVAL ANALYSIS
#This code generates faceted survival plots annotated with p-values from a paired t-test

library(readr)
library(survival)
library(ggplot2)
library(survminer)
library(tidyverse)

##Important note. The lag phase has to have numeric values for all entries. 
#If the fermentation did not reach the end of lag phase by the end of the experiment, 
#code the Status as "0" (censored) and make the lag phase = maximum fermentation time.


#IMPORT DATA
#Set the working directory to the folder containing the data file
setwd("/Directory") #Replace "Directory" with the specific pathname for the folder where the CSV file is located

#Import the data into a dataframe
fulldata <- read_csv("Survival_Analysis_Data.csv", comment = "#")

#PROCESS DATA
#Specify the format of the relevant data
fulldata $ FermentationBarcode <- as.factor(fulldata $ FermentationBarcode)
fulldata $ Location <-as.factor( fulldata $ Location ) #change variable class to factor
fulldata $ Plot <-as.factor( fulldata $ Plot ) #change variable class to factor
fulldata $ Treatment <-as.factor( fulldata $ Treatment ) #change variable class to factor
fulldata $ Year <-as.factor( fulldata $ Year)
fulldata <- fulldata %>% mutate_if(is.character, as.numeric) #convert all columns that are characters to numeric

#Specify order of Locations for plotting
fulldata$Location = factor(fulldata$Location, c('Control', 'Lux Arbor','Lake City','Escanaba', 'Hancock', 'Rhinelander'))


#SET UP FORMATTING
#Specify color and linestyle schemes
mycolors_base = c("#003f5c", "#444e86", "#955196", "#dd5182", "#ff6e54", "#ffa600")
mycolors <- c("#955196", "#dd5182", rep("#003f5c", each = 10), "#444e86", rep("#ff6e54", each = 10), "#ffa600")
linestyle <- c("dashed", "solid", rep("dashed", each = 11), rep("solid", each = 11))
mycolors2 = c("#003f5c", "#ff6e54")


#GENERATE LIST OF P-VALUES FOR SURVIVAL PLOT
#Pull locations and years into lists for iteration
  location_list = unique(fulldata$Location)
  year_list = unique(fulldata$Year)

#Set up dataframe to store p-values
  locn_pvalues = data.frame(matrix(ncol = 3, nrow = 0))
  colnames(locn_pvalues) = c("pvalue", "Location" , "Year")
 
#Iteration to generate a list of p-values for the log-rank test of the difference between treatments for a location/year pairing
for (locn in location_list) {
    for(yr in year_list) {
  locn_yr <- fulldata %>%
    filter(Location == locn, Year == yr) 
  
  print(paste("Generating p-value", locn, yr))
  
  pvalue_item <- survdiff(Surv(LagTime, Status) ~ Treatment, data = locn_yr)$pvalue
  
  locn_pvalues[nrow(locn_pvalues) +1,] = rbind(c(pvalue_item, locn, yr))
  
    }}

#Update the p-value dataframe with text fields (pval_label) to annotate on the survival plots
  locn_pvalues$pvalue <- as.numeric(locn_pvalues$pvalue)
  locn_pvalues$pvalue_brief <- format(round(locn_pvalues$pvalue, 3), nsmall = 3)
  locn_pvalues$pval_label <- paste("p =", locn_pvalues$pvalue_brief)

  
#FACETED SURVIVAL PLOT OF THE LAG PHASE DATA
#Note: The control data is plotted as individual years for convenience of comparison
#...This is coded in the CSV data and does not correspond to the specified harvest years
  fit<- survfit(Surv(LagTime, Status) ~ Treatment+Year+Location, data = fulldata)
  ggsurv <- ggsurvplot(fit, fun = "event", conf.int = F, ggtheme = theme_bw(), palette = mycolors, size = 0.5, linetype = linestyle, censor = F)
  ggsurv$plot +theme_bw() + 
    theme (legend.position = "bottom") +
    xlab("Lag Time (hr)") +
    ylab("Cumulative Proportion Past Lag Phase")+
    facet_grid(Year ~ factor (Location, levels = c('Control','Lux Arbor','Lake City','Escanaba', 'Hancock', 'Rhinelander'))) +
    geom_text(x = 0, y = 0.9, hjust=0, aes(label = pval_label), data = locn_pvalues, size = 3)
  
#Note - the survival plot should be saved as a PDF and edited using software such as Adobe Illustrator or Inkscape to align the p-values and generate a suitable legend.
  