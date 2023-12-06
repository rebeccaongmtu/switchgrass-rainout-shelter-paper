#BIOMASS AND HYDROLYSATE COMPOSITION AND FERMENTATION DATA ANALYSIS
#This code generates bar graphs and conducts ANOVAs for lignocellulosic biomass and hydrolysate composition and fermentation ethanol production and glucose consumption

library(readr)
library(reshape2)
library(ggplot2)
library(ggpubr)
library(dplyr)


# BIOMASS COMPOSITION -----------------------------------------------------

#IMPORT DATA
#Set working directory
setwd("/Directory") #Replace "/Directory" with the specific pathname for the folder where the CSV files are located - Changes the current working directory
NIR_data <- read_csv("NIR_Composition_both_years.csv",  comment = "#") #Import CSV file


#PROCESS DATA
#Specify the format of the relevant data
NIR_data $ Year <-as.factor( NIR_data $ Year)
NIR_data $ Location <-as.factor( NIR_data $ Location ) #change variable class to factor
NIR_data $ Plot <-as.factor( NIR_data $ Plot ) #change variable class to factor
NIR_data $ Treatment <-as.factor( NIR_data $ Treatment ) #change variable class to factor
NIR_data $ Glucan <- as.numeric (NIR_data $ Glucan)
NIR_data $ Xylan <- as.numeric (NIR_data $ Xylan)

#Melt dataframe for processing
meltedNIR <- melt(NIR_data, id.vars=c("Location", "Treatment", "Plot", "Year"))

#Specify order of Location variable
meltedNIR$Location = factor(meltedNIR$Location, c('Lux Arbor','Lake City','Escanaba','Hancock', 'Rhinelander'))


#COLOR SCHEME
mycolors = c("#444e86", "#ff6e54")


#BAR CHART OF BIOMASS GLUCAN & XYLAN
#Plots a bar graph with * representing the p-value for the paired t-test between treatments (ambient/rainout) for the same location/harvest year
ttest_comp <- compare_means(value ~ Treatment, data = meltedNIR, method = "t.test", group.by = c("Location", "Year", "variable"))
ggbarplot(meltedNIR, x = "Location", y = "value", facet.by = c("variable", "Year"), add = "mean_sd", error.plot = "upper_errorbar", ggtheme = theme_bw(),
          color = "Treatment", fill = "Treatment", palette = mycolors, 
          position = position_dodge(0.8)) + ylab("Biomass NIR Composition (%)") +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.75, hjust = 0.75)) +
  #geom_point(position = position_jitterdodge(jitter.width = 0.3), aes(fill = Drought), color = "white", stroke = 0.005, shape = 21) +
  stat_compare_means(aes(group = Treatment), method = "t.test", label = "p.signif", label.y = 30, hide.ns = T)

#Generate a CSV file containing the data from the paired t-test
write.csv(list(ttest_comp), 'biomass_composition_paired_t-test.csv')

#Run an ANOVA on the glucan and xylan contents with Treatment, Location, and Year as variables and Plot nested within Location
anova_Glucan <- lm(Glucan~ Treatment + Location/Plot + Year, data=NIR_data)
anova_Xylan <- lm(Xylan~ Treatment + Location/Plot + Year, data=NIR_data)
anova_Glucan_table <- anova(anova_Glucan)
anova_Xylan_table <- anova(anova_Xylan)

#Make CSV files of the ANOVAs
write.csv(list(anova_Glucan_table), 'biomass_glucan_ANOVA_GLM.csv')
write.csv(list(anova_Xylan_table), 'biomass_xylan_ANOVA_GLM.csv')



# HYDROLYSATE AND FERMENTATION DATA -------------------------------------------------

#IMPORT DATA
#Set working directory and import data
setwd("/Directory") #Replace "/Directory" with the specific pathname for the folder where the CSV files are located - Changes the current working directory
data <- read_csv("FermentationHPLCData.csv",  comment = "#")

#PROCESS DATA
#Remove columns with barcodes
data <- data[,-c(1:4)] 

#Recode Locations
data[data == "Escanaba"] <- "MI-North \n Escanaba"
data[data == "Lake City"] <- "MI-Central \n Lake City"
data[data == "Lux Arbor"] <- "MI-South \n Lux Arbor"
data[data == "Rhinelander"] <- "WI-North \n Rhinelander"
data[data == "Hancock"] <- "WI-Central \n Hancock"

#Specify column formats
data $ Year <-as.factor( data $ Year)
data $ Date <-as.Date( data $ Date , tryFormats = c("%m/%d/%y")) #change variable class to date (m/d/y)
data $ Location <-as.factor( data $ Location ) #change variable class to factor
data $ Plot <-as.factor( data $ Plot ) #change variable class to factor
data $ Treatment <-as.factor( data $ Treatment ) #change variable class to factor
data <- data %>% mutate_if(is.character, as.numeric) #convert all remaining columns that are characters to numeric

#Filter to remove control samples
filtered_data <- data %>% filter(Location != "Control")

#Specify order of Location variable
filtered_data$Location = factor(filtered_data$Location, c('MI-South \n Lux Arbor','MI-Central \n Lake City','MI-North \n Escanaba','WI-Central \n Hancock', 'WI-North \n Rhinelander'))


#COLOR SCHEMES
mycolors = c("#444e86", "#ff6e54")
mycolors6 = c("#444e86", "#dd5182", "#955196", "#003f5c", "#ff6e54", "#ffa600")


#HYDROLYSATE GLUCAN CONCENTRATION - BAR CHART AND STATS
#Plots a bar graph with * representing the p-value for the paired t-test between treatments (ambient/rainout) for the same location/harvest year
ttest_Glu <- compare_means(PreGlucose ~ Treatment, data = filtered_data, method = "t.test", group.by = c("Location", "Plot", "Year"))
ggbarplot(filtered_data, x = "Plot", y = "PreGlucose", facet.by = c("Year", "Location"), add = "mean_sd", error.plot = "upper_errorbar", ggtheme = theme_bw(),
          color = "Treatment", fill = "Treatment", palette = mycolors,
          position = position_dodge(0.8)) + ylab("Hydrolysate Glucose Concentration (g/L)") +
  theme(legend.position = "bottom") +
  #geom_point(position = position_jitterdodge(jitter.width = 0.3), aes(fill = Treatment), color = "white", stroke = 0.005, shape = 21) +
  stat_compare_means(aes(group = Treatment), method = "t.test", label = "p.signif", label.y = 58, hide.ns = T)

#Generate a CSV file containing the data from the paired t-test
write.csv(list(ttest_Glu), 'hydrolysate_glucose_paired_t-test.csv')

#Run an ANOVA on the glucan concentration with Treatment, Location, and Year as variables and Plot nested within Location and generate CSV file
anova_Glu <- lm(PreGlucose~ Treatment + Location/Plot + Year, data=filtered_data)
anova_Glu_table <- anova(anova_Glu)
write.csv(list(anova_Glu_table), 'hydrolysate_glucose_ANOVA_GLM.csv')


#FERMENTATION GLUCOSE CONSUMPTION AND ETHANOL PRODUCTION - BAR CHART AND STATS

#Prepare summary statistics - average and standard deviation - in a dataframe
Summary_means <- data %>% # the names of the new data frame and the data frame to be summarised
  group_by(Location, Treatment, Plot, Year) %>%   # the grouping variable
  dplyr::summarise(across(where(is.numeric), list(mean = mean), .names = "{.col}")) #summarize across all numeric columns

Summary_sd <- data %>% # the names of the new data frame and the data frame to be summarised
  group_by(Location, Treatment, Plot, Year) %>%   # the grouping variable
  dplyr::summarise(across(where(is.numeric), list(sd = sd), .names = "{.col}")) #summarize across all numeric columns

melted_means <- melt(Summary_means, id.vars=c("Location", "Treatment", "Plot", "Year"), value.name = "mean")
melted_sd <- melt(Summary_sd, id.vars=c("Location", "Treatment", "Plot", "Year"), value.name = "sd")
melted <- merge(melted_means, melted_sd)

#Bar Chart for Ethanol Production
melted_EtOH <- melted %>% filter(variable == "EthanolProduced")
melted_EtOH$Location = factor(melted_EtOH$Location, c('Control', 'MI-South \n Lux Arbor','MI-Central \n Lake City','MI-North \n Escanaba','WI-Central \n Hancock', 'WI-North \n Rhinelander'))
ggplot(melted_EtOH, aes(fill = Treatment, x = Plot, y = mean)) + 
  geom_col(width=0.75, position=position_dodge(0.75)) + 
  facet_grid(Year~Location, scales = "free") + 
  geom_errorbar(aes(ymin = mean, ymax = mean + sd),size = 0.25, width=.25, position=position_dodge(0.75)) + 
  scale_fill_manual(values = mycolors6) + 
  theme_bw() + 
  theme(strip.text.y.right = element_text(angle = 0), axis.text.y = element_text(size=10), axis.text.x = element_text(size = 10, angle = 45, vjust =1, hjust = 1), legend.position = "bottom") + 
  ylab("Ethanol Produced (g/L)")

#Run paired t-test on the effect of ambient-rainout treatment on ethanol production and make a CSV file
ttest_EtOH<-compare_means(EthanolProduced ~ Treatment, data = filtered_data, method = "t.test", group.by = c("Location", "Plot", "Year"))
write.csv(list(ttest_EtOH), 'ethanol_produced_paired_t-test.csv')

#Bar Chart for Glucose Consumption 
melted_Glu <- melted %>% filter(variable == "GlucoseConsumed")
melted_Glu$Location = factor(melted_Glu$Location, c('Control', 'MI-South \n Lux Arbor','MI-Central \n Lake City','MI-North \n Escanaba','WI-Central \n Hancock', 'WI-North \n Rhinelander'))
ggplot(melted_Glu, aes(fill = Treatment, x = Plot, y = mean)) + 
  geom_col(width=0.75, position=position_dodge(0.75)) + 
  facet_grid(Year~Location, scales = "free") + 
  geom_errorbar(aes(ymin = mean, ymax = mean + sd),size = 0.25, width=.25, position=position_dodge(0.75)) + 
  scale_fill_manual(values = mycolors6) + 
  theme_bw() + 
  theme(strip.text.y.right = element_text(angle = 0), axis.text.y = element_text(size=10), axis.text.x = element_text(size = 10, angle = 45, vjust =1, hjust = 1), legend.position = "bottom") + 
  ylab("Glucose Consumed (g/L)")

#Paired t-test on the effect of ambient-rainout treatment on glucose consumption and make a CSV file
ttest_Glu<-compare_means(GlucoseConsumed ~ Treatment, data = filtered_data, method = "t.test", group.by = c("Location", "Plot", "Year"))
write.csv(list(ttest_Glu), 'glucose_consumed_paired_t-test.csv')

#ANOVAs on ethanol production and glucose consumption for switchgrass samples (no controls)
anova_EtOH <- lm(EthanolProduced~ Treatment + Location/Plot + Year, data=filtered_data)
anova_EtOH_table <- anova(anova_EtOH)
write.csv(list(anova_EtOH_table), 'ethanol_production_ANOVA_GLM.csv')

anova_GluCnsm <- lm(GlucoseConsumed~ Treatment + Location/Plot + Year, data=filtered_data)
anova_GluCnsm <- anova(anova_GluCnsm)
write.csv(list(anova_GluCnsm_table), 'glucose_consumption_ANOVA_GLM.csv')

