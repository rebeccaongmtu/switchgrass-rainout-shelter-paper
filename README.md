# switchgrass-rainout-shelter-paper

CODE FROM: HIGH TEMPERATURES AND LOW SOIL MOISTURE SYNERGISTICALLY REDUCE SWITCHGRASS YIELDS FROM MARGINAL FIELD SITES AND INHIBIT FERMENTATION

This README file was generated on 2024-01-11 by Rebecca Ong.

GENERAL INFORMATION

1. Title of Repository: Code from: High temperatures and low soil moisture synergistically reduce switchgrass yields from marginal field sites and inhibit fermentation

2. Author Information
	A. Principal Investigator Contact Information
  	 Name: Rebecca Ong	
  	 Institution: Michigan Technological University
  	 Address: Houghton, MI USA
  	 Email: rgong1@mtu.edu

3. Information about funding sources that supported this research: 
	A. Great Lakes Bioenergy Research Center, U.S. Department of Energy, Office of Science, Office of Biological and Environmental Research under Award Number DE-SC0018409
	B. National Science Foundation Long-term Ecological Research Program (DEB 1832042) at the Kellogg Biological Station, and by Michigan State University AgBioResearch
	

SHARING/ACCESS INFORMATION

1. Licenses/restrictions: CC0 1.0 Universal (CC0 1.0) Public Domain

2. Links to publications that cite or use the code:

Chipkar S, Debrauske DJ, Kahmark K, Bohm S, Hussain MZ, Joshi L, Krieg KM, Cronk B, Burke E, Cassidy J, Aguado J, Senyk A, Robertson GP, Sato TK, Hamilton SK, Thelen KD, and Ong RG. High temperatures and low soil moisture synergistically reduce switchgrass yield and inhibit biofuel production from marginal field sites. Glob. Change Biol. Bioenergy (2023)

3. Recommended citation: 

Chipkar S, Debrauske DJ, Kahmark K, Bohm S, Hussain MZ, Joshi L, Krieg KM, Cronk B, Burke E, Cassidy J, Aguado J, Senyk A, Robertson GP, Sato TK, Hamilton SK, Thelen KD, and Ong RG. (2023). Code from: High temperatures and low soil moisture synergistically reduce switchgrass yield and inhibit biofuel production from marginal field sites. Zenodo. https://doi.org/10.5281/zenodo.10278446


DETAILS ON R CODE

R Code was used for statistical analysis and to generate figures used in the paper. All files are posted in Dryad (https://doi.org/10.5061/dryad.qnk98sfps), GitHub (https://github.com/rebeccaongmtu/switchgrass-rainout-shelter-paper), and Zenodo (https://doi.org/10.5281/zenodo.10278446). All code was generated and run using RStudio Version 2023.06.1+524 (2023.06.1+524).

1. MLE_Weather.R

     R code used to generate plots and statistics for soil properties and weather data. Requires Field_Data.csv and TempHeatMap.csv files and the readr, tidyverse, lubridate, ggh4x, reshape2, viridisLite, and viridis packages.


2. FermentationDataAnalysis.R

     R code used to generate bar graphs and conducts ANOVAs for lignocellulosic biomass and hydrolysate composition and fermentation ethanol production and glucose consumption. Requires NIR_Composition_both_years.csv and FermentationHPLCData.csv and the readr, reshape2, ggplot2, ggpubr, and dplyr packages.


3. RespirometerCO2Plots.R

     R code used to aggregate data from multiple CSV files into a single dataframe and then generate faceted line plots of replicates and average CO2 production from respirometer fermentation experiments. Requires the unzipped files in Respirometer_CO2.zip stored in the same folder, and the tidyverse, ggnewscale, and ggh4x packages.


4. Survival_Analysis.R

     R code used to generate faceted survival plots annotated with p-values from a paired t-test. Requires Survival_Analysis_Data.csv and the readr, survival, ggplot2, survminer, and tidyverse packages. 

