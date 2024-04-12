################################ Will Samuel ###################################
################## Cambridge Bay stream temperature modeling ###################
############################ Started on 4/9/2024 ##############################


library(tidyverse) #for data manipulation
library(lubridate) #for data manipulation
#library(data.table) #Changes format to help the analysis run quicker
library(ggplot2)  #for plotting
library(patchwork) #for making panels
library(cowplot)  #for plotting
#library(AICcmodavg) #for model statistics
#library(rcompanion) #for pseudo R-squared
#library(boot) #For cross validation

set.seed(1)  


setwd("C:/Users/npwil/OneDrive/Desktop/UAF Research Work/UAF Research Projects/Beaver riparian resilience/Beaver_Riparian_Resilience_Wildfire_Boreal")

rm(list=ls(all=TRUE)) # clear the working environment