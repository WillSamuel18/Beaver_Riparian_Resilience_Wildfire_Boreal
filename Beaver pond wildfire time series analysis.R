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



# Data prep ---------------------------------------------------------------

beaver_0 <- read.csv("Data/beavers0.csv")
beaver_1 <- read.csv("Data/beavers1.csv")
beaver_2 <- read.csv("Data/beavers2.csv")
beaver_3 <- read.csv("Data/beavers3.csv")
beaver_4 <- read.csv("Data/beavers4.csv")
beaver_5 <- read.csv("Data/beavers5.csv")
beaver_6 <- read.csv("Data/beavers6.csv")
beaver_7 <- read.csv("Data/beavers7.csv")
beaver_8 <- read.csv("Data/beavers8.csv")
beaver_9 <- read.csv("Data/beavers9.csv")
beaver_10 <- read.csv("Data/beavers10.csv")

control_0 <- read.csv("Data/controls0.csv")
control_1 <- read.csv("Data/controls1.csv")
control_2 <- read.csv("Data/controls2.csv")
control_3 <- read.csv("Data/controls3.csv")
control_4 <- read.csv("Data/controls4.csv")
control_5 <- read.csv("Data/controls5.csv")
control_6 <- read.csv("Data/controls6.csv")
control_7 <- read.csv("Data/controls7.csv")
control_8 <- read.csv("Data/controls8.csv")
control_9 <- read.csv("Data/controls9.csv")
control_10 <- read.csv("Data/controls10.csv")




input_list <- list(beaver_0, beaver_1, beaver_2, beaver_3, beaver_4, beaver_5, 
                beaver_6, beaver_7, beaver_8, beaver_9, beaver_10, 
                control_0, control_1, control_2, control_3, control_4, control_5, 
                control_6, control_7, control_8, control_9, control_10)

#Write function to rename all the columns
Pond_processing <- function(x) {
  x <- data.frame(x)
  x <- x %>% 
    rename("dam_num" = Beaver_P_5,
           "fire_name" = Beaver_P_2,
           "fire_year" = Beaver_P_3) %>% 
    select(-c(system.index, Beaver_P_1, .geo))
    #filter(!rownames(x) %in% c("Beaver_P_1", ".geo"))
    #filter(rowname != c("Beaver_P_1", ".geo") %>% 
    #filter(slice(-which(rownames(x) == "3")) %>% 
  assign(new_name, x, envir = .GlobalEnv)
  
  return(x)
}


beaver_0.1 <- Pond_processing(beaver_0)
beaver_0.1

processed_list <- lapply(input_list, Pond_processing)







# Plot the time series ----------------------------------------------------





# T-test and ANOVA's ------------------------------------------------------






















