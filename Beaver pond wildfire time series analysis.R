################################ Will Samuel ###################################
######### Beaver Riparian Resilience to Wildfire in a boreal biome #############
############################ Started on 4/11/2024 ##############################


library(tidyverse) #for data manipulation
library(lubridate) #for data manipulation
library(ggplot2)  #for plotting
library(patchwork) #for making panels
library(cowplot)  #for plotting
library(viridis)  #For plotting color palettes
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


#I can't figure an function out to replicate this across all the datasets.... 

#input_list <- list(beaver_0, beaver_1, beaver_2, beaver_3, beaver_4, beaver_5, 
#                beaver_6, beaver_7, beaver_8, beaver_9, beaver_10, 
#                control_0, control_1, control_2, control_3, control_4, control_5, 
#                control_6, control_7, control_8, control_9, control_10)

#Write function to rename all the columns
#Pond_processing <- function(x) {
#  x <<- data.frame(x)
#  x <<- x %>% 
#    rename("dam_num" = Beaver_P_5,
#           "fire_name" = Beaver_P_2,
#           "fire_year" = Beaver_P_3) %>% 
#    select(-c(system.index, Beaver_P_1, .geo))
 
  #assign(new_name, x, envir = .GlobalEnv)
  
#  return(x)
#}

#filter(!rownames(x) %in% c("Beaver_P_1", ".geo"))
#filter(rowname != c("Beaver_P_1", ".geo") %>% 
#filter(slice(-which(rownames(x) == "3")) %>% 


#beaver_0.1 <- Pond_processing(beaver_0)
#beaver_0.1

#processed_list <- lapply(input_list, Pond_processing)



#for (i in seq_along(processed_list)) {
#  fire_name <- unique(processed_list[[i]]$fire_name)
#  file_name <- paste0(names(input_list)[i], "_", fire_name, ".csv")
#  write.csv(processed_list[[i]], file = file_name, row.names = FALSE)
#}







beaver_0 <- data.frame(beaver_0)
beaver_0_INIAKNUK <- beaver_0 %>% 
    rename("dam_num" = Beaver_P_5,
           "fire_name" = Beaver_P_2,
           "fire_year" = Beaver_P_3) %>% 
  mutate("point_type" = "treatment") %>% 
  select(-c(system.index, Beaver_P_1, .geo))

beaver_1 <- data.frame(beaver_1)
beaver_1_HOGATZA <- beaver_1 %>% 
  rename("dam_num" = Beaver_P_5,
         "fire_name" = Beaver_P_2,
         "fire_year" = Beaver_P_3) %>% 
  mutate("point_type" = "treatment") %>% 
  select(-c(system.index, Beaver_P_1, .geo))

beaver_2 <- data.frame(beaver_2)
beaver_2_HOG <- beaver_2 %>% 
  rename("dam_num" = Beaver_P_5,
         "fire_name" = Beaver_P_2,
         "fire_year" = Beaver_P_3) %>% 
  mutate("point_type" = "treatment") %>% 
  select(-c(system.index, Beaver_P_1, .geo))

beaver_3 <- data.frame(beaver_3)
beaver_3_CHALKYITSIK <- beaver_3 %>% 
  rename("dam_num" = Beaver_P_5,
         "fire_name" = Beaver_P_2,
         "fire_year" = Beaver_P_3) %>% 
  mutate("point_type" = "treatment") %>% 
  select(-c(system.index, Beaver_P_1, .geo))

beaver_4 <- data.frame(beaver_4)
beaver_4_SUNSHINE <- beaver_4 %>% 
  rename("dam_num" = Beaver_P_5,
         "fire_name" = Beaver_P_2,
         "fire_year" = Beaver_P_3) %>% 
  mutate("point_type" = "treatment") %>% 
  select(-c(system.index, Beaver_P_1, .geo))

beaver_5 <- data.frame(beaver_5)
beaver_5_MUNSON <- beaver_5 %>% 
  rename("dam_num" = Beaver_P_5,
         "fire_name" = Beaver_P_2,
         "fire_year" = Beaver_P_3) %>% 
  mutate("point_type" = "treatment") %>% 
  select(-c(system.index, Beaver_P_1, .geo))

beaver_6 <- data.frame(beaver_6)
beaver_6_PAIGE <- beaver_6 %>% 
  rename("dam_num" = Beaver_P_5,
         "fire_name" = Beaver_P_2,
         "fire_year" = Beaver_P_3) %>% 
  mutate("point_type" = "treatment") %>% 
  select(-c(system.index, Beaver_P_1, .geo))

beaver_7 <- data.frame(beaver_7)
beaver_7_HURST <- beaver_7 %>% 
  rename("dam_num" = Beaver_P_5,
         "fire_name" = Beaver_P_2,
         "fire_year" = Beaver_P_3) %>% 
  mutate("point_type" = "treatment") %>% 
  select(-c(system.index, Beaver_P_1, .geo))

beaver_8 <- data.frame(beaver_8)
beaver_8_GROUCH <- beaver_8 %>% 
  rename("dam_num" = Beaver_P_5,
         "fire_name" = Beaver_P_2,
         "fire_year" = Beaver_P_3) %>% 
  mutate("point_type" = "treatment") %>% 
  select(-c(system.index, Beaver_P_1, .geo))

beaver_9 <- data.frame(beaver_9)
beaver_9_VICTORIA <- beaver_9 %>% 
  rename("dam_num" = Beaver_P_5,
         "fire_name" = Beaver_P_2,
         "fire_year" = Beaver_P_3) %>% 
  mutate("point_type" = "treatment") %>% 
  select(-c(system.index, Beaver_P_1, .geo))

beaver_10 <- data.frame(beaver_10)
beaver_10_MUD <- beaver_10 %>% 
  rename("dam_num" = Beaver_P_5,
         "fire_name" = Beaver_P_2,
         "fire_year" = Beaver_P_3) %>% 
  mutate("point_type" = "treatment") %>% 
  select(-c(system.index, Beaver_P_1, .geo))



control_0 <- data.frame(control_0)
control_0_INIAKNUK <- control_0 %>% 
  rename("dam_num" = Beaver_P_5,
         "fire_name" = Beaver_P_2,
         "fire_year" = Beaver_P_3) %>% 
  mutate("point_type" = "control") %>% 
  select(-c(system.index, Beaver_P_1, .geo))

control_1 <- data.frame(control_1)
control_1_HOGATZA <- control_1 %>% 
  rename("dam_num" = Beaver_P_5,
         "fire_name" = Beaver_P_2,
         "fire_year" = Beaver_P_3) %>% 
  mutate("point_type" = "control") %>% 
  select(-c(system.index, Beaver_P_1, .geo))

control_2 <- data.frame(control_2)
control_2_HOG <- control_2 %>% 
  rename("dam_num" = Beaver_P_5,
         "fire_name" = Beaver_P_2,
         "fire_year" = Beaver_P_3) %>% 
  mutate("point_type" = "control") %>% 
  select(-c(system.index, Beaver_P_1, .geo))

control_3 <- data.frame(control_3)
control_3_CHALKYITSIK <- control_3 %>% 
  rename("dam_num" = Beaver_P_5,
         "fire_name" = Beaver_P_2,
         "fire_year" = Beaver_P_3) %>% 
  mutate("point_type" = "control") %>% 
  select(-c(system.index, Beaver_P_1, .geo))

control_4 <- data.frame(control_4)
control_4_SUNSHINE <- control_4 %>% 
  rename("dam_num" = Beaver_P_5,
         "fire_name" = Beaver_P_2,
         "fire_year" = Beaver_P_3) %>% 
  mutate("point_type" = "control") %>% 
  select(-c(system.index, Beaver_P_1, .geo)) 

control_5 <- data.frame(control_5)
control_5_MUNSON <- control_5 %>% 
  rename("dam_num" = Beaver_P_5,
         "fire_name" = Beaver_P_2,
         "fire_year" = Beaver_P_3) %>% 
  mutate("point_type" = "control") %>% 
  select(-c(system.index, Beaver_P_1, .geo))

control_6 <- data.frame(control_6)
control_6_PAIGE <- control_6 %>% 
  rename("dam_num" = Beaver_P_5,
         "fire_name" = Beaver_P_2,
         "fire_year" = Beaver_P_3) %>% 
  mutate("point_type" = "control") %>% 
  select(-c(system.index, Beaver_P_1, .geo))

control_7 <- data.frame(control_7)
control_7_HURST <- control_7 %>% 
  rename("dam_num" = Beaver_P_5,
         "fire_name" = Beaver_P_2,
         "fire_year" = Beaver_P_3) %>% 
  mutate("point_type" = "control") %>% 
  select(-c(system.index, Beaver_P_1, .geo))

control_8 <- data.frame(control_8)
control_8_GROUCH <- control_8 %>% 
  rename("dam_num" = Beaver_P_5,
         "fire_name" = Beaver_P_2,
         "fire_year" = Beaver_P_3) %>% 
  mutate("point_type" = "control") %>% 
  select(-c(system.index, Beaver_P_1, .geo))

control_9 <- data.frame(control_9)
control_9_VICTORIA <- control_9 %>% 
  rename("dam_num" = Beaver_P_5,
         "fire_name" = Beaver_P_2,
         "fire_year" = Beaver_P_3) %>% 
  mutate("point_type" = "control") %>% 
  select(-c(system.index, Beaver_P_1, .geo))

control_10 <- data.frame(control_10)
control_10_MUD <- control_10 %>% 
  rename("dam_num" = Beaver_P_5,
         "fire_name" = Beaver_P_2,
         "fire_year" = Beaver_P_3) %>%
  mutate("point_type" = "control") %>% 
  select(-c(system.index, Beaver_P_1, .geo))




#Separate out NDVI and NDWI

summarize_ndvi <- function(i) {
  ndvi_cols <<- grep("ndvi", names(i), ignore.case = TRUE) #Identify only the NDVI measurement columns
  ndvi_data <<- i[, ndvi_cols, drop = FALSE] #Select those columns

  t_ndvi_data <<- t(ndvi_data) #transpose the data

  t_ndvi_data <<- data.frame(row_date = rownames(t_ndvi_data), t_ndvi_data, row.names = NULL) #make the rownames the first column

  t_ndvi_data <<- t_ndvi_data %>% 
  mutate("Date" = str_extract(row_date, "\\d{4}_\\d{2}_\\d{2}") %>% as.Date(format = "%Y_%m_%d"),
         "DOY" = yday(Date)) 
  
  return(t_ndvi_data)
}


beaver_0_NDVI <- summarize_ndvi(beaver_0_INIAKNUK)
beaver_0_NDVI

beaver_1_NDVI <- summarize_ndvi(beaver_1_HOGATZA)

beaver_2_NDVI <- summarize_ndvi(beaver_2_HOG)

beaver_3_NDVI <- summarize_ndvi(beaver_3_CHALKYITSIK)

beaver_4_NDVI <- summarize_ndvi(beaver_4_SUNSHINE)

beaver_5_NDVI <- summarize_ndvi(beaver_5_MUNSON)

beaver_6_NDVI <- summarize_ndvi(beaver_6_PAIGE)

beaver_7_NDVI <- summarize_ndvi(beaver_7_HURST)

beaver_8_NDVI <- summarize_ndvi(beaver_8_GROUCH)

beaver_9_NDVI <- summarize_ndvi(beaver_9_VICTORIA)

beaver_10_NDVI <- summarize_ndvi(beaver_10_MUD)

processed_list_beaver_NDVI <- list(beaver_0_NDVI, beaver_1_NDVI, beaver_2_NDVI, beaver_3_NDVI, 
                     beaver_4_NDVI, beaver_5_NDVI, beaver_6_NDVI, beaver_7_NDVI, 
                     beaver_8_NDVI, beaver_9_NDVI, beaver_10_NDVI)

beaver_NDVI_combined <- bind_rows(processed_list_beaver_NDVI, .id = "Dataset")
str(beaver_NDVI_combined)


beaver_NDVI_long <- tidyr::pivot_longer(beaver_NDVI_combined, cols = starts_with("X"), 
                               names_to = "Point", values_to = "NDVI")

write.csv(beaver_NDVI_long, "Newdata/beaver_NDVI_long.csv")




control_0_NDVI <- summarize_ndvi(control_0_INIAKNUK)
control_0_NDVI

control_1_NDVI <- summarize_ndvi(control_1_HOGATZA)

control_2_NDVI <- summarize_ndvi(control_2_HOG)

control_3_NDVI <- summarize_ndvi(control_3_CHALKYITSIK)

control_4_NDVI <- summarize_ndvi(control_4_SUNSHINE)

control_5_NDVI <- summarize_ndvi(control_5_MUNSON)

control_6_NDVI <- summarize_ndvi(control_6_PAIGE)

control_7_NDVI <- summarize_ndvi(control_7_HURST)

control_8_NDVI <- summarize_ndvi(control_8_GROUCH)

control_9_NDVI <- summarize_ndvi(control_9_VICTORIA)

control_10_NDVI <- summarize_ndvi(control_10_MUD)


processed_list_control_NDVI <- list(control_0_NDVI, control_1_NDVI, control_2_NDVI, control_3_NDVI, 
                       control_4_NDVI, control_5_NDVI, control_6_NDVI, control_7_NDVI, 
                       control_8_NDVI, control_9_NDVI, control_10_NDVI)


control_NDVI_combined <- bind_rows(processed_list_control_NDVI, .id = "Dataset")
str(control_NDVI_combined)


control_NDVI_long <- tidyr::pivot_longer(control_NDVI_combined, cols = starts_with("X"),             names_to = "Point", values_to = "NDVI")

write.csv(control_NDVI_long, "Newdata/control_NDVI_long.csv")





#Repeat NDWI
summarize_ndwi <- function(i) {
  ndwi_cols <<- grep("ndwi", names(i), ignore.case = TRUE) #Identify only the NDVI measurement columns
  ndwi_data <<- i[, ndwi_cols, drop = FALSE] #Select those columns
  
  t_ndwi_data <<- t(ndwi_data) #transpose the data
  
  t_ndwi_data <<- data.frame(row_date = rownames(t_ndwi_data), t_ndwi_data, row.names = NULL) #make the rownames the first column
  
  t_ndwi_data <<- t_ndwi_data %>% 
    mutate("Date" = str_extract(row_date, "\\d{4}_\\d{2}_\\d{2}") %>% as.Date(format = "%Y_%m_%d"),
           "DOY" = yday(Date)) 
  
  return(t_ndwi_data)
}


beaver_0_NDWI <- summarize_ndwi(beaver_0_INIAKNUK)
beaver_0_NDWI

beaver_1_NDWI <- summarize_ndwi(beaver_1_HOGATZA)

beaver_2_NDWI <- summarize_ndwi(beaver_2_HOG)

beaver_3_NDWI <- summarize_ndwi(beaver_3_CHALKYITSIK)

beaver_4_NDWI <- summarize_ndwi(beaver_4_SUNSHINE)

beaver_5_NDWI <- summarize_ndwi(beaver_5_MUNSON)

beaver_6_NDWI <- summarize_ndwi(beaver_6_PAIGE)

beaver_7_NDWI <- summarize_ndwi(beaver_7_HURST)

beaver_8_NDWI <- summarize_ndwi(beaver_8_GROUCH)

beaver_9_NDWI <- summarize_ndwi(beaver_9_VICTORIA)

beaver_10_NDWI <- summarize_ndwi(beaver_10_MUD)


processed_list_beaver_NDWI <- list(beaver_0_NDWI, beaver_1_NDWI, beaver_2_NDWI, beaver_3_NDWI, 
                                   beaver_4_NDWI, beaver_5_NDWI, beaver_6_NDWI, beaver_7_NDWI, 
                                   beaver_8_NDWI, beaver_9_NDWI, beaver_10_NDWI)

beaver_NDWI_combined <- bind_rows(processed_list_beaver_NDWI, .id = "Dataset")
str(beaver_NDVI_combined)


beaver_NDWI_long <- tidyr::pivot_longer(beaver_NDWI_combined, cols = starts_with("X"), 
                                        names_to = "Point", values_to = "NDWI")

write.csv(beaver_NDWI_long, "Newdata/beaver_NDWI_long.csv")






control_0_NDWI <- summarize_ndwi(control_0_INIAKNUK)
control_0_NDWI

control_1_NDWI <- summarize_ndwi(control_1_HOGATZA)

control_2_NDWI <- summarize_ndwi(control_2_HOG)

control_3_NDWI <- summarize_ndwi(control_3_CHALKYITSIK)

control_4_NDWI <- summarize_ndwi(control_4_SUNSHINE)

control_5_NDWI <- summarize_ndwi(control_5_MUNSON)

control_6_NDWI <- summarize_ndwi(control_6_PAIGE)

control_7_NDWI <- summarize_ndwi(control_7_HURST)

control_8_NDWI <- summarize_ndwi(control_8_GROUCH)

control_9_NDWI <- summarize_ndwi(control_9_VICTORIA)

control_10_NDWI <- summarize_ndwi(control_10_MUD)




processed_list_control_NDWI <- list(control_0_NDWI, control_1_NDWI, control_2_NDWI, control_3_NDWI, 
                                    control_4_NDWI, control_5_NDWI, control_6_NDWI, control_7_NDWI, 
                                    control_8_NDWI, control_9_NDWI, control_10_NDWI)


control_NDWI_combined <- bind_rows(processed_list_control_NDWI, .id = "Dataset")
str(control_NDWI_combined)


control_NDWI_long <- tidyr::pivot_longer(control_NDWI_combined, cols = starts_with("X"),             
                                         names_to = "Point", values_to = "NDWI")

write.csv(control_NDWI_long, "Newdata/control_NDWI_long.csv")



# Investigate individual fires --------------------------------------------

#processed_list_control_NDWI <- list(control_0_NDWI, control_1_NDWI, control_2_NDWI, control_3_NDWI, 
#                                    control_4_NDWI, control_5_NDWI, control_6_NDWI, control_7_NDWI, 
#                                    control_8_NDWI, control_9_NDWI, control_10_NDWI)

beaver_6_NDVI
control_6_NDVI


beaver_6_NDVI_long <- tidyr::pivot_longer(beaver_6_NDVI, cols = starts_with("X"), 
                                        names_to = "Point", values_to = "NDVI")

beaver_6_NDVI_long <- beaver_6_NDVI_long %>% 
  mutate("point_type" = rep("treatment",  n = 2479))


control_6_NDVI_long <- tidyr::pivot_longer(control_6_NDVI, cols = starts_with("X"), 
                                          names_to = "Point", values_to = "NDVI")

control_6_NDVI_long <- control_6_NDVI_long %>% 
  mutate("point_type" = rep("control",  n = 2479))


combined_dataset <- bind_rows(beaver_6_NDVI_long, control_6_NDVI_long, .id = "Dataset")

combined_dataset$point_type <- as.factor(combined_dataset$point_type)
combined_dataset <- data.frame(combined_dataset)


ggplot(combined_dataset, aes(x = DOY, y = NDVI)) +  #col = factor(point_type)
  geom_smooth(aes(group = Point), se=F, col = "grey", alpha = 0.4)+
  geom_point(aes(col = Point), size = 2)+
  geom_smooth(se=T, fill = "lightblue", alpha = 0.6)+
  labs(title = "Paige Fire", x = "", y = "Normalized Difference Vegetation Index", color = "Point") +
  scale_color_viridis(discrete = TRUE) +  # Use the Viridis color palette
  theme_cowplot()+
  theme(legend.position = "none")


str(combined_dataset)

#This doesn't plot well because the treatment and control NDVI is pretty much identical for each area so they are overlapping..... 
ggplot(combined_dataset, aes(x = DOY, y = NDVI)) +  #col = factor(point_type)
  geom_smooth(aes(col = point_type, col = point_type), se = F, alpha = 0.8, linewidth = 2)+
  geom_point(aes(shape = point_type, col = point_type), size = 3, alpha = 0.8)+
  scale_color_manual(values = c("control" = "deepskyblue2", "treatment" = "red3"))+
  #geom_smooth(se=T, fill = "lightblue", alpha = 0.6)+
  labs(title = "Paige Mountain Fire", x = "", y = "Normalized Difference Vegetation Index", color = "Point") +
  #scale_color_viridis(discrete = TRUE) +  
  theme_cowplot()#+
  #theme(legend.position = "none")







#Need to repeat this for all the fires... 
#I'm going to do most of it manually cause I am under a time crunch and functions suck... 

combine_NDVI_beaver <- function(i) {
  
  x <- tidyr::pivot_longer(i, cols = starts_with("X"), 
                                            names_to = "Point", values_to = "NDVI")
  
  x <- x %>% 
    mutate("point_type" = rep("treatment",  n = nrow(x)))
  
  x$point_type <- as.factor(x$point_type)
  x <- data.frame(x)
  
  return(x)
  
}


beaver_0_NDVI_long <- combine_NDVI_beaver(beaver_0_NDVI)
beaver_1_NDVI_long <- combine_NDVI_beaver(beaver_1_NDVI)
beaver_2_NDVI_long <- combine_NDVI_beaver(beaver_2_NDVI)
#beaver_3_NDVI_long <- combine_NDVI_beaver(beaver_3_NDVI) ### I'll exclude this since it only has 1 beaver dam
beaver_4_NDVI_long <- combine_NDVI_beaver(beaver_4_NDVI)
beaver_5_NDVI_long <- combine_NDVI_beaver(beaver_5_NDVI)
beaver_6_NDVI_long <- combine_NDVI_beaver(beaver_6_NDVI)
beaver_7_NDVI_long <- combine_NDVI_beaver(beaver_7_NDVI)
beaver_8_NDVI_long <- combine_NDVI_beaver(beaver_8_NDVI)
beaver_9_NDVI_long <- combine_NDVI_beaver(beaver_9_NDVI)
beaver_10_NDVI_long <- combine_NDVI_beaver(beaver_10_NDVI)


combine_NDVI_control <- function(i) {
  
  x <- tidyr::pivot_longer(i, cols = starts_with("X"), 
                           names_to = "Point", values_to = "NDVI")
  
  x <- x %>% 
    mutate("point_type" = rep("control",  n = nrow(x)))
  
  x$point_type <- as.factor(x$point_type)
  x <- data.frame(x)
  
  return(x)
  
}

control_0_NDVI_long <- combine_NDVI_control(control_0_NDVI)
control_1_NDVI_long <- combine_NDVI_control(control_1_NDVI)
control_2_NDVI_long <- combine_NDVI_control(control_2_NDVI)
#control_3_NDVI_long <- combine_NDVI_control(control_3_NDVI) ### I'll exclude this since it only has 1 beaver dam
control_4_NDVI_long <- combine_NDVI_control(control_4_NDVI)
control_5_NDVI_long <- combine_NDVI_control(control_5_NDVI)
control_6_NDVI_long <- combine_NDVI_control(control_6_NDVI)
control_7_NDVI_long <- combine_NDVI_control(control_7_NDVI)
control_8_NDVI_long <- combine_NDVI_control(control_8_NDVI)
control_9_NDVI_long <- combine_NDVI_control(control_9_NDVI)
control_10_NDVI_long <- combine_NDVI_control(control_10_NDVI)


F0_NDVI <- bind_rows(beaver_0_NDVI_long, control_0_NDVI_long, .id = "Dataset")
F1_NDVI <- bind_rows(beaver_1_NDVI_long, control_1_NDVI_long, .id = "Dataset")
F2_NDVI <- bind_rows(beaver_2_NDVI_long, control_2_NDVI_long, .id = "Dataset")
#F3_NDVI <- bind_rows(beaver_3_NDVI_long, control_3_NDVI_long, .id = "Dataset")
F4_NDVI <- bind_rows(beaver_4_NDVI_long, control_4_NDVI_long, .id = "Dataset")
F5_NDVI <- bind_rows(beaver_5_NDVI_long, control_5_NDVI_long, .id = "Dataset")
F6_NDVI <- bind_rows(beaver_6_NDVI_long, control_6_NDVI_long, .id = "Dataset")
F7_NDVI <- bind_rows(beaver_7_NDVI_long, control_7_NDVI_long, .id = "Dataset")
F8_NDVI <- bind_rows(beaver_8_NDVI_long, control_8_NDVI_long, .id = "Dataset")
F9_NDVI <- bind_rows(beaver_9_NDVI_long, control_9_NDVI_long, .id = "Dataset")
F10_NDVI <- bind_rows(beaver_10_NDVI_long, control_10_NDVI_long, .id = "Dataset")




F0_NDVI_plot <- ggplot(F0_NDVI, aes(x = DOY, y = NDVI)) +  #col = factor(point_type)
  geom_point(aes(shape = point_type, fill = point_type, group = point_type), size = 3, alpha = 0.6, col = "black")+
  scale_shape_manual(values = c("control" = 24, "treatment" = 21)) +  # Different shapes for control and treatment
  scale_fill_manual(values = c("control" = "deepskyblue2", "treatment" = "red3"))+
  geom_smooth(aes(col = point_type, group = point_type), se = F, alpha = 0.8, linewidth = 2)+
  scale_color_manual(values = c("control" = "deepskyblue2", "treatment" = "red3"))+
  labs(title = "A (Iniaknuk Lake Fire)", x = "", y = "NDVI", 
       color = "", shape = "") +
  theme_cowplot()+
  theme(legend.position = c(0.75, 0.95))+
  guides(color = guide_legend(override.aes = list(shape = c(24, 21)),
                              title = NULL),
         fill = guide_legend(override.aes = list(shape = c(21, 24), color = c("red3", "deepskyblue2")),
                             title = NULL), 
         shape = guide_legend(override.aes = list(fill = c("red3", "deepskyblue2")),
                              title = NULL))
F0_NDVI_plot


F1_NDVI_plot <- ggplot(F1_NDVI, aes(x = DOY, y = NDVI)) +  #col = factor(point_type)
  geom_point(aes(shape = point_type, fill = point_type, group = point_type), size = 3, alpha = 0.6, col = "black")+
  scale_shape_manual(values = c("control" = 24, "treatment" = 21)) +  # Different shapes for control and treatment
  scale_fill_manual(values = c("control" = "deepskyblue2", "treatment" = "red3"))+
  geom_smooth(aes(col = point_type, group = point_type), se = F, alpha = 0.8, linewidth = 2)+
  scale_color_manual(values = c("control" = "deepskyblue2", "treatment" = "red3"))+
  labs(title = "B (Hogatza River Fire)", x = "", y = "", color = "Point") +
  theme_cowplot()+
  theme(legend.position = "none")
  

F2_NDVI_plot <- ggplot(F2_NDVI, aes(x = DOY, y = NDVI)) +  #col = factor(point_type)
  geom_point(aes(shape = point_type, fill = point_type, group = point_type), size = 3, alpha = 0.6, col = "black")+
  scale_shape_manual(values = c("control" = 24, "treatment" = 21)) +  # Different shapes for control and treatment
  scale_fill_manual(values = c("control" = "deepskyblue2", "treatment" = "red3"))+
  geom_smooth(aes(col = point_type, group = point_type), se = F, alpha = 0.8, linewidth = 2)+
  scale_color_manual(values = c("control" = "deepskyblue2", "treatment" = "red3"))+
  labs(title = "C (Hog Fire)", x = "", y = "", color = "Point") +
  theme_cowplot()+
  theme(legend.position = "none")

#Excluded since it only has 1 dam
#F3_NDVI_plot <- ggplot(F3_NDVI, aes(x = DOY, y = NDVI)) +  #col = factor(point_type)
#  geom_point(aes(shape = point_type, fill = point_type, group = point_type), size = 4, alpha = 0.6, col = "black")+
#  scale_shape_manual(values = c("control" = 24, "treatment" = 21)) +  # Different shapes for control and treatment
#  scale_fill_manual(values = c("control" = "deepskyblue2", "treatment" = "red3"))+
#  geom_smooth(aes(col = point_type, group = point_type), se = F, alpha = 0.8, linewidth = 2)+
#  scale_color_manual(values = c("control" = "deepskyblue2", "treatment" = "red3"))+
#  labs(title = "Sunshine Mountain Fire", x = "", y = "Normalized Difference Vegetation Index", color = "Point") +
#  theme_cowplot()+
#  theme(legend.position = "none")

F4_NDVI_plot <- ggplot(F4_NDVI, aes(x = DOY, y = NDVI)) +  #col = factor(point_type)
  geom_point(aes(shape = point_type, fill = point_type, group = point_type), size = 3, alpha = 0.6, col = "black")+
  scale_shape_manual(values = c("control" = 24, "treatment" = 21)) +  # Different shapes for control and treatment
  scale_fill_manual(values = c("control" = "deepskyblue2", "treatment" = "red3"))+
  geom_smooth(aes(col = point_type, group = point_type), se = F, alpha = 0.8, linewidth = 2)+
  scale_color_manual(values = c("control" = "deepskyblue2", "treatment" = "red3"))+
  labs(title = "D (Sunshine Mountain Fire)", x = "", y = "NDVI", color = "Point") +
  theme_cowplot()+
  theme(legend.position = "none")

F5_NDVI_plot <- ggplot(F5_NDVI, aes(x = DOY, y = NDVI)) +  #col = factor(point_type)
  geom_point(aes(shape = point_type, fill = point_type, group = point_type), size = 3, alpha = 0.6, col = "black")+
  scale_shape_manual(values = c("control" = 24, "treatment" = 21)) +  # Different shapes for control and treatment
  scale_fill_manual(values = c("control" = "deepskyblue2", "treatment" = "red3"))+
  geom_smooth(aes(col = point_type, group = point_type), se = F, alpha = 0.8, linewidth = 2)+
  scale_color_manual(values = c("control" = "deepskyblue2", "treatment" = "red3"))+
  labs(title = "E (Munson Creek Fire)", x = "", y = "", color = "Point") +
  theme_cowplot()+
  theme(legend.position = "none")


F6_NDVI_plot <- ggplot(F6_NDVI, aes(x = DOY, y = NDVI)) +  #col = factor(point_type)
  geom_point(aes(shape = point_type, fill = point_type, group = point_type), size = 3, alpha = 0.6, col = "black")+
  scale_shape_manual(values = c("control" = 24, "treatment" = 21)) +  # Different shapes for control and treatment
  scale_fill_manual(values = c("control" = "deepskyblue2", "treatment" = "red3"))+
  geom_smooth(aes(col = point_type, group = point_type), se = F, alpha = 0.8, linewidth = 2)+
  scale_color_manual(values = c("control" = "deepskyblue2", "treatment" = "red3"))+
  labs(title = "F (Paige Mountain Fire)", x = "", y = "", color = "Point") +
  theme_cowplot()+
  theme(legend.position = "none")

F7_NDVI_plot <- ggplot(F7_NDVI, aes(x = DOY, y = NDVI)) +  #col = factor(point_type)
  geom_point(aes(shape = point_type, fill = point_type, group = point_type), size = 3, alpha = 0.6, col = "black")+
  scale_shape_manual(values = c("control" = 24, "treatment" = 21)) +  # Different shapes for control and treatment
  scale_fill_manual(values = c("control" = "deepskyblue2", "treatment" = "red3"))+
  geom_smooth(aes(col = point_type, group = point_type), se = F, alpha = 0.8, linewidth = 2)+
  scale_color_manual(values = c("control" = "deepskyblue2", "treatment" = "red3"))+
  labs(title = "G (Hurst Creek Fire)", x = "", y = "NDVI", color = "Point") +
  theme_cowplot()+
  theme(legend.position = "none")

F8_NDVI_plot <- ggplot(F8_NDVI, aes(x = DOY, y = NDVI)) +  #col = factor(point_type)
  geom_point(aes(shape = point_type, fill = point_type, group = point_type), size = 3, alpha = 0.6, col = "black")+
  scale_shape_manual(values = c("control" = 24, "treatment" = 21)) +  # Different shapes for control and treatment
  scale_fill_manual(values = c("control" = "deepskyblue2", "treatment" = "red3"))+
  geom_smooth(aes(col = point_type, group = point_type), se = F, alpha = 0.8, linewidth = 2)+
  scale_color_manual(values = c("control" = "deepskyblue2", "treatment" = "red3"))+
  labs(title = "H (Old Grouch Top Fire)", x = "", y = "", color = "Point") +
  theme_cowplot()+
  theme(legend.position = "none")

F9_NDVI_plot <- ggplot(F9_NDVI, aes(x = DOY, y = NDVI)) +  #col = factor(point_type)
  geom_point(aes(shape = point_type, fill = point_type, group = point_type), size = 3, alpha = 0.6, col = "black")+
  scale_shape_manual(values = c("control" = 24, "treatment" = 21)) +  # Different shapes for control and treatment
  scale_fill_manual(values = c("control" = "deepskyblue2", "treatment" = "red3"))+
  geom_smooth(aes(col = point_type, fill = point_type, group = point_type), se = T, alpha = 0.4, linewidth = 2)+
  scale_color_manual(values = c("control" = "deepskyblue2", "treatment" = "red3"))+
  labs(title = "I (Victoria Mountain Fire)", x = "", y = "", color = "Point") +
  theme_cowplot()+
  theme(legend.position = "none")

F10_NDVI_plot <- ggplot(F10_NDVI, aes(x = DOY, y = NDVI)) +  #col = factor(point_type)
  geom_point(aes(shape = point_type, fill = point_type, group = point_type), size = 3, alpha = 0.6, col = "black")+
  scale_shape_manual(values = c("control" = 24, "treatment" = 21)) +  # Different shapes for control and treatment
  scale_fill_manual(values = c("control" = "deepskyblue2", "treatment" = "red3"))+
  geom_smooth(aes(col = point_type, group = point_type), se = F, alpha = 0.8, linewidth = 2)+
  scale_color_manual(values = c("control" = "deepskyblue2", "treatment" = "red3"))+
  labs(title = "J (Little Mud River Fire)", x = "Day of Year", y = "NDVI", color = "Point") +
  theme_cowplot()+
  theme(legend.position = "none")




panel <- list(F0_NDVI_plot, F1_NDVI_plot, F2_NDVI_plot,
                F4_NDVI_plot, F5_NDVI_plot, F6_NDVI_plot,
                F7_NDVI_plot, F8_NDVI_plot, F9_NDVI_plot,
                F10_NDVI_plot)

individual_fires_NDVI <- wrap_plots(panel, nrow = 4)
individual_fires_NDVI

ggsave(plot = individual_fires_NDVI, 
       "Figures/individual_fires_NDVI.jpeg", 
       width = 40, 
       height = 35,
       units = "cm",
       dpi = 300)






### Do it all again for NDWI

combine_NDWI_beaver <- function(i) {
  
  x <- tidyr::pivot_longer(i, cols = starts_with("X"), 
                           names_to = "Point", values_to = "NDWI")
  
  x <- x %>% 
    mutate("point_type" = rep("treatment",  n = nrow(x)))
  
  x$point_type <- as.factor(x$point_type)
  x <- data.frame(x)
  
  return(x)
  
}


beaver_0_NDWI_long <- combine_NDWI_beaver(beaver_0_NDWI)
beaver_1_NDWI_long <- combine_NDWI_beaver(beaver_1_NDWI)
beaver_2_NDWI_long <- combine_NDWI_beaver(beaver_2_NDWI)
#beaver_3_NDVI_long <- combine_NDWI_beaver(beaver_3_NDWI) ### I'll exclude this since it only has 1 beaver dam
beaver_4_NDWI_long <- combine_NDWI_beaver(beaver_4_NDWI)
beaver_5_NDWI_long <- combine_NDWI_beaver(beaver_5_NDWI)
beaver_6_NDWI_long <- combine_NDWI_beaver(beaver_6_NDWI)
beaver_7_NDWI_long <- combine_NDWI_beaver(beaver_7_NDWI)
beaver_8_NDWI_long <- combine_NDWI_beaver(beaver_8_NDWI)
beaver_9_NDWI_long <- combine_NDWI_beaver(beaver_9_NDWI)
beaver_10_NDWI_long <- combine_NDWI_beaver(beaver_10_NDWI)


combine_NDWI_control <- function(i) {
  
  x <- tidyr::pivot_longer(i, cols = starts_with("X"), 
                           names_to = "Point", values_to = "NDWI")
  
  x <- x %>% 
    mutate("point_type" = rep("control",  n = nrow(x)))
  
  x$point_type <- as.factor(x$point_type)
  x <- data.frame(x)
  
  return(x)
  
}

control_0_NDWI_long <- combine_NDWI_control(control_0_NDWI)
control_1_NDWI_long <- combine_NDWI_control(control_1_NDWI)
control_2_NDWI_long <- combine_NDWI_control(control_2_NDWI)
#control_3_NDWI_long <- combine_NDWI_control(control_3_NDWI) ### I'll exclude this since it only has 1 beaver dam
control_4_NDWI_long <- combine_NDWI_control(control_4_NDWI)
control_5_NDWI_long <- combine_NDWI_control(control_5_NDWI)
control_6_NDWI_long <- combine_NDWI_control(control_6_NDWI)
control_7_NDWI_long <- combine_NDWI_control(control_7_NDWI)
control_8_NDWI_long <- combine_NDWI_control(control_8_NDWI)
control_9_NDWI_long <- combine_NDWI_control(control_9_NDWI)
control_10_NDWI_long <- combine_NDWI_control(control_10_NDWI)


F0_NDWI <- bind_rows(beaver_0_NDWI_long, control_0_NDWI_long, .id = "Dataset")
F1_NDWI <- bind_rows(beaver_1_NDWI_long, control_1_NDWI_long, .id = "Dataset")
F2_NDWI <- bind_rows(beaver_2_NDWI_long, control_2_NDWI_long, .id = "Dataset")
#F3_NDWI <- bind_rows(beaver_3_NDWI_long, control_3_NDWI_long, .id = "Dataset")
F4_NDWI <- bind_rows(beaver_4_NDWI_long, control_4_NDWI_long, .id = "Dataset")
F5_NDWI <- bind_rows(beaver_5_NDWI_long, control_5_NDWI_long, .id = "Dataset")
F6_NDWI <- bind_rows(beaver_6_NDWI_long, control_6_NDWI_long, .id = "Dataset")
F7_NDWI <- bind_rows(beaver_7_NDWI_long, control_7_NDWI_long, .id = "Dataset")
F8_NDWI <- bind_rows(beaver_8_NDWI_long, control_8_NDWI_long, .id = "Dataset")
F9_NDWI <- bind_rows(beaver_9_NDWI_long, control_9_NDWI_long, .id = "Dataset")
F10_NDWI <- bind_rows(beaver_10_NDWI_long, control_10_NDWI_long, .id = "Dataset")




F0_NDWI_plot <- ggplot(F0_NDWI, aes(x = DOY, y = NDWI)) +  #col = factor(point_type)
  geom_point(aes(shape = point_type, fill = point_type, group = point_type), size = 3, alpha = 0.6, col = "black")+
  scale_shape_manual(values = c("control" = 24, "treatment" = 21)) +  # Different shapes for control and treatment
  scale_fill_manual(values = c("control" = "deepskyblue2", "treatment" = "red3"))+
  geom_smooth(aes(col = point_type, group = point_type), se = F, alpha = 0.8, linewidth = 2)+
  scale_color_manual(values = c("control" = "deepskyblue2", "treatment" = "red3"))+
  labs(title = "A (Iniaknuk Lake Fire)", x = "", y = "NDWI", 
       color = "", shape = "") +
  theme_cowplot()+
  theme(legend.position = c(0.75, 0.15))+
  guides(color = guide_legend(override.aes = list(shape = c(24, 21)),
                              title = NULL),
         fill = guide_legend(override.aes = list(shape = c(21, 24), color = c("red3", "deepskyblue2")),
                             title = NULL), 
         shape = guide_legend(override.aes = list(fill = c("red3", "deepskyblue2")),
                              title = NULL))
F0_NDWI_plot


F1_NDWI_plot <- ggplot(F1_NDWI, aes(x = DOY, y = NDWI)) +  #col = factor(point_type)
  geom_point(aes(shape = point_type, fill = point_type, group = point_type), size = 3, alpha = 0.6, col = "black")+
  scale_shape_manual(values = c("control" = 24, "treatment" = 21)) +  # Different shapes for control and treatment
  scale_fill_manual(values = c("control" = "deepskyblue2", "treatment" = "red3"))+
  geom_smooth(aes(col = point_type, group = point_type), se = F, alpha = 0.8, linewidth = 2)+
  scale_color_manual(values = c("control" = "deepskyblue2", "treatment" = "red3"))+
  labs(title = "B (Hogatza River Fire)", x = "", y = "", color = "Point") +
  theme_cowplot()+
  theme(legend.position = "none")


F2_NDWI_plot <- ggplot(F2_NDWI, aes(x = DOY, y = NDWI)) +  #col = factor(point_type)
  geom_point(aes(shape = point_type, fill = point_type, group = point_type), size = 3, alpha = 0.6, col = "black")+
  scale_shape_manual(values = c("control" = 24, "treatment" = 21)) +  # Different shapes for control and treatment
  scale_fill_manual(values = c("control" = "deepskyblue2", "treatment" = "red3"))+
  geom_smooth(aes(col = point_type, group = point_type), se = F, alpha = 0.8, linewidth = 2)+
  scale_color_manual(values = c("control" = "deepskyblue2", "treatment" = "red3"))+
  labs(title = "C (Hog Fire)", x = "", y = "", color = "Point") +
  theme_cowplot()+
  theme(legend.position = "none")

#Excluded since it only has 1 dam
#F3_NDVI_plot <- ggplot(F3_NDVI, aes(x = DOY, y = NDVI)) +  #col = factor(point_type)
#  geom_point(aes(shape = point_type, fill = point_type, group = point_type), size = 4, alpha = 0.6, col = "black")+
#  scale_shape_manual(values = c("control" = 24, "treatment" = 21)) +  # Different shapes for control and treatment
#  scale_fill_manual(values = c("control" = "deepskyblue2", "treatment" = "red3"))+
#  geom_smooth(aes(col = point_type, group = point_type), se = F, alpha = 0.8, linewidth = 2)+
#  scale_color_manual(values = c("control" = "deepskyblue2", "treatment" = "red3"))+
#  labs(title = "Sunshine Mountain Fire", x = "", y = "Normalized Difference Vegetation Index", color = "Point") +
#  theme_cowplot()+
#  theme(legend.position = "none")

F4_NDWI_plot <- ggplot(F4_NDWI, aes(x = DOY, y = NDWI)) +  #col = factor(point_type)
  geom_point(aes(shape = point_type, fill = point_type, group = point_type), size = 3, alpha = 0.6, col = "black")+
  scale_shape_manual(values = c("control" = 24, "treatment" = 21)) +  # Different shapes for control and treatment
  scale_fill_manual(values = c("control" = "deepskyblue2", "treatment" = "red3"))+
  geom_smooth(aes(col = point_type, group = point_type), se = F, alpha = 0.8, linewidth = 2)+
  scale_color_manual(values = c("control" = "deepskyblue2", "treatment" = "red3"))+
  labs(title = "D (Sunshine Mountain Fire)", x = "", y = "NDWI", color = "Point") +
  theme_cowplot()+
  theme(legend.position = "none")

F5_NDWI_plot <- ggplot(F5_NDWI, aes(x = DOY, y = NDWI)) +  #col = factor(point_type)
  geom_point(aes(shape = point_type, fill = point_type, group = point_type), size = 3, alpha = 0.6, col = "black")+
  scale_shape_manual(values = c("control" = 24, "treatment" = 21)) +  # Different shapes for control and treatment
  scale_fill_manual(values = c("control" = "deepskyblue2", "treatment" = "red3"))+
  geom_smooth(aes(col = point_type, group = point_type), se = F, alpha = 0.8, linewidth = 2)+
  scale_color_manual(values = c("control" = "deepskyblue2", "treatment" = "red3"))+
  labs(title = "E (Munson Creek Fire)", x = "", y = "", color = "Point") +
  theme_cowplot()+
  theme(legend.position = "none")


F6_NDWI_plot <- ggplot(F6_NDWI, aes(x = DOY, y = NDWI)) +  #col = factor(point_type)
  geom_point(aes(shape = point_type, fill = point_type, group = point_type), size = 3, alpha = 0.6, col = "black")+
  scale_shape_manual(values = c("control" = 24, "treatment" = 21)) +  # Different shapes for control and treatment
  scale_fill_manual(values = c("control" = "deepskyblue2", "treatment" = "red3"))+
  geom_smooth(aes(col = point_type, group = point_type), se = F, alpha = 0.8, linewidth = 2)+
  scale_color_manual(values = c("control" = "deepskyblue2", "treatment" = "red3"))+
  labs(title = "F (Paige Mountain Fire)", x = "", y = "", color = "Point") +
  theme_cowplot()+
  theme(legend.position = "none")

F7_NDWI_plot <- ggplot(F7_NDWI, aes(x = DOY, y = NDWI)) +  #col = factor(point_type)
  geom_point(aes(shape = point_type, fill = point_type, group = point_type), size = 3, alpha = 0.6, col = "black")+
  scale_shape_manual(values = c("control" = 24, "treatment" = 21)) +  # Different shapes for control and treatment
  scale_fill_manual(values = c("control" = "deepskyblue2", "treatment" = "red3"))+
  geom_smooth(aes(col = point_type, group = point_type), se = F, alpha = 0.8, linewidth = 2)+
  scale_color_manual(values = c("control" = "deepskyblue2", "treatment" = "red3"))+
  labs(title = "G (Hurst Creek Fire)", x = "", y = "NDWI", color = "Point") +
  theme_cowplot()+
  theme(legend.position = "none")

F8_NDWI_plot <- ggplot(F8_NDWI, aes(x = DOY, y = NDWI)) +  #col = factor(point_type)
  geom_point(aes(shape = point_type, fill = point_type, group = point_type), size = 3, alpha = 0.6, col = "black")+
  scale_shape_manual(values = c("control" = 24, "treatment" = 21)) +  # Different shapes for control and treatment
  scale_fill_manual(values = c("control" = "deepskyblue2", "treatment" = "red3"))+
  geom_smooth(aes(col = point_type, group = point_type), se = F, alpha = 0.8, linewidth = 2)+
  scale_color_manual(values = c("control" = "deepskyblue2", "treatment" = "red3"))+
  labs(title = "H (Old Grouch Top Fire)", x = "", y = "", color = "Point") +
  theme_cowplot()+
  theme(legend.position = "none")

F9_NDWI_plot <- ggplot(F9_NDWI, aes(x = DOY, y = NDWI)) +  #col = factor(point_type)
  geom_point(aes(shape = point_type, fill = point_type, group = point_type), size = 3, alpha = 0.6, col = "black")+
  scale_shape_manual(values = c("control" = 24, "treatment" = 21)) +  # Different shapes for control and treatment
  #scale_fill_manual(values = c("control" = "deepskyblue2", "treatment" = "red3"))+
  geom_smooth(aes(col = point_type, fill = point_type, group = point_type),  alpha = 0.4, linewidth = 2)+
  scale_color_manual(values = c("control" = "deepskyblue2", "treatment" = "red3"))+
  labs(title = "I (Victoria Mountain Fire)", x = "", y = "", color = "Point") +
  theme_cowplot()+
  theme(legend.position = "none")
F9_NDWI_plot

F10_NDWI_plot <- ggplot(F10_NDWI, aes(x = DOY, y = NDWI)) +  #col = factor(point_type)
  geom_point(aes(shape = point_type, fill = point_type, group = point_type), size = 3, alpha = 0.6, col = "black")+
  scale_shape_manual(values = c("control" = 24, "treatment" = 21)) +  # Different shapes for control and treatment
  scale_fill_manual(values = c("control" = "deepskyblue2", "treatment" = "red3"))+
  geom_smooth(aes(col = point_type, group = point_type), se = F, alpha = 0.8, linewidth = 2)+
  scale_color_manual(values = c("control" = "deepskyblue2", "treatment" = "red3"))+
  labs(title = "J (Little Mud River Fire)", x = "Day of Year", y = "NDWI", color = "Point") +
  theme_cowplot()+
  theme(legend.position = "none")




panel <- list(F0_NDWI_plot, F1_NDWI_plot, F2_NDWI_plot,
              F4_NDWI_plot, F5_NDWI_plot, F6_NDWI_plot,
              F7_NDWI_plot, F8_NDWI_plot, F9_NDWI_plot,
              F10_NDWI_plot)

individual_fires_NDWI <- wrap_plots(panel, nrow = 4)
individual_fires_NDWI

ggsave(plot = individual_fires_NDWI, 
       "Figures/individual_fires_NDWI.jpeg", 
       width = 40, 
       height = 35,
       units = "cm",
       dpi = 300)











# Plot the time series ----------------------------------------------------

#Read in the data
beaver_NDVI_long <- read.csv("Newdata/beaver_NDVI_long.csv")
control_NDVI_long <- read.csv("Newdata/control_NDVI_long.csv")
beaver_NDWI_long <- read.csv("Newdata/beaver_NDWI_long.csv")
control_NDWI_long <- read.csv("Newdata/control_NDWI_long.csv")





beaver_NDVI_plot <- ggplot(beaver_NDVI_long, aes(x = DOY, y = NDVI, group = Point, color = Point)) +
  geom_point()+
  geom_smooth(se = F,)+
  labs(title = "K (Beaver Ponds)", x = "Day of Year", y = "", color = "Point") + #Normalized Difference Vegetation Index
  scale_color_viridis(discrete = TRUE) +  # Use the Viridis color palette
  theme_cowplot()+
  theme(legend.position = "none")
beaver_NDVI_plot


control_NDVI_plot <- ggplot(control_NDVI_long, aes(x = DOY, y = NDVI, group = Point, color = Point)) +
  geom_point()+
  geom_smooth(se = F)+
  labs(title = "L (Control)", x = "Day of Year", y = "", color = "Point") +
  scale_color_viridis(discrete = TRUE) +  # Use the Viridis color palette
  theme_cowplot()+
  theme(legend.position = "none")
#control_NDVI_plot

#beaver_NDVI_plot+control_NDVI_plot

beaver_NDWI_plot <- ggplot(beaver_NDWI_long, aes(x = DOY, y = NDWI, group = Point, color = Point)) +
  geom_point()+
  geom_smooth(se = F)+
  labs(title = "K (Beaver Ponds)", x = "Day of Year", y = "", color = "Point") +
  scale_color_viridis(option="G", discrete = TRUE) +
  theme_cowplot()+
  theme(legend.position = "none")
#beaver_NDWI_plot


control_NDWI_plot <- ggplot(control_NDWI_long, aes(x = DOY, y = NDWI, group = Point, color = Point)) +
  geom_point()+
  geom_smooth(se = F)+
  labs(title = "L (Control)", x = "Day of Year", y = "", color = "Point") +
  scale_color_viridis(option="G", discrete = TRUE) +
  theme_cowplot()+
  theme(legend.position = "none")
control_NDWI_plot

#beaver_NDWI_plot+control_NDWI_plot


Time_series_panel_1 <- (beaver_NDVI_plot+control_NDVI_plot)/(beaver_NDWI_plot+control_NDWI_plot)
Time_series_panel_1

ggsave(plot = Time_series_panel_1, 
       "Figures/Time_series_panel_1.jpeg", 
       width = 30, 
       height = 20,
       units = "cm",
       dpi = 300)





individual_fires_NDVI_2 <- (F0_NDVI_plot + F1_NDVI_plot + F2_NDVI_plot) /
                           (F4_NDVI_plot + F5_NDVI_plot + F6_NDVI_plot) /
                           (F7_NDVI_plot + F8_NDVI_plot + F9_NDVI_plot) /
                           (F10_NDVI_plot + beaver_NDVI_plot + control_NDVI_plot)


#Make sure to adjust the axis labels on the new plots before saving
ggsave(plot = individual_fires_NDVI_2, 
       "Figures/individual_fires_NDVI_2.jpeg", 
       width = 40, 
       height = 35,
       units = "cm",
       dpi = 300)




individual_fires_NDWI_2 <- 
  (F0_NDWI_plot + F1_NDWI_plot + F2_NDWI_plot) /
  (F4_NDWI_plot + F5_NDWI_plot + F6_NDWI_plot) /
  (F7_NDWI_plot + F8_NDWI_plot + F9_NDWI_plot) /
  (F10_NDWI_plot + beaver_NDWI_plot + control_NDWI_plot)


#Make sure to adjust the axis labels on the new plots before saving
ggsave(plot = individual_fires_NDWI_2, 
       "Figures/individual_fires_NDWI_2.jpeg", 
       width = 40, 
       height = 35,
       units = "cm",
       dpi = 300)








#Plot the overall change (mean across all beaver ponds)

beaver_NDVI_plot_mean <- ggplot(beaver_NDVI_long, aes(x = DOY, y = NDVI)) +
  geom_point(size = 2, shape = 21, fill = "green4", color = "black", alpha = 0.8) +
  geom_smooth()+
  labs(title = "Beaver Ponds", x = "", y = "Normalized Difference Vegetation Index", color = "Point") +
  #scale_color_viridis(discrete = TRUE) +  # Use the Viridis color palette
  theme_cowplot()+
  theme(legend.position = "none")
#beaver_NDVI_plot_mean


control_NDVI_plot_mean <- ggplot(control_NDVI_long, aes(x = DOY, y = NDVI)) +
  geom_point(size = 2, shape = 21, fill = "green4", color = "black", alpha = 0.8) +
  geom_smooth()+
  labs(title = "Control", x = "", y = "", color = "Point") +
  scale_color_viridis(discrete = TRUE) +  # Use the Viridis color palette
  theme_cowplot()+
  theme(legend.position = "none")


beaver_NDWI_plot_mean <- ggplot(beaver_NDWI_long, aes(x = DOY, y = NDWI)) +
  geom_point(size = 2, shape = 21, fill = "lightblue3", color = "black", alpha = 0.8) +
  geom_smooth(col = "black")+
  labs(title = "Beaver Ponds", x = "Day of Year", y = "Normalized Difference Water Index", color = "Point") +
  scale_color_viridis(option="G", discrete = TRUE) +
  theme_cowplot()+
  theme(legend.position = "none")


control_NDVWI_plot_mean <- ggplot(control_NDWI_long, aes(x = DOY, y = NDWI)) +
  geom_point(size = 2, shape = 21, fill = "lightblue3", color = "black", alpha = 0.8) +
  geom_smooth(col = "black")+
  labs(title = "Control", x = "Day of Year", y = "", color = "Point") +
  scale_color_viridis(option="G", discrete = TRUE) +
  theme_cowplot()+
  theme(legend.position = "none")


Time_series_panel_2 <- (beaver_NDVI_plot_mean+control_NDVI_plot_mean)/(beaver_NDWI_plot_mean+control_NDVWI_plot_mean)
Time_series_panel_2

ggsave(plot = Time_series_panel_2, 
       "Figures/Time_series_panel_2.jpeg", 
       width = 30, 
       height = 20,
       units = "cm",
       dpi = 300)










# Summarize the data for each wildfire ------------------------------------




# Analyzing the time series -----------------------------------------------






# T-test and ANOVA's ------------------------------------------------------






















