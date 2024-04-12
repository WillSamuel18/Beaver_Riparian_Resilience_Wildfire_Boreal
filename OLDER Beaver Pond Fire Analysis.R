################################################################################
#################### Beaver Pond and Wildfire Analysis #########################
############################### Will Samuel ####################################
################################################################################

library(ggplot2)
library(ggthemes)
library(patchwork)
library(tidyverse)
library(qpcR)
library(dplyr)



setwd("C:/Users/npwil/OneDrive/Desktop/School/Grad School/Coursework/NRM 641 Remote Sensing/Final Project/Data and R")

BPB_dNDVI <- read.csv("Beaver_Pond_Buffer_dNDVI.csv")
BPB_dNDWI <- read.csv("Beaver_Pond_Buffer_dNDWI.csv")
VB_dNDVI <- read.csv("Valley_Bottom_dNDVI.csv")
VB_dNDWI <- read.csv("Valley_Bottom_dNDWI.csv")



# Data Organization and Manipulation --------------------------------------

#Pull out column 2, so you only keep the data
BPB_dNDVI <- data.frame(BPB_dNDVI[,-c(1,3)])
BPB_dNDWI <- data.frame(BPB_dNDWI[,-c(1,3)])
VB_dNDVI <- data.frame(VB_dNDVI[,-c(1,3)])
VB_dNDWI <- data.frame(VB_dNDWI[,-c(1,3)])

#Classify the data
BPB_dNDVI$Type <- c("Beaver Pond Buffer")
BPB_dNDWI$Type  <- c("Beaver Pond Buffer")
VB_dNDVI$Type  <- c("Valley Bottom")
VB_dNDWI$Type  <- c("Valley Bottom")

#Rename Columns to combine
names(BPB_dNDVI)[1] <- 'Value'
names(BPB_dNDWI)[1] <- 'Value'
names(VB_dNDVI)[1] <- 'Value'
names(VB_dNDWI)[1] <- 'Value'


#Make two data sets
dNDVI <-rbind(BPB_dNDVI, VB_dNDVI)
head(dNDVI)
dNDWI <-rbind(BPB_dNDWI, VB_dNDWI)
head(dNDWI)

#Put all the data into one dataframe  -- I found a better way to do this above
#data <-qpcR:::cbind.na(BPB_dNDVI, BPB_dNDWI, VB_dNDVI, VB_dNDWI)
#names(data) <- c("BPB_dNDVI", "BPB_dNDWI", "VB_dNDVI", "VB_dNDWI")
#head(data)
#View(data)


# Analysis ----------------------------------------------------------------
#dNDVI

t.test(BPB_dNDVI$Value, VB_dNDVI$Value)

BPB_dNDVI_mean <- mean(BPB_dNDVI$Value)
VB_dNDVI_mean <- mean(VB_dNDVI$Value)


p1 <- ggplot(dNDVI, aes(Type, Value, fill = Type))+
  geom_boxplot(fill = c("lightgreen", "lightblue"))+
  #geom_jitter(color = "darkgrey",  alpha=0.5)+
  #geom_hline(yintercept = BPB_dNDVI_mean, color = "lightgreen", lwd = 2)+
  #geom_hline(yintercept = VB_dNDVI_mean, color = "lightblue", lwd = 2)+
  #geom_hline(yintercept = VB_dNDVI_mean, color = "red",
  #           lwd = 1.5, linetype = "dashed")+
  geom_hline(yintercept = 0, color = "red",
              lwd = 1.5, linetype = "dashed")+
  theme_classic()+
  theme(legend.position="none")+
  theme(axis.title = element_text(size=14),
        axis.text = element_text(size=13, color = "grey35"),
        plot.title = element_text(size=18, face = "bold"))+
        #legend.position = c(0.2, 0.89), 
        #legend.title = element_text(size=14), 
        #legend.text = element_text(size=12),)+
  labs(title = "A                   dNDVI", 
       x = "Area Class", y = "dNDVI Raster Value")+
  #scale_y_continuous(limits = c(-600,700), n.breaks = 13)+
  annotate(geom="text", x=1, y=-480, label="P-value < 0.0001", size = 4.5)
  
p1 
  

#dNDWI

t.test(BPB_dNDWI$Value, VB_dNDWI$Value)

  BPB_dNDWI_mean <- mean(BPB_dNDWI$Value)
VB_dNDWI_mean <- mean(VB_dNDWI$Value)

#NDWI_Change <- BPB_dNDWI_mean+VB_dNDWI_mean/2


p2 <- ggplot(dNDWI, aes(Type, Value))+
  geom_boxplot(fill = c("lightgreen", "lightblue"))+
  #geom_jitter(color = "darkgrey",  alpha=0.5)+
  #geom_hline(yintercept = BPB_dNDVI_mean, color = "lightgreen", lwd = 2)+
  #geom_hline(yintercept = VB_dNDVI_mean, color = "lightblue", lwd = 2)+
  #geom_hline(yintercept = VB_dNDWI_mean, color = "red",
  #           lwd = 1.5, linetype = "dashed")+
  geom_hline(yintercept = 0, color = "red",
             lwd = 1.5, linetype = "dashed")+
  theme_classic()+
  theme_classic()+
  theme(legend.position="none")+
  theme(axis.title = element_text(size=14),
        axis.text = element_text(size=13, color = "grey35"),
        plot.title = element_text(size=18, face = "bold"))+
  #legend.position = c(0.2, 0.89), 
  #legend.title = element_text(size=14), 
  #legend.text = element_text(size=12),)+
  labs(title = "B                   dNDWI",
       x = "Area Class", y = "dNDVI Raster Value")+
  #scale_y_continuous(limits = c(-600,700), n.breaks = 13)+
  annotate(geom="text", x=1, y=-650, label="P-value < 0.0001", size = 4.5)


p2


t_tests <- p1+p2
t_tests

ggsave("t_tests7.jpeg",
  path = "C:/Users/npwil/OneDrive/Desktop/School/Grad School/Coursework/NRM 641 Remote Sensing/Final Project/Figures",
  width = 24, height = 16,  units = c("cm"), 
  dpi = 700,  limitsize = TRUE)




