################################ Will Samuel ###################################
######### Beaver Riparian Resilience to Wildfire in a boreal biome #############
###################### REVISED analysis, started 10/27/2026 ####################

#This script revises the statistics in "Beaver pond wildfire time series analysis.R"
#after the ERL review. It only includes the analyses used in the manuscript.
#The NDVI/NDWI long files in "Newdata/" were made by the "Data prep" section of
#the original script, so run that first if those files ever need to be rebuilt.
#Notes on what changed are flagged with "#CHANGED:" or "#NEW:" so they are easy to find.
#CHANGED: control sites are now called "Reference" sites (in the data and figures), since they are close to beaver ponds
#and may not be fully independent of beaver activity. Some comments still say "control"; it means the same sites.


library(tidyverse) #for data manipulation
library(lubridate) #for data manipulation
library(ggplot2)  #for plotting
library(patchwork) #for making panels
library(cowplot)  #for plotting
library(viridis)  #for plotting color palettes
library(lme4)     #for mixed effect model
library(lmerTest) #for mixed effect model p-values
library(ordinal) #for ordinal logistic regression with random effect

set.seed(1)

#rm(list=ls(all=TRUE)) # clear the working environment



# Data prep ---------------------------------------------------------------

### Burn severity data __________________________________________________________

Beaver_BurnSeverity_data <- read.csv("Data/Burn Severity Summaries/Beaver_Pond_Polygons_With_burn_Severity_ExportTable.csv")
Control_BurnSeverity_data <- read.csv("Data/Burn Severity Summaries/Control_Polygons_With_burn_Severity_ExportTable.csv")
Fires_BurnSeverity_data <- read.csv("Data/Burn Severity Summaries/Fire_perims_With_burn_Severity_ExportTable.csv")


#NEW: one function to clean the beaver and control tables the same way (they had identical code before)
#Column names from the GIS export are kept, but they are the MTBS classes: Percent_Unburned = Unburned to Low,
#Percent_Mild_Burn = Low, Percent_Moderate_Burn = Moderate, Percent_Severe_Burn = High
clean_burn_severity <- function(x, type) {
  x %>%
    rename(Incid_Name = Beaver_Ponds_2020_Fire_Name,
           Pair = Beaver_Ponds_2020_Dam_Num) %>%  #NEW: control points carry the number of the beaver pond they were paired with
    dplyr::select(Incid_Name, Pair, Percent_Unburned, Percent_Mild_Burn,
                  Percent_Moderate_Burn, Percent_Severe_Burn) %>%
    mutate(Total_Percent = Percent_Unburned + Percent_Mild_Burn + Percent_Moderate_Burn + Percent_Severe_Burn) %>%
    filter(Total_Percent > 0) %>%  #CHANGED: drop sites with no MTBS data (masked for poor imagery) instead of setting them to 0%
    mutate(Percent_Unburned = (Percent_Unburned / Total_Percent)*100,  #Normalize the burn percentages to 100%
           Percent_Mild_Burn = (Percent_Mild_Burn / Total_Percent)*100,
           Percent_Moderate_Burn = (Percent_Moderate_Burn / Total_Percent)*100,
           Percent_Severe_Burn = (Percent_Severe_Burn / Total_Percent)*100,
           Type = type)
}

Beaver_BurnSeverity_data <- clean_burn_severity(Beaver_BurnSeverity_data, "Beaver")    #225 of 235 sites have data
Control_BurnSeverity_data <- clean_burn_severity(Control_BurnSeverity_data, "Reference") #219 of 235 sites have data


Fires_BurnSeverity_data <- Fires_BurnSeverity_data %>%
  dplyr::select(Incid_Name, Percent_Unburned, Percent_Mild_Burn,
                Percent_Moderate_Burn, Percent_Severe_Burn) %>%
  mutate(Total_Percent = Percent_Unburned + Percent_Mild_Burn + Percent_Moderate_Burn + Percent_Severe_Burn,
         Percent_Unburned = (Percent_Unburned / Total_Percent)*100,
         Percent_Mild_Burn = (Percent_Mild_Burn / Total_Percent)*100,
         Percent_Moderate_Burn = (Percent_Moderate_Burn / Total_Percent)*100,
         Percent_Severe_Burn = (Percent_Severe_Burn / Total_Percent)*100,
         Type = "Entire Fire",
         Pair = NA)


BS_data <- rbind(Beaver_BurnSeverity_data, Control_BurnSeverity_data, Fires_BurnSeverity_data) %>%
  mutate(Incid_Name = case_when(
    Incid_Name == "INIAKUK LAKE" ~ "Iniakuk Lake Fire",
    Incid_Name == "HOGATZA RIVER" ~ "Hogatza River Fire",
    Incid_Name == "HOG" ~ "Hog Fire",
    Incid_Name %in% c("SUNSHINE MOUNTAIN", "SUNSHINE MOUNTAINS") ~ "Sunshine Mountain Fire",
    Incid_Name == "MUNSON CREEK" ~ "Munson Creek Fire",
    Incid_Name %in% c("PAIGE MOUNTAN", "PAIGE MOUNTAIN", "PAGE MOUNTAIN") ~ "Page Mountain Fire",
    Incid_Name == "HURST CREEK" ~ "Hurst Creek Fire",
    Incid_Name == "OLD GROUCH TOP" ~ "Old Grouch Top Fire",
    Incid_Name == "VICTORIA MOUNTAIN" ~ "Victoria Mountain Fire",
    Incid_Name == "LITTLE MUD RIVER" ~ "Little Mud River Fire",
    Incid_Name == "CHALKYITSIK COMPLEX" ~ "Chalkyitsik Complex Fire",
    TRUE ~ Incid_Name
  )) %>%
  filter(Incid_Name != "Chalkyitsik Complex Fire") %>%  #Removing this fire since it only has 1 beaver pond
  mutate(Pair_ID = ifelse(is.na(Pair), NA, paste(Incid_Name, Pair, sep = "_")))  #NEW: unique ID for each beaver-control pair

table(BS_data$Type, BS_data$Incid_Name) #number of sites per fire




### NDVI and NDWI data _________________________________________________________

#Read in the data made by the "Data prep" section of the original script
beaver_NDVI_long <- read.csv("Newdata/beaver_NDVI_long.csv")
control_NDVI_long <- read.csv("Newdata/control_NDVI_long.csv")
beaver_NDWI_long <- read.csv("Newdata/beaver_NDWI_long.csv")
control_NDWI_long <- read.csv("Newdata/control_NDWI_long.csv")


#NEW: Build a key that links each Point (e.g., "X3_Paige") to its beaver pond number, so beaver and control sites can be paired.
#Points were named by their row number in each GEE export, so row 3 of beavers6.csv is "X3_Paige".
#In the beaver files the pond number is "Beaver_P_5". In the control files the columns are shifted, and the paired pond number is "Control_Po".
fire_suffix <- c("Iniakuk", "Hogatza", "Hog", "Chalkyitsik", "Sunshine", "Munson",
                 "Paige", "Hurst", "Grouch", "Victoria", "Mud")  #same order as files 0-10

pair_key <- data.frame()
for (i in 0:10) {
  beaver_raw <- read.csv(paste0("Data/beavers", i, ".csv"))
  control_raw <- read.csv(paste0("Data/controls", i, ".csv"))

  pair_key <- rbind(pair_key,
                    data.frame(Point = paste0("B_X", seq_len(nrow(beaver_raw)), "_", fire_suffix[i+1]),
                               Pair = beaver_raw$Beaver_P_5),
                    data.frame(Point = paste0("C_X", seq_len(nrow(control_raw)), "_", fire_suffix[i+1]),
                               Pair = control_raw$Control_Po))
}


#Add Type and modify Point, same as before
beaver_NDVI_long <- beaver_NDVI_long %>% mutate("Type" = "Beaver", Point = paste0("B_", Point))
control_NDVI_long <- control_NDVI_long %>% mutate("Type" = "Reference", Point = paste0("C_", Point))
beaver_NDWI_long <- beaver_NDWI_long %>% mutate("Type" = "Beaver", Point = paste0("B_", Point))
control_NDWI_long <- control_NDWI_long %>% mutate("Type" = "Reference", Point = paste0("C_", Point))


NDVI_long <- rbind(beaver_NDVI_long, control_NDVI_long) %>%
  dplyr::select(Point, Type, Date, DOY, NDVI) %>%
  mutate(Fire = str_extract(Point, "[^_]+$")) %>%
  filter(Fire != "Chalkyitsik") %>%   #only 1 beaver pond, excluded like before
  left_join(pair_key, by = "Point") %>%  #NEW: add the pond number
  mutate(Pair_ID = paste(Fire, Pair, sep = "_"))  #NEW: unique ID for each beaver-control pair

NDWI_long <- rbind(beaver_NDWI_long, control_NDWI_long) %>%
  dplyr::select(Point, Type, Date, DOY, NDWI) %>%
  mutate(Fire = str_extract(Point, "[^_]+$")) %>%
  filter(Fire != "Chalkyitsik") %>%
  left_join(pair_key, by = "Point") %>%
  mutate(Pair_ID = paste(Fire, Pair, sep = "_"))


sum(is.na(NDVI_long$Pair))  #should be 0, every point matched to a pond number
sum(is.na(NDWI_long$Pair))
length(unique(NDVI_long$Point))  #452 sites with NDVI data




# Testing for effects of beavers on burn severity ----------------------------

BS_data_LONG <- BS_data %>%
  filter(Type != "Entire Fire") %>%  #beaver and control sites only for the model
  pivot_longer(
    cols = starts_with("Percent"),
    names_to = "Burn_Type",
    values_to = "Percent"
  ) %>%
  mutate(Burn_Type = recode(Burn_Type,  #CHANGED: official MTBS class names (the GIS export called them Unburned/Mild/Moderate/Severe)
                            Percent_Unburned = "Unburned to Low",
                            Percent_Mild_Burn = "Low",
                            Percent_Moderate_Burn = "Moderate",
                            Percent_Severe_Burn = "High"))

BS_data_LONG$Burn_Type <- factor(BS_data_LONG$Burn_Type, 
                             levels = c("Unburned to Low", "Low", "Moderate", "High"),
                             ordered = TRUE)
BS_data_LONG$Incid_Name <- as.factor(BS_data_LONG$Incid_Name)
BS_data_LONG$Pair_ID <- as.factor(BS_data_LONG$Pair_ID)


#ORIGINAL model, kept for comparison. weights = Percent made each site count as ~100 observations
#(the percents add up to 100), so the standard error was ~10x too small and p was far too low. 
model_original <- clmm(Burn_Type ~ Type + (1 | Incid_Name),
                       data = BS_data_LONG,
                       weights = Percent)
summary(model_original) #Estimate = 0.197, SE = 0.019, p < 0.001


#CHANGED: weights = Percent/100, so each site's weights add up to 1 and each site counts once.
#NEW: random effect for each beaver-control pair, since paired sites are close together (median 160 m) 
model <- clmm(Burn_Type ~ Type + (1 | Incid_Name) + (1 | Pair_ID),
              data = filter(BS_data_LONG, Percent > 0),  #categories with 0% add nothing to the model
              weights = Percent/100)
summary(model)
#Same estimate as before (0.196), but SE = 0.19 and p = 0.30, so no detectable difference between beaver and control sites.
#The pair random effect is estimated at ~0 (the fire effect absorbs the local variation), and the result is identical without it.

exp(coef(model)["TypeReference"]) #odds ratio = 1.22
exp(coef(model)["TypeReference"] + c(-1.96, 1.96) * sqrt(diag(vcov(model)))["TypeReference"]) #95% CI = 0.84 - 1.77
#In plain terms: control sites had 22% higher odds of being in a higher burn severity class than beaver sites, 
#but the confidence interval runs from 16% lower to 77% higher, so we can't tell them apart. 
#NOTE: this is an odds ratio, not a "19% greater probability" like the original manuscript said. 




### NEW: Riparian sites vs the entire fire _____________________________________
#Compare the % moderate + high burn in riparian sites (beaver and control combined) to the whole fire perimeter.
#The fire is the sample unit (n = 10), since there is only one "Entire Fire" value per fire.
#The whole-fire values include the riparian areas, which makes this comparison slightly conservative.

Riparian_vs_fire <- BS_data %>% 
  mutate(Site = ifelse(Type == "Entire Fire", "Entire Fire", "Riparian"),
         Percent_ModSev = Percent_Moderate_Burn + Percent_Severe_Burn,  #% moderate + high severity
         Percent_Burned = 100 - Percent_Unburned) %>%                   #% low, moderate, or high severity (above "Unburned to Low")
  group_by(Incid_Name, Site) %>% 
  summarize(Percent_ModSev = mean(Percent_ModSev), 
            Percent_Burned = mean(Percent_Burned), .groups = "drop") %>% 
  pivot_wider(names_from = Site, values_from = c(Percent_ModSev, Percent_Burned))

Riparian_vs_fire

#Paired t-tests across the 10 fires
t.test(Riparian_vs_fire$Percent_ModSev_Riparian, Riparian_vs_fire$`Percent_ModSev_Entire Fire`, paired = TRUE)
#Moderate + high: 19.5% riparian vs 80.3% entire fire, difference = -60.9 points (95% CI -77.0 to -44.7), t = -8.54, df = 9, p < 0.001
t.test(Riparian_vs_fire$Percent_Burned_Riparian, Riparian_vs_fire$`Percent_Burned_Entire Fire`, paired = TRUE)
#Low or higher: 75.0% riparian vs 93.2% entire fire, difference = -18.2 points (95% CI -30.6 to -5.8), t = -3.32, df = 9, p = 0.009

sum(Riparian_vs_fire$Percent_ModSev_Riparian < Riparian_vs_fire$`Percent_ModSev_Entire Fire`) #10 of 10 fires had less moderate + high burn in riparian sites
sum(Riparian_vs_fire$Percent_Burned_Riparian < Riparian_vs_fire$`Percent_Burned_Entire Fire`)  #7 of 10 fires had less area at low or higher severity (Hog, Sunshine, and Little Mud did not)




### Figure 3: Burn severity of beaver, control, and entire fire _______________

BS_data_LONG_fig <- BS_data %>%   #same as above, but keeping the entire fire data for plotting
  pivot_longer(cols = starts_with("Percent"), names_to = "Burn_Type", values_to = "Percent") %>%
  mutate(Burn_Type = recode(Burn_Type,
                            Percent_Unburned = "Unburned to Low",
                            Percent_Mild_Burn = "Low",
                            Percent_Moderate_Burn = "Moderate",
                            Percent_Severe_Burn = "High"))

BS_data_LONG_fig$Type <- factor(BS_data_LONG_fig$Type, levels = c("Beaver", "Reference", "Entire Fire"))  #plot order
BS_data_LONG_fig$Burn_Type <- factor(BS_data_LONG_fig$Burn_Type, 
                                     levels = c("Unburned to Low", "Low", "Moderate", "High"))

mean_points <- BS_data_LONG_fig %>%
  group_by(Burn_Type, Type) %>%
  summarise(mean_percent = mean(Percent, na.rm = TRUE), .groups = "drop")

Burn_severity_plot <- ggplot(BS_data_LONG_fig, aes(x = Burn_Type, y = Percent, fill = Type)) +
  geom_violin(trim = TRUE, scale = "width", position = position_dodge(0.9), alpha = 0.6) +  #CHANGED: trim = TRUE so violins don't extend past 0-100%
  geom_boxplot(width = 0.2, position = position_dodge(0.9), alpha = 0.6) +
  geom_point(data = mean_points, aes(x = Burn_Type, y = mean_percent, fill = Type), 
             shape = 16, size = 3, color = "black", position = position_dodge(0.9)) +  
  labs(
    x = "Burn Severity Category",
    y = "Percent of Burn Severity Category"
  ) +
  scale_fill_manual(values = c("Reference" = "#FFA366", "Beaver" = "deepskyblue2", "Entire Fire" = "darkred")) +
  scale_y_continuous(limits = c(0, 100), expand = expansion(mult = c(0, 0.05)))+
  theme_minimal() +  
  theme(
    title = element_text(face = "bold", size = 14),
    legend.position = c(0.85, 0.85),  
    legend.title = element_text(size = 14), 
    legend.text = element_text(size = 12),  
    legend.background = element_rect(fill = "white", color = "black"),  
    axis.text.x = element_text(color = "grey20", size = 12),
    axis.text.y = element_text(color = "grey20", size = 12),
    axis.title.x = element_text(color = "black", size = 14),
    axis.title.y = element_text(color = "black", size = 14)
  )
Burn_severity_plot

ggsave(plot = Burn_severity_plot, 
       "Figures/REVISED_Figure_3_burn_severity.jpeg", 
       width = 20, 
       height = 12,
       units = "cm",
       dpi = 300)




### Figure S1: Burn severity for each fire ____________________________________
#CHANGED: the original panels used filter(Incid_Name != "fire"), which plotted every fire EXCEPT the named one,
#so all the panels looked nearly the same. This uses facet_wrap so each panel shows only its own fire.
#NEW: fire year added to each panel title (Reviewer 2 asked for this).

fire_years <- data.frame(Incid_Name = c("Iniakuk Lake Fire", "Hogatza River Fire", "Hog Fire", "Sunshine Mountain Fire",
                                        "Munson Creek Fire", "Page Mountain Fire", "Hurst Creek Fire",
                                        "Old Grouch Top Fire", "Victoria Mountain Fire", "Little Mud River Fire"),
                         Fire_Year = c(2016, 2016, 2016, 2019, 2021, 2019, 2019, 2019, 2019, 2019),
                         Panel = LETTERS[1:10])

BS_data_S1 <- BS_data_LONG_fig %>% 
  bind_rows(BS_data_LONG_fig %>% mutate(Incid_Name = "All Fires")) %>%   #extra "All Fires" panel, like panel K before
  left_join(fire_years, by = "Incid_Name") %>% 
  mutate(Panel_title = ifelse(Incid_Name == "All Fires", "K (All Fires)",
                              paste0(Panel, " (", Incid_Name, ", ", Fire_Year, ")")),
         Type = recode(as.character(Type), "Beaver" = "Beaver Ponds", "Reference" = "Reference Sites"))

BS_data_S1$Type <- factor(BS_data_S1$Type, levels = c("Beaver Ponds", "Reference Sites", "Entire Fire"))  #plot order
BS_data_S1$Panel_title <- factor(BS_data_S1$Panel_title, levels = sort(unique(BS_data_S1$Panel_title)))  #keep A-K order
BS_data_S1$Burn_Type <- factor(BS_data_S1$Burn_Type, 
                               levels = c("High", "Moderate", "Low", "Unburned to Low"))

custom_colors <- c("Unburned to Low" = "darkgreen",
                   "Low" = "yellow",
                   "Moderate" = "#ff7f0e",
                   "High" = "#d62728")

Burn_Severity_Panel <- ggplot(BS_data_S1, aes(x = Type, y = Percent, fill = Burn_Type)) +
  stat_summary(fun = "mean", geom = "bar", position = "stack", width = 0.6) +  #mean % of each class, stacks to 100%
  labs(x = "", y = "Mean Percent of Area", fill = "Burn Severity") +
  scale_fill_manual(values = custom_colors) +
  scale_y_continuous(limits = c(0, 100.01), breaks = seq(0, 100, 20))+
  facet_wrap(~ Panel_title, ncol = 4) +
  theme_cowplot() +
  theme(strip.background = element_blank(),
        strip.text = element_text(face = "bold", hjust = 0, size = 12),
        axis.text.x = element_text(angle = 45, hjust = 1), 
        legend.position = c(0.8, 0.12),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 16, face = "bold"))
Burn_Severity_Panel

ggsave(plot = Burn_Severity_Panel, 
       "Figures/REVISED_Figure_S1_burn_severity_by_fire.jpeg", 
       width = 32, 
       height = 36,  #same shape as the figure frame in the manuscript
       units = "cm",
       dpi = 300)




# Plot the time series ----------------------------------------------------

#NEW: table of fire info for panel titles and ignition dates (MTBS ignition dates from the burn severity export)
fire_info <- data.frame(Fire = c("Iniakuk", "Hogatza", "Hog", "Sunshine", "Munson", 
                                 "Paige", "Hurst", "Grouch", "Victoria", "Mud"),
                        Fire_Name = c("Iniakuk Lake Fire", "Hogatza River Fire", "Hog Fire", "Sunshine Mountain Fire",
                                      "Munson Creek Fire", "Page Mountain Fire", "Hurst Creek Fire",
                                      "Old Grouch Top Fire", "Victoria Mountain Fire", "Little Mud River Fire"),
                        Ig_Date = as.Date(c("2016-06-26", "2016-06-26", "2016-07-06", "2019-06-14", "2021-06-19",
                                            "2019-06-22", "2019-06-22", "2019-06-05", "2019-06-22", "2019-06-21")),
                        Panel = LETTERS[1:10]) %>% 
  mutate(Fire_Year = year(Ig_Date),
         Ig_DOY = yday(Ig_Date),  #ignition day of year
         Title = paste0(Panel, " (", Fire_Name, ", ", Fire_Year, ")"))  #NEW: fire year in the panel title (Reviewer 2)

fire_info  #most fires ignited between DOY 165 and 178. The old early summer window (DOY < 175) overlapped active burning


#NEW: one function makes each fire's panel, instead of 20 copies of the same ggplot code.
#Same look as before. Changes: 
#  - grey shading shows the pre-fire (before ignition) and post-fire (DOY > 225) windows used for dNDVI and dNDWI
#  - dashed red line at the MTBS ignition date
#  - no confidence ribbon on panel I (it was the only panel with one, which Reviewer 2 noticed)
plot_fire <- function(data, fire, index, y_limits, y_label, x_label = "") {
  
  x <- data %>% filter(Fire == fire)
  info <- fire_info %>% filter(Fire == fire)
  names(x)[names(x) == index] <- "Value"  #so the same function works for NDVI and NDWI
  
  ggplot(x, aes(x = DOY, y = Value)) +
    annotate("rect", xmin = -Inf, xmax = info$Ig_DOY, ymin = -Inf, ymax = Inf, fill = "grey85", alpha = 0.5) +  #pre-fire window (before ignition)
    annotate("rect", xmin = 225, xmax = Inf, ymin = -Inf, ymax = Inf, fill = "grey85", alpha = 0.5) +   #post-fire window
    geom_vline(xintercept = info$Ig_DOY, linetype = "dashed", color = "red3", linewidth = 1) +           #ignition date
    geom_point(aes(shape = Type, fill = Type, group = Type), size = 3, alpha = 0.6, col = "black")+
    scale_shape_manual(values = c("Reference" = 24, "Beaver" = 21)) +  # Different shapes for control and treatment
    scale_fill_manual(values = c("Reference" = "#FFA366", "Beaver" = "deepskyblue2"))+
    geom_smooth(aes(col = Type, group = Type), method = "loess", se = FALSE, alpha = 0.8, linewidth = 2) +
    scale_color_manual(values = c("Reference" = "#FFA366", "Beaver" = "deepskyblue2"))+
    labs(title = info$Title, x = x_label, y = y_label) +
    scale_x_continuous(limits = c(150, 245), breaks = seq(150, 225, 25))+  #same x axis on every panel
    scale_y_continuous(limits = y_limits, breaks = seq(y_limits[1], y_limits[2], 0.25))+
    theme_cowplot()+
    theme(legend.position = "none")
}


### Figure 4: NDVI _____________________________________________________________

F0_NDVI_plot <- plot_fire(NDVI_long, "Iniakuk", "NDVI", c(0, 0.75), "NDVI") + 
  theme(legend.position = c(0.75, 0.9), legend.title = element_blank())  #legend on the first panel only, like before
F1_NDVI_plot <- plot_fire(NDVI_long, "Hogatza", "NDVI", c(0, 0.75), "")
F2_NDVI_plot <- plot_fire(NDVI_long, "Hog", "NDVI", c(0, 0.75), "")
F4_NDVI_plot <- plot_fire(NDVI_long, "Sunshine", "NDVI", c(0, 0.75), "NDVI")
F5_NDVI_plot <- plot_fire(NDVI_long, "Munson", "NDVI", c(0, 0.75), "")
F6_NDVI_plot <- plot_fire(NDVI_long, "Paige", "NDVI", c(0, 0.75), "")
F7_NDVI_plot <- plot_fire(NDVI_long, "Hurst", "NDVI", c(0, 0.75), "NDVI")
F8_NDVI_plot <- plot_fire(NDVI_long, "Grouch", "NDVI", c(0, 0.75), "")
F9_NDVI_plot <- plot_fire(NDVI_long, "Victoria", "NDVI", c(0, 0.75), "")
F10_NDVI_plot <- plot_fire(NDVI_long, "Mud", "NDVI", c(0, 0.75), "NDVI", "Day of Year")


#All beaver ponds and all controls (panels K and L), same as before
beaver_NDVI_plot <- ggplot(filter(NDVI_long, Type == "Beaver"), aes(x = DOY, y = NDVI, group = Point, color = Point)) +
  geom_point()+
  geom_smooth(method = "loess", se = FALSE) +
  labs(title = "K (Beaver Ponds)", x = "Day of Year", y = "", color = "Point") +
  scale_color_viridis(discrete = TRUE) +  # Use the Viridis color palette
  scale_x_continuous(limits = c(150, 245), breaks = seq(150, 225, 25))+
  scale_y_continuous(limits = c(0, 0.75), breaks = seq(0, 0.75, 0.25))+
  theme_cowplot()+
  theme(legend.position = "none")

control_NDVI_plot <- ggplot(filter(NDVI_long, Type == "Reference"), aes(x = DOY, y = NDVI, group = Point, color = Point)) +
  geom_point()+
  geom_smooth(method = "loess", se = FALSE) +
  labs(title = "L (Reference)", x = "Day of Year", y = "", color = "Point") +
  scale_color_viridis(discrete = TRUE) +
  scale_x_continuous(limits = c(150, 245), breaks = seq(150, 225, 25))+
  scale_y_continuous(limits = c(0, 0.75), breaks = seq(0, 0.75, 0.25))+
  theme_cowplot()+
  theme(legend.position = "none")


individual_fires_NDVI_2 <- (F0_NDVI_plot + F1_NDVI_plot + F2_NDVI_plot) /
                           (F4_NDVI_plot + F5_NDVI_plot + F6_NDVI_plot) /
                           (F7_NDVI_plot + F8_NDVI_plot + F9_NDVI_plot) /
                           (F10_NDVI_plot + beaver_NDVI_plot + control_NDVI_plot)

ggsave(plot = individual_fires_NDVI_2, 
       "Figures/REVISED_Figure_4_NDVI_by_fire.jpeg", 
       width = 40, 
       height = 35,
       units = "cm",
       dpi = 300)




### Figure 5: NDWI _____________________________________________________________
#Same as Figure 4, repeated for NDWI

F0_NDWI_plot <- plot_fire(NDWI_long, "Iniakuk", "NDWI", c(-0.75, 0.5), "NDWI") + 
  theme(legend.position = c(0.75, 0.9), legend.title = element_blank())
F1_NDWI_plot <- plot_fire(NDWI_long, "Hogatza", "NDWI", c(-0.75, 0.5), "")
F2_NDWI_plot <- plot_fire(NDWI_long, "Hog", "NDWI", c(-0.75, 0.5), "")
F4_NDWI_plot <- plot_fire(NDWI_long, "Sunshine", "NDWI", c(-0.75, 0.5), "NDWI")
F5_NDWI_plot <- plot_fire(NDWI_long, "Munson", "NDWI", c(-0.75, 0.5), "")
F6_NDWI_plot <- plot_fire(NDWI_long, "Paige", "NDWI", c(-0.75, 0.5), "")
F7_NDWI_plot <- plot_fire(NDWI_long, "Hurst", "NDWI", c(-0.75, 0.5), "NDWI")
F8_NDWI_plot <- plot_fire(NDWI_long, "Grouch", "NDWI", c(-0.75, 0.5), "")
F9_NDWI_plot <- plot_fire(NDWI_long, "Victoria", "NDWI", c(-0.75, 0.5), "")
F10_NDWI_plot <- plot_fire(NDWI_long, "Mud", "NDWI", c(-0.75, 0.5), "NDWI", "Day of Year")

beaver_NDWI_plot <- ggplot(filter(NDWI_long, Type == "Beaver"), aes(x = DOY, y = NDWI, group = Point, color = Point)) +
  geom_point()+
  geom_smooth(method = "loess", se = FALSE) +
  labs(title = "K (Beaver Ponds)", x = "Day of Year", y = "", color = "Point") +
  scale_color_viridis(option="G", discrete = TRUE) +
  scale_x_continuous(limits = c(150, 245), breaks = seq(150, 225, 25))+
  scale_y_continuous(limits = c(-0.75, 0.5), breaks = seq(-0.75, 0.5, 0.25))+
  theme_cowplot()+
  theme(legend.position = "none")

control_NDWI_plot <- ggplot(filter(NDWI_long, Type == "Reference"), aes(x = DOY, y = NDWI, group = Point, color = Point)) +
  geom_point()+
  geom_smooth(method = "loess", se = FALSE) +
  labs(title = "L (Reference)", x = "Day of Year", y = "", color = "Point") +
  scale_color_viridis(option="G", discrete = TRUE) +
  scale_x_continuous(limits = c(150, 245), breaks = seq(150, 225, 25))+
  scale_y_continuous(limits = c(-0.75, 0.5), breaks = seq(-0.75, 0.5, 0.25))+
  theme_cowplot()+
  theme(legend.position = "none")


individual_fires_NDWI_2 <- (F0_NDWI_plot + F1_NDWI_plot + F2_NDWI_plot) /
                           (F4_NDWI_plot + F5_NDWI_plot + F6_NDWI_plot) /
                           (F7_NDWI_plot + F8_NDWI_plot + F9_NDWI_plot) /
                           (F10_NDWI_plot + beaver_NDWI_plot + control_NDWI_plot)

ggsave(plot = individual_fires_NDWI_2, 
       "Figures/REVISED_Figure_5_NDWI_by_fire.jpeg", 
       width = 40, 
       height = 35,
       units = "cm",
       dpi = 300)




# Generalized linear mixed effect models for NDVI and NDWI -------------------
#Testing how beaver vs control sites affect dNDVI and dNDWI

#CHANGED: pre-fire is now each fire's own window, from June 1 to the day before its MTBS ignition date.
#Before, pre-fire was June 1 to DOY 175 for every fire, which overlapped active burning in most fires 
#(e.g., Old Grouch Top ignited on DOY 156). Post-fire is still DOY > 225 (Aug 13 - Aug 31).
#NEW: this function does the pre/post summary for NDVI or NDWI, so the code isn't repeated
pre_post_change <- function(data, index) {
  
  x <- data %>% left_join(dplyr::select(fire_info, Fire, Ig_DOY), by = "Fire")
  names(x)[names(x) == index] <- "Value"
  
  x %>% 
    filter(!is.na(Value)) %>% 
    mutate(Season = case_when(DOY < Ig_DOY ~ "Pre",     #before ignition
                              DOY > 225 ~ "Post")) %>%  #end of summer
    filter(!is.na(Season)) %>% 
    group_by(Point, Type, Fire, Pair_ID, Season) %>% 
    summarize(Value = mean(Value), .groups = "drop") %>%   #mean of all images in each window
    pivot_wider(names_from = Season, values_from = Value) %>% 
    filter(!is.na(Pre), !is.na(Post)) %>%                  #sites need images in both windows
    mutate(Change = Post - Pre)
}


#### DO NDVI FIRST _____________________________________________________________

NDVI_combined <- pre_post_change(NDVI_long, "NDVI") %>% 
  rename(NDVI_pre = Pre, NDVI_post = Post, dNDVI = Change)

dim(NDVI_combined) #450 sites with images in both windows

#Means for the text
NDVI_combined %>% group_by(Type) %>% 
  summarize(NDVI_pre = mean(NDVI_pre), NDVI_post = mean(NDVI_post), dNDVI = mean(dNDVI),
            Percent_change = mean(dNDVI)/mean(NDVI_pre)*100)  #% decline relative to the pre-fire value
#Beaver: pre = 0.443, post = 0.342, dNDVI = -0.101 (-22.9%)
#Control: pre = 0.477, post = 0.338, dNDVI = -0.139 (-29.1%)


#CHANGED: paired t-tests, comparing each beaver site to its own control (only pairs where both sites have data)
NDVI_pairs <- NDVI_combined %>% 
  dplyr::select(Pair_ID, Fire, Type, NDVI_pre, NDVI_post, dNDVI) %>% 
  pivot_wider(names_from = Type, values_from = c(NDVI_pre, NDVI_post, dNDVI)) %>% 
  drop_na()

nrow(NDVI_pairs) #219 complete pairs

#Is the pre-fire NDVI different between sites? 
t.test(NDVI_pairs$NDVI_pre_Reference, NDVI_pairs$NDVI_pre_Beaver, paired = TRUE) 
#Before the fire, control sites were greener than their paired beaver sites: difference = 0.035, t = 7.65, df = 218, p < 0.001
#Is the post-fire NDVI different between sites? 
t.test(NDVI_pairs$NDVI_post_Reference, NDVI_pairs$NDVI_post_Beaver, paired = TRUE) 
#After the fire there was no difference: difference = 0.001, t = 0.09, df = 218, p = 0.925


#ORIGINAL model structure, kept for comparison (fire random effect only)
model_NDVI_original <- lmer(dNDVI ~ Type + (1|Fire), data = NDVI_combined)
summary(model_NDVI_original)
#Control estimate = -0.035, t = -2.68, p = 0.008 (with the new pre-fire window)

#NEW: paired model, with a random effect for each beaver-control pair 
model_NDVI <- lmer(dNDVI ~ Type + (1|Fire) + (1|Pair_ID), data = NDVI_combined)
summary(model_NDVI)
#Control sites lose more NDVI than their paired beaver sites: estimate = -0.034, t = -4.53, p < 0.001

#NEW: same model, accounting for the pre-fire NDVI. Sites that start greener have more to lose, 
#and beaver sites start lower (likely because of open water in the pond buffers).
model_NDVI_adj <- lmer(dNDVI ~ Type + NDVI_pre + (1|Fire) + (1|Pair_ID), data = NDVI_combined)
summary(model_NDVI_adj)
#Control estimate = -0.0004, t = -0.05, p = 0.960. NDVI_pre estimate = -1.00, p < 0.001
#Once the starting NDVI is accounted for, there is no difference between beaver and control sites. 
#So the smaller NDVI decline at beaver sites comes from their lower starting NDVI, not a buffering effect.




### NOW DO NDWI ________________________________________________________________
#Same steps as NDVI

NDWI_combined <- pre_post_change(NDWI_long, "NDWI") %>% 
  rename(NDWI_pre = Pre, NDWI_post = Post, dNDWI = Change)

NDWI_combined %>% group_by(Type) %>% 
  summarize(NDWI_pre = mean(NDWI_pre), NDWI_post = mean(NDWI_post), dNDWI = mean(dNDWI))
#Beaver: pre = -0.329, post = -0.241, dNDWI = 0.089
#Control: pre = -0.351, post = -0.239, dNDWI = 0.112

NDWI_pairs <- NDWI_combined %>% 
  dplyr::select(Pair_ID, Fire, Type, NDWI_pre, NDWI_post, dNDWI) %>% 
  pivot_wider(names_from = Type, values_from = c(NDWI_pre, NDWI_post, dNDWI)) %>% 
  drop_na()

t.test(NDWI_pairs$NDWI_pre_Reference, NDWI_pairs$NDWI_pre_Beaver, paired = TRUE) 
#Before the fire, beaver sites were wetter: difference = -0.022, t = -5.75, df = 218, p < 0.001
t.test(NDWI_pairs$NDWI_post_Reference, NDWI_pairs$NDWI_post_Beaver, paired = TRUE) 
#After the fire there was no difference: difference = -0.001, t = -0.08, df = 218, p = 0.935

model_NDWI_original <- lmer(dNDWI ~ Type + (1|Fire), data = NDWI_combined)
summary(model_NDWI_original)
#Control estimate = 0.021, t = 1.88, p = 0.061

model_NDWI <- lmer(dNDWI ~ Type + (1|Fire) + (1|Pair_ID), data = NDWI_combined)
summary(model_NDWI)
#Paired: control sites gain more NDWI, estimate = 0.022, t = 3.19, p = 0.002

model_NDWI_adj <- lmer(dNDWI ~ Type + NDWI_pre + (1|Fire) + (1|Pair_ID), data = NDWI_combined)
summary(model_NDWI_adj)
#Adjusted for pre-fire NDWI: estimate = 0.002, t = 0.25, p = 0.803. Same pattern as NDVI, the difference comes from the starting values.




### Figure 6: Pre vs post-fire NDVI and NDWI ___________________________________
#Same layout as before. CHANGED: asterisks are now placed from the p-values above instead of by hand
#(paired t-tests for panels A and C, the adjusted models for panels B and D).

p_star <- function(p) ifelse(p < 0.05, "*", "")  #asterisk if significant

NDVI_stars <- c(Pre = p_star(t.test(NDVI_pairs$NDVI_pre_Reference, NDVI_pairs$NDVI_pre_Beaver, paired = TRUE)$p.value),
                Post = p_star(t.test(NDVI_pairs$NDVI_post_Reference, NDVI_pairs$NDVI_post_Beaver, paired = TRUE)$p.value),
                Change = p_star(coef(summary(model_NDVI_adj))["TypeReference", "Pr(>|t|)"]))
NDWI_stars <- c(Pre = p_star(t.test(NDWI_pairs$NDWI_pre_Reference, NDWI_pairs$NDWI_pre_Beaver, paired = TRUE)$p.value),
                Post = p_star(t.test(NDWI_pairs$NDWI_post_Reference, NDWI_pairs$NDWI_post_Beaver, paired = TRUE)$p.value),
                Change = p_star(coef(summary(model_NDWI_adj))["TypeReference", "Pr(>|t|)"]))


NDVI_combined_2 <- bind_rows(
  NDVI_combined %>% dplyr::select(Type, NDVI = NDVI_pre) %>% mutate(Season = "Pre-Fire"),
  NDVI_combined %>% dplyr::select(Type, NDVI = NDVI_post) %>% mutate(Season = "Post-Fire")
) %>%
  mutate(Season = factor(Season, levels = c("Pre-Fire", "Post-Fire")))

Spring_vs_fall_NDVI <- ggplot(NDVI_combined_2, aes(x = Season, y = NDVI, fill = Type)) +
  geom_violin(position = position_dodge(width = 0.75), alpha = 0.7) +
  geom_boxplot(position = position_dodge(width = 0.75), width = 0.1) +
  stat_summary(fun = mean, geom = "point", shape = 16, size = 3, 
               color = "black", position = position_dodge(width = 0.75))+
  theme_minimal() +
  labs(title = "A",
       x = "", y = "Normalized Difference Vegetation Index") +
  scale_fill_manual(values = c("Beaver" = "deepskyblue2", "Reference" = "#FFA366"))+
  scale_y_continuous(limits = c(0, 0.8), breaks = c(0, 0.2, 0.4, 0.6, 0.8))+
  annotate("text", x = c("Pre-Fire", "Post-Fire"), y = 0, label = NDVI_stars[c("Pre", "Post")], size = 14, color = "black") +
  theme(
    title = element_text(face = "bold", size = 14),
    legend.position = "none",
    axis.text.x = element_text(color = "black", size = 14),
    axis.text.y = element_text(color = "grey20", size = 12),
    axis.title.y = element_text(size = 14))
Spring_vs_fall_NDVI  


beaver_violin_NDVI <- ggplot(NDVI_combined, aes(x = Type, y = dNDVI, fill = Type)) +
  geom_violin(trim = FALSE, alpha = 0.6) +  
  geom_boxplot(width = 0.1, position = position_dodge(width = 0.9), color = "black", alpha = 0.7) +  # Boxplot on top
  stat_summary(fun = mean, geom = "point", shape = 16, size = 3, 
               color = "black", position = position_dodge(width = 0.75))+ 
  scale_fill_manual(values = c("Reference" = "#FFA366", "Beaver" = "deepskyblue2")) +
  labs(title = "B",
    x = "", y = "NDVI Change (dNDVI)") +
  theme_minimal() +  
  annotate("text", x = 1.4, y = -0.6, label = NDVI_stars["Change"], size = 14, color = "black", hjust = 0.5)+
  theme(
    title = element_text(face = "bold", size = 14),
    legend.position = "none",
    axis.text.x = element_text(color = "black", size = 14),
    axis.text.y = element_text(color = "grey20", size = 14),
    axis.title.y = element_text(size = 14))+
  coord_flip()  # Flip the axes
beaver_violin_NDVI


NDWI_combined_2 <- bind_rows(
  NDWI_combined %>% dplyr::select(Type, NDWI = NDWI_pre) %>% mutate(Season = "Pre-Fire"),
  NDWI_combined %>% dplyr::select(Type, NDWI = NDWI_post) %>% mutate(Season = "Post-Fire")
) %>%
  mutate(Season = factor(Season, levels = c("Pre-Fire", "Post-Fire")))

Spring_vs_fall_NDWI <- ggplot(NDWI_combined_2, aes(x = Season, y = NDWI, fill = Type)) +
  geom_violin(position = position_dodge(width = 0.75), alpha = 0.7) +
  geom_boxplot(position = position_dodge(width = 0.75), width = 0.1) +
  stat_summary(fun = mean, geom = "point", shape = 16, size = 3, 
               color = "black", position = position_dodge(width = 0.75))+
  theme_minimal() +
  labs(title = "C",
       x = "", y = "Normalized Difference Water Index") +
  scale_fill_manual(values = c("Beaver" = "deepskyblue2", "Reference" = "#FFA366"))+
  scale_y_continuous(limits = c(-0.7, 0.2), breaks = c(-0.6, -0.4, -0.2, 0, 0.2))+
  annotate("text", x = c("Pre-Fire", "Post-Fire"), y = -0.7, label = NDWI_stars[c("Pre", "Post")], size = 14, color = "black") +
  theme(
    title = element_text(face = "bold", size = 14),
    legend.position = c(0.27, 0.9),  
    legend.title = element_text(size = 14), 
    legend.text = element_text(size = 12),  
    legend.background = element_rect(fill = "white", color = "black"),  
    axis.text.x = element_text(color = "black", size = 14),
    axis.text.y = element_text(color = "grey20", size = 12),
    axis.title.y = element_text(size = 14))
Spring_vs_fall_NDWI  


beaver_violin_NDWI <- ggplot(NDWI_combined, aes(x = Type, y = dNDWI, fill = Type)) +
  geom_violin(trim = FALSE, alpha = 0.6) +  
  geom_boxplot(width = 0.1, position = position_dodge(width = 0.9), color = "black", alpha = 0.7) +  # Boxplot on top
  stat_summary(fun = mean, geom = "point", shape = 16, size = 3, 
               color = "black", position = position_dodge(width = 0.75)) +
  scale_fill_manual(values = c("Reference" = "#FFA366", "Beaver" = "deepskyblue2")) +
  labs(title = "D",
       x = "", y = "NDWI Change (dNDWI)") +
  theme_minimal() +  
  annotate("text", x = 1.4, y = -0.3, label = NDWI_stars["Change"], size = 14, color = "black", hjust = 0.5)+
  theme(
    title = element_text(face = "bold", size = 14),
    legend.position = "none",
    axis.text.x = element_text(color = "black", size = 14),
    axis.text.y = element_text(color = "grey20", size = 14),
    axis.title.y = element_text(size = 14))+
  coord_flip()  # Flip the axes
beaver_violin_NDWI


#Now combine the panels, same layout as before
NDVI_change_panel <- Spring_vs_fall_NDVI / beaver_violin_NDVI + 
  plot_layout(heights = c(3, 1))
NDWI_change_panel <- Spring_vs_fall_NDWI / beaver_violin_NDWI + 
  plot_layout(heights = c(3, 1))

overall_change_panel <- NDVI_change_panel | NDWI_change_panel
overall_change_panel

ggsave(plot = overall_change_panel, 
       "Figures/REVISED_Figure_6_pre_post_change.jpeg", 
       width = 25, 
       height = 25,
       units = "cm",
       dpi = 300)




# Summary of numbers for the manuscript ---------------------------------------
#NEW: prints every number cited in the manuscript in one place, in the order they appear.
#Re-run this after any change above so the text and the code always match.

fmt_p <- function(p) ifelse(p < 0.001, "p < 0.001", paste0("p = ", formatC(p, format = "f", digits = 3)))  #p-value formatting
pull_lmer <- function(m, term = "TypeReference") {  #estimate, t, and p for one term of a lmer model
  x <- coef(summary(m))[term, ]
  sprintf("estimate = %.3f, t = %.2f, %s", x["Estimate"], x["t value"], fmt_p(x["Pr(>|t|)"]))
}
pull_t <- function(a, b) {  #paired t-test result as text
  x <- t.test(a, b, paired = TRUE)
  sprintf("difference = %.3f, t = %.2f, df = %d, %s", x$estimate, x$statistic, as.integer(x$parameter), fmt_p(x$p.value))
}


### Methods: sample sizes ______________________________________________________

#Distance between each beaver site and its paired control, from the polygon centers in the GEE files
center <- function(geo) {  #mean longitude and latitude of a GeoJSON polygon string
  xy <- as.numeric(str_extract_all(geo, "-?\\d+\\.\\d+")[[1]])
  c(lon = mean(xy[c(TRUE, FALSE)]), lat = mean(xy[c(FALSE, TRUE)]))
}
pair_distance <- data.frame()
for (i in 0:10) {
  beaver_raw <- read.csv(paste0("Data/beavers", i, ".csv"))
  control_raw <- read.csv(paste0("Data/controls", i, ".csv"))
  b_xy <- t(sapply(beaver_raw$.geo, center)); c_xy <- t(sapply(control_raw$.geo, center))
  m <- match(control_raw$Control_Po, beaver_raw$Beaver_P_5)  #find each control's paired beaver pond
  ok <- !is.na(m)
  pair_distance <- rbind(pair_distance, data.frame(
    Fire = fire_suffix[i+1],
    Distance_m = sqrt(((c_xy[ok, "lon"] - b_xy[m[ok], "lon"]) * 111320 * cos(c_xy[ok, "lat"] * pi/180))^2 +  #degrees to meters
                      ((c_xy[ok, "lat"] - b_xy[m[ok], "lat"]) * 110540)^2)))
}
pair_distance <- filter(pair_distance, Fire != "Chalkyitsik")

cat("\n==== METHODS: SAMPLE SIZES ====\n")
cat(sprintf("Wildfires: %d (%s)\n", nrow(fire_info), paste(range(fire_info$Fire_Year), collapse = "-")))
cat(sprintf("Burn severity: %d beaver sites and %d control sites with MTBS data\n", 
            sum(BS_data$Type == "Beaver"), sum(BS_data$Type == "Reference")))
cat(sprintf("NDVI/NDWI: %d sites with images before ignition and after DOY 225, %d complete beaver-control pairs\n", 
            nrow(NDVI_combined), nrow(NDVI_pairs)))
cat(sprintf("Distance between paired sites: median = %.0f m, 90th percentile = %.0f m, %d of %d pairs > 1 km\n",
            median(pair_distance$Distance_m), quantile(pair_distance$Distance_m, 0.9),
            sum(pair_distance$Distance_m > 1000), nrow(pair_distance)))

#Sites and image dates per fire (Reviewer 1 asked for the image dates). Also saved as a csv for a supplementary table.
image_dates <- NDVI_long %>% 
  filter(!is.na(NDVI)) %>% 
  left_join(dplyr::select(fire_info, Fire, Fire_Name, Fire_Year, Ig_Date, Ig_DOY), by = "Fire") %>% 
  group_by(Fire_Name, Fire_Year, Ig_Date) %>% 
  summarize(Beaver_sites = n_distinct(Point[Type == "Beaver"]),
            Reference_sites = n_distinct(Point[Type == "Reference"]),
            Image_dates_pre = n_distinct(Date[DOY < Ig_DOY]),
            Image_dates_post = n_distinct(Date[DOY > 225]),
            Image_dates_all = paste(sort(unique(Date)), collapse = "; "), .groups = "drop")
print(dplyr::select(image_dates, -Image_dates_all))
write.csv(image_dates, "Figures/REVISED_Table_image_dates_by_fire.csv", row.names = FALSE)


### Results: burn severity ____________________________________________________

cat("\n==== RESULTS: BURN SEVERITY ====\n")
BS_means <- BS_data %>% 
  group_by(Type) %>% 
  summarize(across(starts_with("Percent"), mean))
print(BS_means)  #mean % of each MTBS class for beaver, control, and the entire fire

x <- coef(summary(model))["TypeReference", ]
ci <- exp(x["Estimate"] + c(-1.96, 1.96) * x["Std. Error"])
cat(sprintf("CLMM beaver vs reference: estimate = %.3f (SE = %.3f), odds ratio = %.2f (95%% CI %.2f-%.2f), z = %.2f, %s\n",
            x["Estimate"], x["Std. Error"], exp(x["Estimate"]), ci[1], ci[2], x["z value"], fmt_p(x["Pr(>|z|)"])))

cat(sprintf("Moderate + high, riparian vs entire fire: %.1f%% vs %.1f%%, %s; riparian lower in %d of %d fires\n",
            mean(Riparian_vs_fire$Percent_ModSev_Riparian), mean(Riparian_vs_fire$`Percent_ModSev_Entire Fire`),
            pull_t(Riparian_vs_fire$Percent_ModSev_Riparian, Riparian_vs_fire$`Percent_ModSev_Entire Fire`),
            sum(Riparian_vs_fire$Percent_ModSev_Riparian < Riparian_vs_fire$`Percent_ModSev_Entire Fire`), nrow(Riparian_vs_fire)))
cat(sprintf("Low or higher, riparian vs entire fire: %.1f%% vs %.1f%%, %s; riparian lower in %d of %d fires\n",
            mean(Riparian_vs_fire$Percent_Burned_Riparian), mean(Riparian_vs_fire$`Percent_Burned_Entire Fire`),
            pull_t(Riparian_vs_fire$Percent_Burned_Riparian, Riparian_vs_fire$`Percent_Burned_Entire Fire`),
            sum(Riparian_vs_fire$Percent_Burned_Riparian < Riparian_vs_fire$`Percent_Burned_Entire Fire`), nrow(Riparian_vs_fire)))
cat(sprintf("Range across fires, moderate + high: riparian %.1f-%.1f%%, entire fire %.1f-%.1f%%\n",
            min(Riparian_vs_fire$Percent_ModSev_Riparian), max(Riparian_vs_fire$Percent_ModSev_Riparian),
            min(Riparian_vs_fire$`Percent_ModSev_Entire Fire`), max(Riparian_vs_fire$`Percent_ModSev_Entire Fire`)))


### Results: NDVI and NDWI ____________________________________________________

cat("\n==== RESULTS: NDVI ====\n")
NDVI_combined %>% group_by(Type) %>% 
  summarize(NDVI_pre = mean(NDVI_pre), NDVI_post = mean(NDVI_post), dNDVI = mean(dNDVI),
            Percent_change = mean(dNDVI)/mean(NDVI_pre)*100) %>% print()
cat("Pre-fire, reference minus beaver (paired t-test):", pull_t(NDVI_pairs$NDVI_pre_Reference, NDVI_pairs$NDVI_pre_Beaver), "\n")
cat("Post-fire, reference minus beaver (paired t-test):", pull_t(NDVI_pairs$NDVI_post_Reference, NDVI_pairs$NDVI_post_Beaver), "\n")
cat("dNDVI, paired model:", pull_lmer(model_NDVI), "\n")
cat("dNDVI, adjusted for pre-fire NDVI:", pull_lmer(model_NDVI_adj), "\n")
cat("   pre-fire NDVI term:", pull_lmer(model_NDVI_adj, "NDVI_pre"), "\n")

cat("\n==== RESULTS: NDWI ====\n")
NDWI_combined %>% group_by(Type) %>% 
  summarize(NDWI_pre = mean(NDWI_pre), NDWI_post = mean(NDWI_post), dNDWI = mean(dNDWI)) %>% print()
cat("Pre-fire, reference minus beaver (paired t-test):", pull_t(NDWI_pairs$NDWI_pre_Reference, NDWI_pairs$NDWI_pre_Beaver), "\n")
cat("Post-fire, reference minus beaver (paired t-test):", pull_t(NDWI_pairs$NDWI_post_Reference, NDWI_pairs$NDWI_post_Beaver), "\n")
cat("dNDWI, paired model:", pull_lmer(model_NDWI), "\n")
cat("dNDWI, adjusted for pre-fire NDWI:", pull_lmer(model_NDWI_adj), "\n")
cat("   pre-fire NDWI term:", pull_lmer(model_NDWI_adj, "NDWI_pre"), "\n")

