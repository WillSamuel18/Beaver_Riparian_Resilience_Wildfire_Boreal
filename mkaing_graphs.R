#library(ggplot2)

data <- read.csv("formatted_data.csv")

# Data processing
data$calendar_year <- with(data, Measurement_year - 2016) #turns year into a more sensible variable

data$Measurement_year <- as.numeric(data$Measurement_year)

df <- data

# Example data
# Assuming your data frame is named 'df'

# Filter data for plotting points where 'Type' is 'Beaver' or 'Control'
points_data <- df[df$Type %in% c('Beaver', 'Control'), ]

points_data$Measurement_year <- as.numeric(points_data$Measurement_year)

# Filter data for plotting lines where 'Type' is 'Fire'
fire_data <- df[df$Type == 'Fire', ]

fireNames <- unique(data$Associated_fire)

for (i in fireNames){
  oneFireData <- points_data[points_data$Associated_fire == i,]
  minYear <- min(oneFireData$yrs_post_fire)
  maxYear <- max(oneFireData$yrs_post_fire)
  for (j in minYear:maxYear){
    oneYearData <- oneFireData[oneFireData$yrs_post_fire == j,]
    
    beaverMeanNDVI <- with(oneYearData, mean(ndvi[Type == 'Beaver']))
    beaverMeanNDWI <- with(oneYearData, mean(ndwi[Type == 'Beaver']))
    controlMeanNDVI <- with(oneYearData, mean(ndvi[Type == 'Control']))
    controlMeanNDWI <- with(oneYearData, mean(ndwi[Type == 'Control']))
    # ID	Associated_fire	Type	Fire_Year	ndvi	ndwi	Measurement_year	yrs_post_fire
    beaverRow <- c('mean', i, 'Beaver', oneYearData[1, "Fire_Year"],  beaverMeanNDVI, beaverMeanNDWI, oneYearData[1, "Measurement_year"], as.numeric(j), oneYearData[1, "calendar_year"])
    controlRow <- c('mean', i, 'Control', oneYearData[1, "Fire_Year"],  controlMeanNDVI, controlMeanNDWI, oneYearData[1, "Measurement_year"], as.numeric(j), oneYearData[1, "calendar_year"])
    
    fire_data <- rbind(fire_data, beaverRow)
    fire_data <- rbind(fire_data, controlRow)
  }
}

fire_data$yrs_post_fire <- as.numeric(fire_data$yrs_post_fire)
fire_data$ndvi <- as.numeric(fire_data$ndvi)
fire_data$Measurement_year < as.numeric(as.character(fire_data$Measurement_year))

# Create the plot
ggplot() +
  # Plot points for 'Beaver' and 'Control'
  geom_point(data = points_data, aes(x = Measurement_year, y = ndvi, color = Type)) +
  # Plot lines for 'Fire'
  geom_line(data = fire_data, aes(x = Measurement_year, y = ndvi, color = Type)) +
  labs(title = "NDVI over Year",
       x = "Year",
       y = "NDVI",
       color = "Type") +  # Add legend title
  scale_color_manual(values = c(Beaver = "green", Control = "blue", Fire = "red")) +  # Specify color values
  facet_wrap(~ Associated_fire, scales = "free_y", ncol = 3)  # Facet by 'Associated_fire'
