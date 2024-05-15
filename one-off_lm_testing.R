library(AICcmodavg)

data <- read.csv("formatted_data.csv")

# Data processing
data$calendar_year <- with(data, Measurement_year - 2016) #turns year into a more sensible variable

# Models
data_subset <- data[data$Type != 'Fire',]

model1 = lm(ndvi ~ Associated_fire + calendar_year, data = data_subset)

summary(model1)

AICc(model1)
