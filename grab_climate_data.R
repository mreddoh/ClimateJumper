
# Description ----


# Load Packages ----
library(tidyverse)


# Grab Data ----
bundoora.bom <- read_csv(file="data/IDCJAC0002_086351_Data1.csv", na = "null")
olympic.bom <- read_csv(file="data/IDCJAC0002_086338_Data1.csv", na = "null")
airport.bom <- read_csv(file="data/IDCJAC0002_086282_Data1.csv", na = "null")
viewbank.bom <- read_csv(file="data/IDCJAC0002_086068_Data1.csv", na = "null")


## Plot Monthly Data ----
airport.bom %>% 
  mutate(yearmon = paste0(Year,"-",Month)) %>%
  ggplot(data = .,
         aes(x = yearmon, y = `Mean maximum temperature (°C)`)) +
  geom_line(group=1)     
         
## Plot Yearly Data ----
airport.bom %>% 
  group_by(Year) %>% 
  summarise(year_ave_max = mean(`Mean maximum temperature (°C)`)) %>%
  ggplot(data = .,
         aes(x = Year, y = year_ave_max)) +
  geom_line(group=1)     

airport.yearly <- airport.bom %>% 
  group_by(Year) %>% 
  summarise(year_ave_max = mean(`Mean maximum temperature (°C)`)) 


# Create Colour Bands ----
rr <- range(airport.yearly$year_ave_max)
svals <- (airport.yearly$year_ave_max-rr[1])/diff(rr)

f <- colorRamp(c("blue", "red"))
colors <- rgb(f(svals)/255)

image(seq_along(svals), 1, as.matrix(seq_along(svals)), col=colors,
      axes=FALSE, xlab="", ylab="")
     