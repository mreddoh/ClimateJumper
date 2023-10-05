
# Description ----


# Load Packages ----
library(tidyverse)


# Grab Data ----
mro.bom <- read_csv(file="data/IDCJAC0010_086071_1800_Data.csv", na = "null") %>% 
  filter(!is.na(`Maximum temperature (Degree C)`)) %>% 
  mutate(date = dmy(paste0(Day,Month,Year)),
         mro.temp = `Maximum temperature (Degree C)`) %>%
  select(date,mro.temp)

mop.bom <- read_csv(file="data/IDCJAC0010_086338_1800_Data.csv", na = "null") %>% 
  filter(!is.na(`Maximum temperature (Degree C)`)) %>% 
  mutate(date = dmy(paste0(Day,Month,Year)),
         mop.temp = `Maximum temperature (Degree C)`) %>%
  select(date,mop.temp)

# get dates that crossover
dates <- mop.bom %>% filter(date %in% (mro.bom %>% select(date) %>% pull())) %>% select(date) %>% pull()


station.adjustment <- inner_join(mro.bom, mop.bom, by=c("date")) %>%
  mutate(diff = mop.temp - mro.temp,
         yearmonth = floor_date(date, "months")) %>%
  mutate(month = month(yearmonth)) %>% 
  group_by(month) %>% 
  summarise(adjustment.value = mean(diff))

mro.bom.adj <- mro.bom %>% 
  mutate(month = month(date)) %>% 
  left_join(., station.adjustment, by=c("month")) %>% 
  mutate(mro.temp = mro.temp + adjustment.value) %>% 
  select(-c(adjustment.value,month))

combined.weather.stations <- mro.bom.adj %>% 
  filter(date < min(dates)) %>% 
  rename(max.temp = mro.temp) %>% 
  rbind(.,(mop.bom %>% rename(max.temp = mop.temp))) %>% 
  filter(year(date) < 2023)


## Plot Monthly Data ----
combined.weather.stations %>% 
  mutate(yearmon = floor_date(date, "months")) %>%
  group_by(yearmon) %>%
  summarise(avg_max_temp = mean(max.temp)) %>%
  ggplot(data = .,
         aes(x = yearmon, y = avg_max_temp)) +
  geom_line(group=1)     
         
## Plot Yearly Data ----
combined.weather.stations %>% 
  mutate(Year = floor_date(date, "years")) %>%
  group_by(Year) %>% 
  summarise(year_ave_max = mean(max.temp)) %>%
  ggplot(data = .,
         aes(x = Year, y = year_ave_max)) +
  geom_line(group=1)     



bands = 20
colours = 7

min.year = 1923
max.year = 2022

graph <- combined.weather.stations %>% 
  mutate(Year = year(date)) %>%
  filter(Year >= min.year & Year <= max.year) %>% 
  mutate(period = cut(Year, breaks = seq(min.year, max.year, (max.year - min.year) / bands), include.lowest = T)) %>% 
  group_by(period) %>% 
  summarise(min.year = min(Year),
            ave_max = mean(max.temp)) %>% 
  ungroup() %>% 
  mutate(colour.bucket = cut(ave_max, breaks = colours, include.lowest = T))

colour_spectrum = c("#17375c","#558ed5","#c4d6ee","#efefef","#f2dddc","#d79593","#943735")


temp.mapping <- tibble(colour.bucket = levels(graph$colour.bucket), colour_spectrum = colour_spectrum)


graph <- graph %>% left_join(.,temp.mapping, by=c("colour.bucket"))




# Create Colour Bands ----

     