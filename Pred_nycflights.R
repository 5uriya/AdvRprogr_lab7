
# Packages ----------------------------------------------------------------

library(tidyverse)
library(nycflights13)
library(caret)

# Read data ---------------------------------------------------------------

flights <- nycflights13::flights
weather <- nycflights13::weather


# Removed variable time stamps from weather
weather <- weather %>%
    select(-year, -month, -day, -hour)

# Join flights and weather data
df <- left_join(flights, weather, by = c("origin", "time_hour")) 

# Flight missing weather information is distributed on all airports and months
weather_NA <- df %>%
    filter(is.na(temp)) %>%
    group_by(origin, month) %>%
    summarise(missing = n()) %>%
    spread(month, missing)

weather_NA
# No systematic missing pattern (1 airport etc), therefor I exclude all flights with missing weather
rm(weather_NA)

df <- df %>%
    filter(!is.na(temp))


# Create test and training data -------------------------------------------

set.seed(1991)
# Create train and test data
in_train <- createDataPartition(y = df$origin, p = .8, list = FALSE)

training <- df[in_train, ]
tmp <- df[-in_train, ]

# create validate and test data
in_validate <- createDataPartition(y = tmp$origin, p = .75, list = FALSE)

validate <- tmp[in_validate, ]
testing <- tmp[-in_validate, ]

