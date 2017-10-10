
# Packages ----------------------------------------------------------------

library(tidyverse)
library(nycflights13)
library(caret)

# Read data ---------------------------------------------------------------

flights <- nycflights13::flights
weather <- nycflights13::weather


# Data preparation --------------------------------------------------------

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
# No systematic missingness in airports (1 airport etc), therefor I exclude all flights with missing weather.
# However seasonality...

lapply(df, FUN = function(x){summary(x)})

df <- df %>%
    filter(!is.na(temp), # Removes missing weather data
           !is.na(arr_delay)) %>% # Removes missing delay data
    select(-year) # Removes year since no variance

# Select variables with predictive value
cor_matrix <- cor(df[sapply(df, is.numeric)], use = "na.or.complete")
cor_matrix

has_effect <- cor_matrix[cor_matrix[,"dep_delay"] >= abs(0.15) ,"dep_delay"]

# Small data set with cor higher than abs(.15)
df1 <- df %>%
    select(names(has_effect),
           -sched_dep_time)

str(df)
df <- df %>%
    select(-c(sched_dep_time, carrier, flight, tailnum, dest,
              minute, time_hour, wind_gust))


# Create test and training data -------------------------------------------

set.seed(1991)
# Create train and test data
in_train <- createDataPartition(y = df$origin, 
                                p = .8, 
                                list = FALSE)

training <- df[in_train, ]
tmp <- df[-in_train, ]

# create validate and test data
in_validate <- createDataPartition(y = tmp$origin, p = .75, list = FALSE)

validate <- tmp[in_validate, ]
testing <- tmp[-in_validate, ]

rm(tmp)


# Set resampling method ---------------------------------------------------

ctrl <- trainControl(method = "repeatedcv", repeats = 3)


# Train model -------------------------------------------------------------

lmr <- train(arr_delay ~ .,
                  data = training,
                  method = "",
                  trControl = ctrl)



# Model validation --------------------------------------------------------

lmr_class <- predict(lmr, newdata = validate)
lmr_probs <- predict(multinom, newdata = val, type = "prob")

confusionMatrix(data = lmr_class, validate$arr_delay)


# Final model -------------------------------------------------------------

