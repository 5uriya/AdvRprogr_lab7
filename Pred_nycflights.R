
# Packages ----------------------------------------------------------------

library(tidyverse)
library(nycflights13)
library(caret)

set.seed(1991)

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

# Delete all cases with missing value
df <- df[complete.cases(df), ] %>%
    select(-year)

# Reducing the amount of data used for analysis
df <- df %>%
    sample_frac(.1)

# Select variables with predictive value
cor_matrix <- cor(df[sapply(df, is.numeric)], use = "na.or.complete")
cor_matrix

has_effect <- cor_matrix[cor_matrix[,"dep_delay"] >= abs(0.15) ,"dep_delay"]

# Small data set with cor higher than abs(.15)
df1 <- df %>%
    select(names(has_effect),
           -sched_dep_time)

df <- df %>%
    select(-c(sched_dep_time, carrier, flight, tailnum, dest,
              minute, time_hour, wind_gust))

# Create test and training data -------------------------------------------

set.seed(1991)
# Create train and test data
in_train <- createDataPartition(y = df$arr_delay, 
                                p = .8, 
                                list = FALSE)

training <- df[in_train, ]
tmp <- df[-in_train, ]

# create validate and test data
in_validate <- createDataPartition(y = tmp$arr_delay, p = .75, list = FALSE)

validate <- tmp[in_validate, ]
testing <- tmp[-in_validate, ]

rm(tmp)


# Set resampling method ---------------------------------------------------

ctrl <- trainControl(method = "repeatedcv", repeats = 3)


# Train model -------------------------------------------------------------

# Model 1
# Full data 15 min to run, lambda = 0, Rsquared = 87.124% MAE 10.87018
start <- Sys.time()
lmr_fit <- train(arr_delay ~ .,
                 data = training,
                 method = "ridge",
                 trControl = ctrl,
                 preProc = "scale")
end <- Sys.time()
end-start


# Model validation --------------------------------------------------------

lmr_class <- predict(lmr_fit, newdata = validate)


confusionMatrix(data = lmr_class, validate$arr_delay)


# Final model -------------------------------------------------------------

