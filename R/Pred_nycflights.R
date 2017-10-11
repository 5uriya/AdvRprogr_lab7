
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
    dplyr::select(-year, -month, -day, -hour)

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
    dplyr::select(-year)


# Reducing the amount of data used for analysis
df <- df %>%
    sample_frac(.1)


# Select variables with predictive value
cor_matrix <- cor(df[sapply(df, is.numeric)])

has_effect <- cor_matrix[cor_matrix[,"arr_delay"] >= abs(0.15) ,"arr_delay"]

formula_short <- arr_delay ~ dep_delay + dep_time + hour


# Remove variables without effect
df <- df %>%
    dplyr::select(-c(sched_dep_time, carrier, flight, tailnum, dest,
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

ctrl <- trainControl(method = "repeatedcv", 
                     repeats = 3)


# Modeling ----------------------------------------------------------------

# Model 1
start <- Sys.time()
lmr_fit1 <- train(arr_delay ~ .,
                 data = training,
                 method = "ridge",
                 trControl = ctrl,
                 preProc = "scale")
end <- Sys.time()
end-start
# Full data 15 min to run, lambda = 0, Rsquared = 87.124% MAE 10.87018

# Predict
lmr_class1 <- predict(lmr_fit1, newdata = validate)

# Evaluate
lmr_fit1

df_lmr_fit1 <- validate %>%
    dplyr::select(arr_delay) %>%
    cbind(., fitted_values = predict(lmr_fit1, newdata = validate)) %>%
    mutate(resid = arr_delay - fitted_values)
    

gg1_lmr_fit1 <- ggplot(data = df_lmr_fit1, aes(x = arr_delay, y = fitted_values)) +
    geom_point(aes(alpha = .01)) +
    geom_abline(slope = 1, intercept = 0, colour = "red", size = 0.5)

gg2_lmr_fit1 <- ggplot(data = df_lmr_fit1, aes(x = fitted_values, y = resid)) +
    geom_point(aes(alpha = .01)) +
    geom_smooth(method = "loess", se = FALSE, colour = "red", size = .5)

postResample(lmr_class1, validate$arr_delay)



# Model 2
lmr_fit2 <- train(formula_short,
                  data = training,
                  method = "ridge",
                  trControl = ctrl,
                  preProc = "scale")

# Predict
lmr_class2 <- predict(lmr_fit2, newdata = validate)

# Evaluted
lmr_fit2

df_lmr_fit2 <- validate %>%
    dplyr::select(arr_delay) %>%
    cbind(., fitted_values = predict(lmr_fit2, newdata = validate)) %>%
    mutate(resid = arr_delay - fitted_values)


gg1_lmr_fit2 <- ggplot(data = df_lmr_fit2, aes(x = arr_delay, y = fitted_values)) +
    geom_point(aes(alpha = .01)) +
    geom_abline(slope = 1, intercept = 0, colour = "red", size = 0.5)

gg2_lmr_fit2 <- ggplot(data = df_lmr_fit2, aes(x = fitted_values, y = resid)) +
    geom_point(aes(alpha = .01)) +
    geom_smooth(method = "loess", se = FALSE, colour = "red", size = .5)

postResample(lmr_class2, validate$arr_delay)

# Best model

resamp <- resamples(list(rreg_all_vars = lmr_fit1,
                         rreg_high_cor = lmr_fit2))

summary(resamp)

# Final model -------------------------------------------------------------

