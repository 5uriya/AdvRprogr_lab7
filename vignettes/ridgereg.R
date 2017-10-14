## ---- include = FALSE----------------------------------------------------
library(linreg)
library(tidyverse)
library(mlbench)
library(caret) 
data("BostonHousing")

## ------------------------------------------------------------------------
boston <- BostonHousing[,-4]
colnames(boston[-10])

## ------------------------------------------------------------------------
set.seed(1991)
inTrain <- createDataPartition(y = boston$ptratio,
                                 ## the outcome data are needed
                                p = .8,
                                 ## The percentage of data in the
                                ## training set
                                list = FALSE)
training <- boston[ inTrain,]
testing <- boston[-inTrain,]

## ------------------------------------------------------------------------
# Resampling method set
ctrl <- trainControl(method = "repeatedcv", repeats = 3)
lm_fit <- train(ptratio ~ .,
                 data = training,
                 method = "lm",
                 trControl = ctrl,
                 preProc = "scale")
summary(lm_fit)


## ------------------------------------------------------------------------
# Resampling method set
ctrl <- trainControl(method = "repeatedcv", repeats = 3)
lm_fit <- train(ptratio ~ zn + indus + nox + rad + b + lstat + medv,
                 data = training,
                 method = "lm",
                 trControl = ctrl,
                 preProc = "scale")


## ---- message=FALSE, warning=FALSE---------------------------------------
lmf_fit <- train(ptratio ~ .,
                 data = training,
                 method = 'leapForward',
                 trControl = ctrl,
                 preProc = "scale")

summary(lmf_fit)

## ------------------------------------------------------------------------
lmf_fit <- train(ptratio ~ zn + nox + rad + medv,
                 data = training,
                 method = 'leapForward',
                 trControl = ctrl,
                 preProc = "scale")


## ------------------------------------------------------------------------

resamps <- resamples(list(LinReg = lm_fit, LinRegForw = lmf_fit))
summary(resamps)

## ---- include = FALSE----------------------------------------------------

plot_data <- as.data.frame(cbind(scale(resid(lm_fit)),scale(resid(lmf_fit)),predict(lm_fit),predict(lmf_fit)))

plot1 <- ggplot(plot_data, aes(plot_data[,3],plot_data[,1]))+
        geom_point()+
        geom_smooth(method = "loess")+
        xlab("Fitted Values by lm")+
        ylab("Residuals")+
       ggtitle("Resid vs Fitted LM")


plot2 <- ggplot(plot_data, aes(plot_data[,4],plot_data[,2]))+
    geom_point()+
    geom_smooth(method = "loess")+
    xlab("Fitted Values by lm forward")+
    ylab("Residuals")+
    ggtitle("Resid vs Fitted LM Forward")

## ---- echo= FALSE--------------------------------------------------------
plot1
plot2

## ------------------------------------------------------------------------
Ridgereg_model <- list(
    type = c("Regression"),
    library = "linreg",
    loop = NULL,
    prob = NULL)

Ridgereg_model$parameters <- data.frame(parameter = "lambda",
                                        class = "numeric",
                                        label = "lambda")

Ridgereg_model$grid <- function (x, y, len = NULL, search = "grid"){
    data.frame(lambda = 2)
}

Ridgereg_model$fit <- function (x, y, wts, param, lev, last, classProbs, ...){
    dat <- if (is.data.frame(x))
        x
    else
        as.data.frame(x)
    dat$.outcome <- y
    
    output <-
        ridgereg$new(.outcome ~ ., data = dat, lambda = param$lambda)
    
    output
}


Ridgereg_model$predict <- function(modelFit, newdata, preProc = NULL, submodels = NULL){
    if (!is.data.frame(newdata))
        newdata <- as.data.frame(newdata)
    newdata <- scale(newdata)
    predict(modelFit, newdata)
}

## ------------------------------------------------------------------------
set.seed(12345)
#ridgereg_example <- train(ptratio ~ ., data=training, method=Ridgereg_model, trControl=ctrl)

## ---- echo= FALSE--------------------------------------------------------
sessionInfo()

