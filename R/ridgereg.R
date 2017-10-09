#' @title Ridge regression
#' @field formula a formula, format y ~ x_1 + x_2 + ... + x_n.
#' @field data a data frame.
#' @field lambda a hyperparameter.
#' @description Returns the result of the Ridge Regression
#' @examples 
#' data(iris)
#' ridgereg$new(Petal.Length~Sepal.Width+Sepal.Length, data=iris, lambda = 0)$print()
#' ridgereg$new(Petal.Length~Sepal.Width+Sepal.Length, data=iris, lambda = 0)$predict()
#' ridgereg$new(Petal.Length~Sepal.Width+Sepal.Length, data=iris, lambda = 0)$coef()
#' @export ridgereg
#' @export

ridgereg <- setRefClass(
    Class = "ridgereg",
    fields = list(
        formula = "formula",
        data = "data.frame",
        lambda = "numeric",
        beta_hat = "matrix",
        beta_zero= "numeric",
        y_hat = "matrix",
        data_name = "character"
    ),
    methods = list(
        initialize = function(formula, data, lambda){
        
            
            formula <<- formula
            data <<- data
            lambda <<- lambda
            normalise <- TRUE
            
            data_name <<- deparse(substitute(data))
            # Define matrix
            X <- model.matrix(formula, data)
            
            if (normalise == TRUE){
            # Normalise the X matrix without intercept
            X_int <- X[,1]
            X_sc <- scale(X[,-1])
            X <- cbind(X_int,X_sc)
            }
            
            # extract the dependant variable from data set
            dep_name <- all.vars(expr = formula)[1]
            y <- (data[, dep_name])
            I <- diag(ncol(X))
            # Calculate coefficients
            beta_hat <<- solve((t(X) %*% X + lambda*I)) %*% t(X) %*% y
            #beta_zero <<- mean(y)
            
            # find the fitted values of y using beta_hat
            y_hat <<- X %*% beta_hat
         },
        # Build ridgereg print function
        print = function() {
           
            # Print function call
            cat(sep = "\n")
            cat("Call:")
            cat(sep = "\n")
            cat(paste("ridgereg(", "formula = ", formula[2], " ", formula[1], " ", formula[3], ", ", "data = ", data_name, ")", sep = "" ))
            
            cat(sep = "\n")
            cat(sep = "\n")
            cat("Coefficients:")
            
            cat(sep = "\n")
            coef_v <-as.vector(beta_hat)
            names(coef_v) <- c("(Intercept)",row.names(beta_hat)[-1])
           
            return(coef_v)
            
        },
        # Build ridgereg pred print function
        predict = function() {
            return((Fitted_values = round(y_hat, 2)))
        },
        # Build ridgereg coef print function
        coef = function() {
            coef_v <-as.vector(beta_hat[-1])
            names(coef_v) <- c(row.names(beta_hat)[-1])
            return(coef_v)
        }
        
    )
)


