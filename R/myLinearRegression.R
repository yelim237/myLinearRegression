#' Find Coefficients and P-values from Linear Model with Scatter Plot
#'
#' `myLinearRegression` is used to find coefficients and corresponding p-values from a linear model. Along with the outcomes, the scatter plot between each variable would be provided.
#'
#' @param Y a vector of outcomes.
#' @param X a matrix of covariates.
#' @param sub a list of subjects (i.e., a set of integers corresponding to rows in X).
#' @return
#' @export coeff a set of coefficients from the linear regression of Y on X.
#' @export pvals a set of corresponding p-values.
#' @examples
#' ## Use the data called `myData`.
#' ## Set your working directory and read the csv file.
#' myData = read.csv("myData.csv")
#'
#' ## Dependent variable (column 1) is GPA
#' ## Explanatory variables (column 2-6) are those:
#' ## studying time (X1), numbers of game playing (X2),
#' ## number of friends(X3), commute time (X4),
#' ## and shopping time per week (X5).
#' simu_y = myData[, 1]
#' simu_x = myData[, 2:6]
#' simu_sub = 1:nrow(myData) # Can be any list of subjects.
#'
#' ## Provide coefficients and p-values of each explanatory variable.
#' ## A scatterplot between each variable also created.
#' myLinearRegression(simu_y, simu_x, simu_sub)


myLinearRegression <- function(Y,X,sub){

   data_total <- cbind(Y, X)
   sub_data <- data_total[sub, ]
   LinearModel <- lm(Y ~ ., data = sub_data)

   coef <- summary(LinearModel)$coefficients[, 1]
   pvals <- summary(LinearModel)$coefficients[, 4]

   if(ncol(X) < 6){
    plot = GGally::ggpairs(sub_data)
  }
  else {
    plot = "Too many variables to plot"
  }
   return(list(coef = coef, pvals = pvals, plot = plot))
}

