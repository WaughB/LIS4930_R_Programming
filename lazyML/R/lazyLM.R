lazyLM <- function(dataframe, formula1, formula2, formula3) {
  # Required library.
  cat("Checking for required libraries")
  require(stats)
  
  # First run.
  lin1 <- lm(formula = formula1, data=dataframe)
  residuals1 <- lin1$residuals
  
  # Determine the RMSE of the linear model. 
  lin1RMSE <- rmse(residuals1)
  list1 <- list(model = "First Linear Model", "RMSE" = lin1RMSE, "Description" = lin1$call, "Coefficients" = lin1$coefficients)
  
  # Second run.
  lin2 <- lm(formula = formula2, data=dataframe)
  residuals2 <- lin2$residuals
  
  # Determine the RMSE of the linear model. 
  lin2RMSE <- rmse(residuals2)
  list2 <- list(model = "Second Linear Model", "RMSE" = lin2RMSE, "Description" = lin2$call, "Coefficients" = lin2$coefficients)
  
  # Third run.
  lin3 <- lm(formula = formula3, data=dataframe)
  residuals3 <- lin3$residuals
  
  # Determine the RMSE of the linear model. 
  lin3RMSE <- rmse(residuals3)
  list3 <- list(model = "Third Linear Model", "RMSE" = lin3RMSE, "Description" = lin3$call, "Coefficients" = lin3$coefficients)
  
  # Return a list of results.
  return(list(list1, list2, list3))
}
