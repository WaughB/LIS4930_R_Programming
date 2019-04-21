lazySVM <- function(dataframe, formula) {
  # Required library.
  cat("Checking required libraries")
  require(e1071)

  # Grid search for best Epsilon and Cost values. Made with help from: https://rpubs.com/richkt/280840
  cat("Starting grid search, this may take a while")
  tuneResult1 <- tune(svm, formula,  data = dataframe,
                      ranges = list(epsilon = seq(0,1,0.01), cost = seq(0.01,5,0.05))
  )

  # Continuation of Grid Search.
  tuneResult <- tune(svm, formula,  data = dataframe,
                     ranges = list(epsilon = seq(tuneResult1$best.model$epsilon*1.01,
                                                 tuneResult1$best.model$epsilon*1.1,
                                                 length.out = 10),
                                   cost = seq(tuneResult1$best.model$cost-1,
                                              tuneResult1$best.model$cost+1,
                                              length=10)))

  # SVM model with values from tuneResult.
  svmModTuned = svm(formula, data = dataframe, cost=tuneResult$best.parameters$cost, epsilon=tuneResult$best.parameters$epsilon, cross=10, scale=F, kernal='radial')

  # Return the RMSE of the SVM.
  return(paste('SVM RMSE: ',rmse(svmModTuned$residuals)))
}
