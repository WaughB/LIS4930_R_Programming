rmse <- function(residuals)
{
  val = sqrt(mean(residuals^2))
  return(val)
}
