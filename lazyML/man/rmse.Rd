\name{rmse}
\alias{rmse}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{rmse}
\description{ Calculates the RMSE value based on the residuals of a model.
}
\usage{
rmse(residuals)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{residuals}{
Residuals from a model.
}
}
\details{
Often will be found by model$residuals.
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\author{
Brett W. <waughb@mail.usf.edu>
}
\examples{
dataframe <- data.frame(iris)
formula <- Sepal.Length ~ Sepal.Width
model <- lm(formula, dataframe)
modelResiduals<- model$residuals
rmse(modelResiduals)
}
