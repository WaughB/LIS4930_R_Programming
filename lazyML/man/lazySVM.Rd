\name{lazySVM}
\alias{lazySVM}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{lazySVM}
\description{ Creates a SVM for a dataframe. The dataframe and formula are needed to use this. The function will dynamically create the training and test set. SVM has ten-fold cross validation.
}
\usage{
lazySVM(dataframe, formula)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dataframe}{
The dataframe supplied to the model to perform the calculation on. Should be provided as a dataframe.
}
  \item{formula}{
The formula used by the model to perform the calculation. This has a prediction variable along with other dependant features.
}
}
\details{
Due to the grid search, this function may take a while.
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
Process for grid search was based off of the process found here: https://rpubs.com/richkt/280840 .
}
\author{
Brett W. <waughb@mail.usf.edu>
}
\examples{
load("~/lazyML/data/iris.Rdata")
dataframe <- data.frame(iris)
formula <- Sepal.Length ~ Sepal.Width + Petal.Length +Petal.Width
lazySVM(dataframe, formula)
}
