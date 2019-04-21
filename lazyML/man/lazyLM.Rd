\name{lazyLM}
\alias{lazyLM}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{lazyLM}
\description{ Creates three linear models and performs different formulas on each one. This saves the time of testing different formulas on the same dataframe.
}
\usage{
lazyLM(dataframe, formula1, formula2, formula3)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dataframe}{
The dataframe supplied to the model to perform the calculation on. Should be provided as a dataframe.
}
  \item{formula1}{
The formula used by the model to perform the calculation. This has a prediction variable along with other dependant features.
}
  \item{formula2}{
The formula used by the model to perform the calculation. This has a prediction variable along with other dependant features.
}
  \item{formula3}{
The formula used by the model to perform the calculation. This has a prediction variable along with other dependant features.
}
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
formula1 <- Sepal.Length ~ Sepal.Width
formula2 <- Sepal.Length ~ Sepal.Width + Petal.Length 
formula3 <- Sepal.Length ~ Sepal.Width + Petal.Length + Petal.Width
lazyLM(dataframe, formula1, formula2, formula3)
}
