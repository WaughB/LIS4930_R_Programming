# Brett Waugh
# 17 February 2019
# R Object: S3 vs. S4
# mod7.R

library(pryr)

# S3 Creation
demoData <- list(firstName="John", lastName="Doe", age="30", state="Florida", married=TRUE)

class(demoData) <- "People"

attributes(demoData)

demoData$firstName

# S4 Class
setClass("demoData2",    
          representation
          (   
            firstName= "character",
            lastName= "character",
            age= "numeric",   
            state= "character",
            married= "logical"
          )
        )

jd <- new("demoData2", firstName="John", lastName="Doe" , age=30, state="Florida", married=TRUE)

jd@firstName

# Generic function UseMethod()
demoData3 <- function(firstName="John", lastName="Doe", age="30", state="Florida", married=TRUE)
  {
  person <- list(
    first=firstName, 
    last=lastName, 
    years=age, 
    location=state, 
    union=married
    )
  
  class(person) <- append(class(person), "demoData3")
  return(person)
}

johnie <- demoData3()
johnie

johnie$first