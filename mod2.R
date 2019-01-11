# Brett Waugh
# 11 January 2019
# Module # 2 Introduction to basic R functions
# and Data Structures 

# Given the dataset.
assignment2 <- c(16, 18, 14, 22, 27, 17, 19, 17, 17, 22, 20, 22)

# Explain why this function does not work. 
# myMean <- function(passedValue) { return(sum(passedValue)/length(passedValue)) }
# The function does not work because different variables were used. 

# The corrected version is shown below.
myMean <- function(passedValue) 
  + { return
      (
      sum(passedValue)/length(passedValue)
      ) 
    }