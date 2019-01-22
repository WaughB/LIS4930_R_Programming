# Brett Waugh
# 21 January 2019
# Module 3 Introduction to Data.frame

# Given this psuedodata. 
name <- c("Jeb", "Donald", "Ted", "Marco", "Carly", "Hillary", "Berine")

abc_poll <- c(4, 62, 51, 21, 2, 14, 15)

cbs_poll <- c(12, 75, 43, 19, 1, 21, 19) 

# Display the polling data. 
polling <- cbind(name, abc_poll, cbs_poll)
polling

# Turn polling into a dataframe. 
polling.df <- data.frame(name, abc_poll, cbs_poll)
polling.df 

# Basic stats.
sapply(polling.df, mean, na.rm = TRUE)

sapply(polling.df[,2:3], mean, na.rm = TRUE)


