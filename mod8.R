# Brett Waugh
# 22 February 2019
# mod8.R
# Input/Output, string manipulation and plyr package

# Necessary libraries.
require(readr)
require(pryr)
require(plyr)
require(ISLR)
require(boot)
require(data.table)

# Load in data. 
dataset6 <- read_csv("LIS4930_R_Programming/Assignment 6 Dataset.txt")

# Computes student averages. 
StudentAverage = ddply(dataset6,"Sex",transform,Grade.Average=mean(Grade))
StudentAverage

# Write output to file. 
write.table(StudentAverage, file= "~/Students_Gendered_Mean.csv", quote=FALSE, row.names = FALSE, sep=",")

# Search for only names with the letter "i" in them.
iNames = subset(dataset6,grepl("[iI]",dataset6$Name))

# Output to another file. 
write.table(iNames,file ="~/DataSubset.csv", quote=FALSE, row.names = FALSE,sep=",")

