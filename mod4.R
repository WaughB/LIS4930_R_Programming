# Brett Waugh
# 29 January
# Module # 4 Programming Structure in R 

# Load in the data.
library(readr)
hospital_patient_data <- read_csv("LIS4930_R_Programming/hospital_patient_data.csv")
View(hospital_patient_data)

# Create a dataframe.
df <- data.frame(hospital_patient_data)

# Attach data foe ease of use. 
attach(df)

# Create a boxplot for each dataset. 
boxplot(freq, first, second, finalDecision,names=c("Frequency","First","Second", "Final Decision"))

# Create a boxplot for bloodp.
boxplot(bloodp)

# Create a histogram for each dataset.
hist(freq)
hist(first)
hist(second)
hist(finalDecision)
hist(bloodp)
