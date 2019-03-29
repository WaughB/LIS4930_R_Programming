# Brett Waugh
# 13 March 2019
# mod9.R
# Visualization in R

# Required libraries.
require(ggplot2)
require(lattice)
require(readr)

# Get the data in. 
data <- read_csv("/home/brett/LIS4930_Visual_Analytics/data/Presidential_Approval.csv", col_types = cols(X1 = col_skip(), iraq.war = col_logical()))

# Create a plot with no additional packages. 
plot(x=data$year, y=data$approve, main = "Presidential Approval by Year", sub = "based on data from: https://cran.r-project.org/web/packages/lattice/index.html", xlab = "Year", ylab = "U.S. Presidential Approval (%)")

# Create a plot using lattice.
xyplot(data$approve ~ data$year, data, grid = TRUE, xlab = "Year", ylab = "U.S. Presidential Approval (%)", group = data$month, main="U.S. President Approval", sub = "based on data from: https://cran.r-project.org/web/packages/lattice/index.html")

# Create a plot using ggplot2. 
ggplot(data, aes(x=data$year, y=data$approve, color = data$month)) + geom_point() + labs(x = "Year", y = "U.S. Presidential Approval (%)", color = "Month", title = "U.S. Presidential Approval", caption = "based on data from: https://cran.r-project.org/web/packages/lattice/index.html")


