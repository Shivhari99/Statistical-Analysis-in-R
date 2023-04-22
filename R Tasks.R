## TASK 1

#Q1. To compute the mean, median, and mode of sepal width in the iris dataset, you can use the following code:

# Load the iris dataset
data(iris)

# Compute the mean, median and mode of sepal width
mean_sw <- mean(iris$Sepal.Width)
median_sw <- median(iris$Sepal.Width)
mode_sw <- names(sort(-table(iris$Sepal.Width)))[1]

# Print the results
cat("Mean Sepal Width:", mean_sw, "\n")
cat("Median Sepal Width:", median_sw, "\n")
cat("Mode Sepal Width:", mode_sw, "\n")


#Q2. To calculate the minimum, maximum and range of sepal width, you can use the following code:

# Calculate the minimum, maximum and range of sepal width
min_sw <- min(iris$Sepal.Width)
max_sw <- max(iris$Sepal.Width)
range_sw <- max_sw - min_sw

# Print the results
cat("Minimum Sepal Width:", min_sw, "\n")
cat("Maximum Sepal Width:", max_sw, "\n")
cat("Range of Sepal Width:", range_sw, "\n")


#Q3. To calculate the interquartile range (IQR) of sepal width and the first and third quartiles, you can use the following code:

# Calculate the interquartile range and quartiles of sepal width
iqr_sw <- IQR(iris$Sepal.Width)
quartiles_sw <- quantile(iris$Sepal.Width, probs = c(0.25, 0.75))

# Print the results
cat("IQR of Sepal Width:", iqr_sw, "\n")
cat("Quartiles of Sepal Width:", quartiles_sw, "\n")

#Q4. To show the minimum, maximum, mean, median, and first and third quartiles all at once for sepal width, you can use the following code:

# Calculate summary statistics for sepal width
summary_sw <- summary(iris$Sepal.Width)

# Print the results
cat("Summary statistics for Sepal Width:\n", summary_sw)

#Q5. To compute the mean and quartiles of each numeric column in the iris dataset using the sapply function, you can use the following code:

# Compute mean and quartiles of each numeric column
stats <- sapply(iris[, 1:4], function(x) c(mean = mean(x), quartiles = quantile(x, probs = c(0.25, 0.5, 0.75))))

# Print the results
cat("Summary statistics for each numeric column:\n", stats)

#Q6: To create a histogram for sepal width with the specified arguments, you can use the following code:

# Create a histogram for sepal width
hist(iris$Sepal.Width,
     main = "Histogram for Sepal Width",
     xlab = "Sepal Width",
     border = "green",
     col = "blue")

#Q7. Use function ggplot() to create one plot containing four boxplots of sepal length, sepal width, petal length and petal width using 4 different colours

# Load the ggplot2 library
# Load the ggplot2 and reshape2 libraries
install.packages("reshape2")
library(ggplot2)
library(reshape2)

# Create a data frame with the four variables
data <- iris[, c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width")]

# Create the plot with four boxplots
ggplot(melt(data), aes(x = variable, y = value)) +
  geom_boxplot(fill = c("blue", "green", "red", "purple"), color = "black") +
  labs(title = "Boxplots of Iris Variables",
       x = "Variable",
       y = "Value")



#Q8. Use qqnorm() and qqplot() to create a QQ plot for sepal width. You will need to: set the reference line colour as red, and its width as 3)

qqnorm(iris$Sepal.Width)
qqline(iris$Sepal.Width, col = "red", lwd = 3)


#The QQ plot compares the distribution of the sepal width variable to a normal distribution. The points on the plot represent the observed values of sepal width, and the line represents the expected values of sepal width if it were normally distributed.

#If the points on the plot closely follow the line, then the sepal width variable can be considered approximately normally distributed. If the points deviate significantly from the line, then the sepal width variable may not be normally distributed.

#Therefore, based on the QQ plot generated, we can determine whether or not sepal width is normally distributed.


  
#TASK 2


#Q1. Choose the right function to display the structure and the summary statistics of the dataset txhousing, and then briefly describe this dataset (data structure, number of variables, meaning of each variable, number of records,) using your own words.



# Load the ggplot2 library
library(ggplot2)

# View the structure of the txhousing dataset
str(ggplot2::txhousing)

# View the summary statistics of the txhousing dataset
summary(ggplot2::txhousing)


#The txhousing dataset is a built-in dataset in the ggplot2 package. It contains information on the sales of houses in Texas from 2000 to 2008. The dataset has 8 variables and 8604 records.

#Here is a brief description of each variable in the txhousing dataset:

#city: The name of the city where the house is located.
#year: The year in which the house was sold.
#month: The month in which the house was sold.
#sales: The number of houses sold in that month in that city.
#volume: The total volume of houses sold in that month in that city (in millions of dollars).
#median: The median price of the houses sold in that month in that city (in thousands of dollars).
#list: The average list price of the houses for sale in that month in that city (in thousands of dollars).
#age: The median age of the houses sold in that month in that city (in years).
#The dataset is in a data frame format and can be accessed using the ggplot2::txhousing command. The structure of the dataset shows the number of variables, the data type of each variable, and the first few observations of the dataset. The summary statistics provide information about the central tendency, spread, and shape of each variable in the dataset.

#Q2. Convert the data structure of variable ‘year’ into factor and then using geom_density() to show a density plot of yearly median sale prices with colour indicating different years and interpret the resulting plot.'''

# Install ggplot2 package (if not installed)
install.packages("ggplot2")

# Load ggplot2 package
library(ggplot2)


# Convert the year variable to a factor
txhousing$year <- factor(txhousing$year)

# Create a density plot of yearly median sale prices
ggplot(txhousing, aes(x = median, fill = year)) +
  geom_density(alpha = 0.5) +
  scale_fill_brewer(palette = "Set1") +
  labs(title = "Density Plot of Yearly Median Sale Prices",
       x = "Median Sale Price (Thousands of Dollars)",
       y = "Density",
       fill = "Year")

#Q3. Plot and explain: What is the monthly trend of sales in the city of ‘Abilene’? If you were to buy a house in Abilene, what is the best/worst month to check the housing market inAbilene?


library(ggplot2)
library(tidyverse)

# Load the txhousing dataset
df <- ggplot2::txhousing

# Filter the dataset to keep only records for Abilene
abilene_sales <- filter(df, city == "Abilene")

# Convert the time-series object to a data frame
abilene_sales_df <- as.data.frame(abilene_sales)

# Convert the year variable to a factor
abilene_sales_df$year <- factor(abilene_sales_df$year)

# Create a line plot of monthly sales in Abilene
ggplot(abilene_sales_df, aes(x = month, y = sales, group = 1, color = year)) +
  geom_point() +
  scale_color_brewer(palette = "Set1") +
  labs(title = "Monthly Trend of Sales in Abilene",
       x = "Month",
       y = "Number of Sales",
       color = "Year")





