
# EDA examples

# Load necessary packages
# For data manipulation
library(dplyr)
# For data tidying
library(tidyr)
# For data visualization
library(ggplot2)

# Set seed for reproducibility
set.seed(123)

# Create a dirty dataset with missing values,
#   incorrect types, and outliers
dirty_data <- data.frame(
    id = 1:100,
    age = c(sample(18:90, 90, replace = TRUE),
            rep("Unknown", 10)),
    gender = c(sample(c("Male", "Female"),
                      90,
                      replace = TRUE),
               rep(NA, 10)),
    income = c(rnorm(80,
                     mean = 50000,
                     sd = 10000),
               rep(999999, 10),
               rep(NA, 10)),
    stringsAsFactors = FALSE
)

# View the first five rows
head(dirty_data, n = 5)

# Step 1: Data Cleaning

# Convert age to numeric, setting non-numeric values to NA
dirty_data$age <- as.numeric(dirty_data$age)
# This is expected due to non-numeric values
#   warning("NAs introduced by coercion")

# Replace outlier income values with NA
dirty_data$income[dirty_data$income > 100000] <- NA

# Handle missing values
# Here we remove rows with any missing values for simplicity.
#   Depending on your data and task, you may want to handle
#   missing values differently (e.g., imputation).
clean_data <- dirty_data %>% drop_na()

# Step 2: Data Exploration

# Summary statistics
summary(clean_data)

# Age distribution
ggplot(clean_data, aes(x = age)) +
    geom_histogram(bins = 30,
                   fill = 'blue',
                   color = 'black') +
    theme_minimal() +
    labs(title = "Age Distribution",
         x = "Age",
         y = "Count")

# Income by gender
ggplot(clean_data, aes(x = gender,
                       y = income)) +
    geom_boxplot(fill = 'blue',
                 color = 'black') +
    theme_minimal() +
    labs(title = "Income by Gender",
         x = "Gender",
         y = "Income")


# Create boxplot for age
boxplot(dirty_data$age,
        main = "Age",
        ylab = "Years",
        xlab = "Age",
        na.rm = TRUE)
# Add data points for age
points(jitter(rep(1,
                  length(dirty_data$age[!is.na(dirty_data$age)]))),
              dirty_data$age[!is.na(dirty_data$age)],
              col = 'blue')

# Create boxplot for income
boxplot(dirty_data$income,
        main = "Income",
        ylab = "Income",
        xlab = "Income",
        na.rm = TRUE)
# Add data points for income
points(jitter(rep(1, length(dirty_data$income[!is.na(dirty_data$income)]))),
       dirty_data$income[!is.na(dirty_data$income)],
       col = 'blue')

# Frequency counts
print(table(dirty_data$gender))
# Female   Male
#  49      41

# Store the gender count
gender_count <- table(dirty_data$gender)

# Create the barplot and capture the midpoints of the bars
midpoints <- barplot(gender_count,
                     main = "Gender Distribution",
                     xlab = "Gender",
                     ylab = "Count",
                     ylim = c(0, max_count))

# For text placement halfway up each bar
label_pos <- gender_count / 2

# Add the counts to the bars
text(x = midpoints,
     y = label_pos,
     labels = gender_count,
     col = "black")

# Pie chart
colors <- c("blue", "red")

pie(gender_count,
    main = "Gender Distribution",
    col = colors)

