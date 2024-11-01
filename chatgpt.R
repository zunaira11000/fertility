# Load necessary libraries
library(ggplot2)
library(dplyr)

# Load your data (adjust the path and name as needed)
data <- read.csv("D:/fertility_Diagnosis.csv")

# Ensure Diagnosis is a factor for proper plotting and analysis
data$Diagnosis <- as.factor(data$Diagnosis)

# 1. Smoking Habit vs Diagnosis (Boxplot)
ggplot(data, aes(x = Diagnosis, y = Smoking.habit, fill = Diagnosis)) +
  geom_boxplot() +
  labs(title = "Smoking Habit by Diagnosis", x = "Diagnosis", y = "Smoking Habit") +
  theme_minimal()

# 2. Histograms of Continuous Variables (Age and Number of hours spent sitting per day)
# Histogram for Age
ggplot(data, aes(x = Age)) +
  geom_histogram(binwidth = 0.05, color = "black", fill = "skyblue") +
  labs(title = "Histogram of Age", x = "Age", y = "Frequency") +
  theme_minimal()

# Histogram for Number of hours spent sitting per day
ggplot(data, aes(x = Number.of.hours.spent.sitting.per.day)) +
  geom_histogram(binwidth = 0.05, color = "black", fill = "orange") +
  labs(title = "Histogram of Hours Spent Sitting per Day", x = "Hours Sitting per Day", y = "Frequency") +
  theme_minimal()

# 3. Calculating Mean and Standard Deviation
# Overall mean and standard deviation for continuous variables
continuous_vars <- data %>% select(Age, Smoking.habit, Number.of.hours.spent.sitting.per.day)
summary_stats <- continuous_vars %>%
  summarise_all(list(mean = ~mean(.), sd = ~sd(.)))

print("Summary statistics (mean and standard deviation) for continuous variables:")
print(summary_stats)

# Mean and SD by Diagnosis Group
grouped_stats <- data %>%
  group_by(Diagnosis) %>%
  summarise(
    Mean_Age = mean(Age, na.rm = TRUE),
    SD_Age = sd(Age, na.rm = TRUE),
    Mean_Smoking_Habit = mean(Smoking.habit, na.rm = TRUE),
    SD_Smoking_Habit = sd(Smoking.habit, na.rm = TRUE),
    Mean_Hours_Sitting = mean(Number.of.hours.spent.sitting.per.day, na.rm = TRUE),
    SD_Hours_Sitting = sd(Number.of.hours.spent.sitting.per.day, na.rm = TRUE)
  )

print("Summary statistics by Diagnosis group:")
print(grouped_stats)

