#' ---
#' title: "Student Marks Mini-Project"
#' ---

#' ## 1. Dataset Creation
students <- data.frame(
  Name = c("Alice", "Bob", "Charlie", "Diana", "Eve", "Frank", "Grace", "Hank", "Ivy", "Jack"),
  Gender = as.factor(c("F", "M", "M", "F", "F", "M", "F", "M", "F", "M")),
  Age = c(20, 21, 20, 22, 19, 21, 20, 23, 19, 22),
  Study_Hours = c(5, 3, 6, 4, 7, 2, 8, 3, 5, 4),
  Math_Score = c(85, 60, 88, 75, 92, 55, 95, 62, 80, 70),
  Science_Score = c(80, 65, 85, 78, 88, 60, 90, 58, 82, 75)
)

head(students)

#' ## 2. Mean, Median, and Standard Deviation

science_mean <- mean(students$Science_Score)
science_median <- median(students$Science_Score)
science_sd <- sd(students$Science_Score)

# Print Science Stats
cat("Science - Mean:", science_mean, " Median:", science_median, " SD:", science_sd, "\n")

study_mean <- mean(students$Study_Hours)
study_median <- median(students$Study_Hours)
study_sd <- sd(students$Study_Hours)

# Print Study Hours Stats
cat("Study Hours - Mean:", study_mean, " Median:", study_median, " SD:", study_sd, "\n")

#' ## 3. Visualizations

library(ggplot2)

# Scatterplot
ggplot(students, aes(x = Study_Hours, y = Science_Score)) +
  geom_point(color="blue", size=3) +
  geom_smooth(method = "lm", color="red", se=FALSE) +
  labs(title = "Study Hours Vs Science Score",
       x = "Hours Spent", y = "Science Score") +
  theme_minimal()

# Boxplot
ggplot(students, aes(x = Gender, y = Science_Score, fill = Gender)) +
  geom_boxplot() +
  scale_fill_manual(values = c("F" = "lightpink", "M" = "lightblue")) +
  labs(title = "Science Score by Gender",
       x = "Gender", y = "Science Score") +
  theme_minimal()

# Barplot
gender_counts_df <- as.data.frame(table(students$Gender)) 
colnames(gender_counts_df) <- c("Gender", "Count")

ggplot(gender_counts_df, aes(x = Gender, y = Count)) +
  geom_col(fill = "darkgreen") +
  labs(title = "Count of Students by Gender", 
       x = "Gender", 
       y = "Number of Students") +
  theme_minimal()

#' ## 4. Correlation and Heatmaps

# Correlation between two variables
cor_value <- cor(students$Study_Hours, students$Science_Score)
cat("Correlation between Study Hours and Science Score:", cor_value, "\n")

# Heatmap
numeric_data <- students[, c("Age", "Study_Hours", "Math_Score", "Science_Score")]
my_correlations <- cor(numeric_data)
heatmap(my_correlations, Rowv = NA, Colv = NA, col = heat.colors(10))

#' ## 5. Interpretation
#' 
#' **Statistics:** The average mean of Science_score in the dataset is 76.1, with a median of 79. The standard deviation is approximately 11.44, indicating moderate variability in how students performed.
#' 
#' **Correlation:** The correlation between the 2 variables is 0.92.
#' 
#' **Visualizations:** 
#' 
#' *   **Scatterplot:** The plot demonstrates a clear upward trend, showing that as the number of study hours increases, the student's Science score predictably rises. 
#' *   **Box Plot:** The box plot illustrates the distribution of Science scores between genders. In this specific small sample, female students showed a higher median score and less variance compared to male students.
#' *   **Heatmap:** Intense colors at the intersections of Study Hours, Math Score, and Science Score indicate they all move closely together. Age showed little to no correlation with academic performance.