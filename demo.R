# Load necessary packages
library(psych)
library(dplyr)

# Simulate survey data
survey_data <- data.frame(
  satisfaction = c(5, 4, 4, 5, 4, 3, 5, 5, 4, 3, 5, 4, 4, 5, 3, 4, 5, 4, 3, 4),
  collaboration = c(4, 3, 4, 4, 3, 2, 4, 4, 3, 2, 4, 3, 4, 3, 2, 3, 4, 3, 2, 3),
  workload = c(1, 2, 2, 1, 3, 4, 1, 2, 4, 3, 1, 2, 3, 2, 3, 4, 1, 2, 4, 3),  # Low correlation
  leadership = c(5, 4, 4, 5, 4, 5, 5, 4, 4, 4, 5, 4, 4, 5, 4, 5, 5, 4, 4, 5),
  culture = c(4, 4, 3, 5, 4, 4, 5, 5, 3, 4, 5, 4, 4, 5, 3, 4, 5, 4, 3, 4),
  recognition = c(5, 5, 4, 5, 5, 4, 5, 5, 5, 4, 5, 5, 4, 5, 4, 5, 5, 5, 4, 5)
)


# Print the first few rows of data
head(survey_data)

# Calculate Cronbach's Alpha
alpha_result <- psych::alpha(survey_data)

# Print results
alpha_result$total

# Item-total statistics
alpha_result$item.stats

# Remove the low-correlated item
refined_data <- survey_data %>% select(-workload)
alpha_refined <- psych::alpha(refined_data)
alpha_refined$total


# Perform Factor Analysis
fa_result <- fa(refined_data, nfactors = 2, rotate = "varimax")  # Adjust nfactors if needed

summary(fa_result)

fa_result$loadings
