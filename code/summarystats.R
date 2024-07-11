#Percentages of respondents with prediabetes across different age groups (e.g., 18-24, 25-34, 35-44, etc.).

# Load required libraries
library(dplyr)

# Assuming 'data' is your cleaned dataset with variables including 'age' and 'diabetes_binary'
# Ensure 'age' is a character variable with appropriate age group labels

# Aggregate data by age and calculate percentage with Prediabetes
prediabetes_percentages <- data %>%
  group_by(age) %>%
  summarize(
    n_participants = n(),  # Calculate number of participants per age group
    percent_prediabetes = mean(diabetes_binary == "Prediabetes") * 100
  )

# Define order of age groups for table display
age_order <- c("18 - 24", "25 - 29", "30 - 34", "35 - 39", 
               "40 - 44", "45 - 49", "50 - 54", "55 - 59", 
               "60 - 64", "65 - 69", "70 - 74", "75 - 79", "80 - 99")

# Reorder based on age_order
prediabetes_percentages <- prediabetes_percentages %>%
  mutate(age = factor(age, levels = age_order))

# Print table using kable for Markdown format with adjusted alignment and column spacing
knitr::kable(prediabetes_percentages, 
             caption = "Percentage of respondents with Prediabetes across age groups",
             col.names = c("Age Group", "Number of Participants", "Prediabetes (%)"),
             align = c("l", "c", "c"),  # Adjust alignment: left for Age Group, center for numbers
             digits = 1,               # Round percentages to one decimal place
             format.args = list(big.mark = ",", decimal.mark = ".", scientific = FALSE),  # Formatting for numbers
             col.widths = c("8em", "8em", "6em"))  # Adjust column widths
