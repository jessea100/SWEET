#Summary Statistics 

- Summary Statistics for Percentages of respondents with diabetes across different age groups (e.g., 18-24, 25-34, 35-44, etc.).

```{r}
# Load required libraries if not already loaded
library(gtsummary)
library(dplyr)
library(readr)

# Read your data (adjust path and read function as per your file)
data <- read_csv("/Users/jesseageorgia/Desktop/SWEET/PROJECT/DIABETESDATA.CSV")

# Ensure 'diabetes_binary' is a factor or character column
data$diabetes_binary <- factor(data$diabetes_binary)

# Assuming 'age' column is already in a suitable format or converted to numeric

# Create age groups based on 'age' column
data <- data %>%
  mutate(age_group = cut(as.numeric(age), breaks = c(18, 24, 34, 44, 54, 64, Inf), labels = c("18-24", "25-34", "35-44", "45-54", "55-64", "65+")))

# Create a gtsummary table to display percentages of diabetes by age group
tbl <- data %>%
  mutate(diabetes_status = ifelse(diabetes_binary == "Diabetes", "Diabetic", "Non-Diabetic")) %>%
  group_by(age_group, diabetes_status) %>%
  summarise(count = n()) %>%
  mutate(percent = count / sum(count) * 100) %>%
  filter(diabetes_status == "Diabetic") %>%
  select(age_group, percent) %>%
  gtsummary::tbl_summary(
    statistic = list(
      percent = "{mean} ({sd})"
    ),
    missing = "no"
  )

# Print the table
tbl
```

```{r calculate-percentages}
# Calculate percentages of respondents with diabetes across different age groups
library(dplyr)

# Assuming 'data' is your cleaned dataset with variables including 'age' and 'diabetes_binary'
# Ensure 'age' is a character variable with appropriate age group labels

# Aggregate data by age and calculate percentage with diabetes
diabetes_percentages <- data %>%
  group_by(age) %>%
  summarize(percent_diabetes = mean(diabetes_binary == "Diabetes") * 100)

# Define order of age groups for table display
age_order <- c("18 - 24", "25 - 29", "30 - 34", "35 - 39", 
               "40 - 44", "45 - 49", "50 - 54", "55 - 59", 
               "60 - 64", "65 - 69", "70 - 74", "75 - 79", "80 - 99")

# Reorder based on age_order
diabetes_percentages <- diabetes_percentages %>%
  mutate(age = factor(age, levels = age_order))

# Print table using kable for Markdown format
knitr::kable(diabetes_percentages, 
             caption = "Percentage of respondents with diabetes across different age groups",
             col.names = c("Age Group", "Percentage with Diabetes"))
```

```{r calculate-percentages}
library(dplyr)

# Calculate percentages of diabetes by age group
diabetes_percentages <- data %>%
  group_by(age) %>%
  summarize(percent_diabetes = mean(diabetes_binary == "Diabetes") * 100) %>%
  arrange(match(age, c("18 - 24", "25 - 29", "30 - 34", "35 - 39", "40 - 44", 
                       "45 - 49", "50 - 54", "55 - 59", "60 - 64", "65 - 69", 
                       "70 - 74", "75 - 79", "80 - 99")))

# Ensure 'age' is treated as a factor with specified order
diabetes_percentages$age <- factor(diabetes_percentages$age, levels = c("18 - 24", "25 - 29", "30 - 34", "35 - 39", "40 - 44", 
                                                                       "45 - 49", "50 - 54", "55 - 59", "60 - 64", "65 - 69", 
                                                                       "70 - 74", "75 - 79", "80 - 99"))

# Load psych package for summary statistics
library(psych)

# Summary statistics for diabetes percentages by age group
summary_table <- diabetes_percentages %>%
  describe() %>%
  select(variable, mean, sd, median, min, max)

# Rename columns for clarity
colnames(summary_table) <- c("Age Group", "Mean (%)", "Std Dev", "Median (%)", "Min (%)", "Max (%)")

# Print the summary table
print(summary_table)


```
```{r calculate-percentages}
# Load required libraries
library(dplyr)

# Assuming 'data' is your cleaned dataset with variables including 'age' and 'diabetes_binary'
# Ensure 'age' is a character variable with appropriate age group labels

# Aggregate data by age and calculate percentages for each category
diabetes_percentages <- data %>%
  group_by(age) %>%
  summarize(
    percent_pre_diabetic = mean(diabetes_binary == "Pre-diabetic") * 100,
    percent_no_diabetes = mean(diabetes_binary == "No Diabetes") * 100,
    percent_diabetes = mean(diabetes_binary == "Diabetes") * 100
  )

# Define order of age groups for table display
age_order <- c("18 - 24", "25 - 29", "30 - 34", "35 - 39", 
               "40 - 44", "45 - 49", "50 - 54", "55 - 59", 
               "60 - 64", "65 - 69", "70 - 74", "75 - 79", "80 - 99")

# Reorder based on age_order
diabetes_percentages <- diabetes_percentages %>%
  mutate(age = factor(age, levels = age_order))

# Print table using kable for Markdown format
knitr::kable(diabetes_percentages, 
             caption = "Percentage of respondents in different diabetes categories across age groups",
             col.names = c("Age Group", "Pre-diabetic (%)", "No Diabetes (%)", "Diabetes (%)"))
```


```{r calculate-percentages}
# Load required libraries
library(dplyr)

# Assuming 'data' is your cleaned dataset with variables including 'age' and 'diabetes_binary'
# Ensure 'age' is a character variable with appropriate age group labels

# Aggregate data by age and calculate percentages for each category
diabetes_percentages <- data %>%
  group_by(age) %>%
  summarize(
    percent_prediabetes = mean(diabetes_binary == "Prediabetes") * 100,
    percent_No_diabetes = mean(diabetes_binary == "No Diabetes") * 100,
    percent_diabetes = mean(diabetes_binary == "Diabetes") * 100
  )

# Define order of age groups for table display
age_order <- c("18 - 24", "25 - 29", "30 - 34", "35 - 39", 
               "40 - 44", "45 - 49", "50 - 54", "55 - 59", 
               "60 - 64", "65 - 69", "70 - 74", "75 - 79", "80 - 99")

# Reorder based on age_order
diabetes_percentages <- diabetes_percentages %>%
  mutate(age = factor(age, levels = age_order))

# Print table using kable for Markdown format
knitr::kable(diabetes_percentages, 
             caption = "Percentage of respondents in different diabetes categories across age groups",
             col.names = c("Age Group", "Prediabetes (%)", "No Diabetes (%)", "Diabetes (%)"))
```
```{r calculate-percentages}
# Load required libraries
library(dplyr)

# Assuming 'data' is your cleaned dataset with variables including 'age' and 'diabetes_binary'
# Ensure 'age' is a character variable with appropriate age group labels

# Aggregate data by age and calculate percentage of respondents with No Diabetes
no_diabetes_percentages <- data %>%
  group_by(age) %>%
  summarize(
    percent_no_diabetes = mean(diabetes_binary == "No Diabetes") * 100
  )

# Define order of age groups for table display
age_order <- c("18 - 24", "25 - 29", "30 - 34", "35 - 39", 
               "40 - 44", "45 - 49", "50 - 54", "55 - 59", 
               "60 - 64", "65 - 69", "70 - 74", "75 - 79", "80 - 99")

# Reorder based on age_order
no_diabetes_percentages <- no_diabetes_percentages %>%
  mutate(age = factor(age, levels = age_order))

# Print table using kable for Markdown format
knitr::kable(no_diabetes_percentages, 
             caption = "Percentage of respondents with No Diabetes across different age groups",
             col.names = c("Age Group", "Percentage with No Diabetes"))
```



