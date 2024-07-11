#Percentages of respondents with prediabetes across different age groups (e.g., 18-24, 25-34, 35-44, etc.).

# Load required libraries
library(tidyverse) # Changed from dplyr
library(here) # Added by Abhi
library(gtsummary) # Added by Abhi

# Here i am
here::i_am("code/summarystats.R") # Added by Abhi

# Read in cleaned data
data <- readRDS(here("data/cleaned_data/cleaned_data_basic.RDS")) # Added by Abhi

# Recode diabetes_binary to be 0 = No diabetes, and 1 = Diabetes
data <- data %>%
  mutate(diabetes_binary = factor(ifelse(diabetes_binary == "No diabetes", "No Diabetes", "Diabetes")))


###############
# JESSEA'S CODE
###############

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

##############
# Abhi's code
##############

# Categorize BMI
# I did this so that it did not give me one row per BMI value, which was silly in a table
data <- data %>%
  # Create a new column 'bmi_category' based on BMI values
  mutate(bmi_category = case_when(
    bmi < 18.5 ~ "Underweight",  # BMI less than 18.5 is categorized as Underweight
    bmi >= 18.5 & bmi < 24.9 ~ "Normal weight",  # BMI between 18.5 and 24.9 is categorized as Normal weight
    bmi >= 25 & bmi < 29.9 ~ "Overweight",  # BMI between 25 and 29.9 is categorized as Overweight
    bmi >= 30 ~ "Obese"  # BMI 30 and above is categorized as Obese
  ))

# Create a Table 1 using gtsummary package
# This table should basically be the same as the one you made above, except it is able to have values for all the variables
# While this table already looks nice, you can do much more to it, so explore the gtsummary package. 
# Also, you may want to re-label the variables to be more informative, some of the binary variables (eg: Any access to 
# healthcare, no doctor because of cost, etc.) are not informative in the context of this table, so it may be good to re-label them
# Also, it looks like some rows have missing values, so you may want to exclude them from the table/filter them out.

table1 <- data %>%
  # Select relevant columns for the summary table
  select(
    diabetes_binary, high_bp, high_chol, chol_check, bmi_category, smoker, stroke, heart_diseaseor_attack, 
    phys_activity, fruits, veggies, hvy_alcohol_consump, any_healthcare, no_docbc_cost, 
    gen_hlth, diff_walk, sex, age, education, income
  ) %>%
  # Generate summary statistics table grouped by diabetes status
  tbl_summary(
    by = diabetes_binary,  # Group by diabetes status
    label = list(
      diabetes_binary = "Diabetes Status",
      high_bp = "High Blood Pressure",
      high_chol = "High Cholesterol",
      chol_check = "Cholesterol Check",
      bmi_category = "BMI Category",
      smoker = "Smoker",
      stroke = "Stroke",
      heart_diseaseor_attack = "Heart Disease or Attack",
      phys_activity = "Physical Activity",
      fruits = "Fruits Consumption",
      veggies = "Vegetables Consumption",
      hvy_alcohol_consump = "Heavy Alcohol Consumption",
      any_healthcare = "Any Healthcare",
      no_docbc_cost = "No Doctor Because of Cost",
      gen_hlth = "General Health",
      diff_walk = "Difficulty Walking",
      sex = "Sex",
      age = "Age",
      education = "Education",
      income = "Income"
    ),
    statistic = list(all_continuous() ~ "{mean} ({sd})", all_categorical() ~ "{n} ({p}%)"),  # Define statistics to display
    missing = "no"  # Exclude missing values from the summary
  ) %>%
  # Modify the header of the table
  modify_header(label = "**Variable**") %>%
  add_overall() %>%
  # Bold the labels in the table
  bold_labels()

# Print the table
table1
