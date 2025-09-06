---
title: "Lab 0: Getting Started with dplyr"
subtitle: "Your First Data Analysis"
author: "Your Name Here"
date: today
format: 
  html:
    code-fold: false
    toc: true
    toc-location: left
    theme: cosmo
execute:
  warning: false
  message: false
---

# Overview

Welcome to your first lab! In this assignment, you'll practice the fundamental dplyr operations we covered in class using car sales data. This lab will help you get comfortable with:

- Basic data exploration
- Column selection and manipulation  
- Creating new variables
- Filtering data
- Grouping and summarizing

**Instructions:** Copy this template into your portfolio repository under a `lab_0/` folder, then complete each section with your code and answers.

# Setup

```{r setup}
# Load the tidyverse library
library(tidyverse)

# Read in the car sales data
# Make sure the data file is in your lab_0/data/ folder
car_data <- read_csv("data/car_sales_data.csv")
```

# Exercise 1: Getting to Know Your Data

## 1.1 Data Structure Exploration

Explore the structure of your data and answer these questions:

```{r explore-structure}
# Use glimpse() to see the data structure
glimpse(car_data)

# Check the column names
names(car_data)

# Look at the first few rows
car_data
```

**Questions to answer:**
- How many rows and columns does the dataset have?
- What types of variables do you see (numeric, character, etc.)?
- Are there any column names that might cause problems? Why?

**Your answers:**
- Rows: [YOUR ANSWER]
- Columns: [YOUR ANSWER]  
- Variable types: [YOUR ANSWER]
- Problematic names: [YOUR ANSWER]

## 1.2 Tibble vs Data Frame

Compare how tibbles and data frames display:

```{r tibble-vs-dataframe}
# Look at the tibble version (what we have)
car_data

# Convert to regular data frame and display
car_df <- as.data.frame(car_data)
car_df
```

**Question:** What differences do you notice in how they print?

**Your answer:** [YOUR ANSWER]

# Exercise 2: Basic Column Operations

## 2.1 Selecting Columns

Practice selecting different combinations of columns:

```{r column-selection}
# Select just Model and Mileage columns
select(car_data, Model, Mileage)

# Select Manufacturer, Price, and Fuel type
select(car_data, Manufacturer, Price, `Fuel type`)

# Challenge: Select all columns EXCEPT Engine and Transmission
select(car_data, -Engine, -Transmission)
```

## 2.2 Renaming Columns

Let's fix that problematic column name:

```{r rename-columns}
# Rename 'Year of manufacture' to 'year'
car_data <- rename(car_data, year = `Year of manufacture`)

# Check that it worked
names(car_data)
```

**Question:** Why did we need backticks around `Year of manufacture` but not around `year`?

**Your answer:** [YOUR ANSWER]

# Exercise 3: Creating New Columns

## 3.1 Calculate Car Age

```{r calculate-age}
# Create an 'age' column (2025 minus year of manufacture)
car_data <- mutate(car_data, age = 2025 - year)

# Create a mileage_per_year column  
car_data <- mutate(car_data, mileage_per_year = Mileage / age)

# Look at your new columns
select(car_data, Model, year, age, Mileage, mileage_per_year)
```

## 3.2 Categorize Cars

```{r categorize-cars}
# Create a price_category column
car_data <- mutate(car_data, 
  price_category = case_when(
    Price < 15000 ~ "budget",
    Price >= 15000 & Price <= 30000 ~ "mid-range",
    Price > 30000 ~ "luxury"
  )
)

# Check your categories
select(car_data, Model, Price, price_category)
```

# Exercise 4: Filtering Practice

## 4.1 Basic Filtering

```{r basic-filtering}
# Find all Toyota cars
toyota_cars <- filter(car_data, Manufacturer == "Toyota")
toyota_cars

# Find cars with mileage less than 30,000
low_mileage <- filter(car_data, Mileage < 30000)
low_mileage

# Find luxury cars with low mileage
luxury_low_miles <- filter(car_data, price_category == "luxury" & Mileage < 30000)
luxury_low_miles
```

## 4.2 Multiple Conditions

```{r multiple-conditions}
# Find cars that are EITHER Honda OR Nissan
honda_nissan <- filter(car_data, Manufacturer == "Honda" | Manufacturer == "Nissan")

# Find cars with price between $20,000 and $35,000
mid_price <- filter(car_data, Price >= 20000 & Price <= 35000)

# Challenge: Find diesel cars less than 10 years old
young_diesel <- filter(car_data, `Fuel type` == "Diesel" & age < 10)
young_diesel
```

**Question:** How many diesel cars are less than 10 years old?

**Your answer:** [YOUR ANSWER]

# Exercise 5: Grouping and Summarizing

## 5.1 Basic Summaries

```{r basic-summaries}
# Calculate average price by manufacturer
avg_price_by_brand <- car_data %>%
  group_by(Manufacturer) %>%
  summarize(avg_price = mean(Price, na.rm = TRUE))

avg_price_by_brand

# Calculate average mileage by fuel type
avg_mileage_by_fuel <- car_data %>%
  group_by(`Fuel type`) %>%
  summarize(avg_mileage = mean(Mileage, na.rm = TRUE))

avg_mileage_by_fuel

# Count cars by manufacturer
car_counts <- car_data %>%
  group_by(Manufacturer) %>%
  summarize(count = n())

car_counts
```

## 5.2 More Complex Summaries

```{r complex-summaries}
# Multiple statistics by manufacturer
manufacturer_summary <- car_data %>%
  group_by(Manufacturer) %>%
  summarize(
    avg_price = mean(Price, na.rm = TRUE),
    count = n(),
    oldest_year = min(year, na.rm = TRUE),
    newest_year = max(year, na.rm = TRUE),
    .groups = "drop"
  )

manufacturer_summary
```

## 5.3 Categorical Summaries

```{r categorical-summaries}
# Frequency table for price categories
price_category_summary <- car_data %>%
  group_by(price_category) %>%
  summarize(count = n()) %>%
  mutate(percentage = count / sum(count) * 100)

price_category_summary
```

**Question:** What percentage of cars fall into each price category?

**Your answer:** [YOUR ANSWER]

# Exercise 6: Putting It All Together

## 6.1 Mini Analysis Challenge

Filter for cars from 2015 or newer, then analyze by fuel type:

```{r mini-analysis}
recent_cars_analysis <- car_data %>%
  filter(year >= 2015) %>%
  group_by(`Fuel type`) %>%
  summarize(
    count = n(),
    avg_price = mean(Price, na.rm = TRUE),
    avg_mileage = mean(Mileage, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(avg_price))

recent_cars_analysis
```

**Questions:** 
- Which fuel type has the highest average price among recent cars?
- Which has the lowest average mileage?

**Your answers:** 
- Highest price: [YOUR ANSWER]
- Lowest mileage: [YOUR ANSWER]

## 6.2 Your Own Exploration

Come up with your own question about the car data and use dplyr to answer it:

**Your research question:** [Write your question here]

```{r your-analysis}
# Write your code here to answer your question

```

**What you found:** [Describe your findings]

# Reflection

Write a brief reflection (3-4 sentences) on this lab:

1. **What did you learn?** [YOUR ANSWER]

2. **What was most challenging?** [YOUR ANSWER]

3. **What connections do you see to policy analysis?** Think about how these same techniques might apply to census data, crime statistics, housing markets, etc. [YOUR ANSWER]

4. **What questions do you still have?** [YOUR ANSWER]

# Submission Notes

**To submit this lab:**
1. Make sure your code runs without errors
2. Fill in all the "[YOUR ANSWER]" sections  
3. Save this file in your portfolio's `lab_0/` folder
4. Commit and push to GitHub
5. Check that it appears on your GitHub Pages portfolio site

**Due:** [INSERT DUE DATE]

**Questions?** Post on the course discussion board or come to office hours!