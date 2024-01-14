# Comprehensive-Analysis-of-Financial-Inclusion-in-Rwanda

This R project focuses on exploring and analyzing financial inclusion data. The following steps outline the process:

## Data Loading and Exploratory Data Analysis (EDA)

```R
# Reading and exploring the data
table(duplicated(data))
str(Data88)
data8 <- Data88[,-1]
head(data8)
colSums(is.na(data8))
data2 <- laterite_mobilemoney_data
table(unique(Data88))

# Finding unique items in the data
library(dplyr)
data3 <- distinct(data43, hhid, .keep_all = TRUE)
head(data3)
View(data3)

# Adding new variables to the dataset
data3$financially_excluded <- ifelse(data3$account_type == "None", "yes", "no")
data3$digital_financial_inclusion <- ifelse(data3$account_type == "None", "No", "Yes")

# Visualizing financial exclusion according to districts
library(ggplot2)
ggplot(data3, aes(x = district, y = financially_excluded, fill = district)) +
  geom_bar(stat = "identity", width = 1)

# Overall rates for the combined population of three districts
ggplot(data = data3, aes(x = factor(district), y = prop.table(stat(count)),
                        fill = factor(financially_excluded),
                        label = scales::percent(prop.table(stat(count))))) +
  geom_bar(position = "dodge") +
  geom_text(stat = 'count', position = position_dodge(0.9), vjust = -0.5, size = 3) +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "District", y = "Financially Included", fill = "District") +
  ggtitle("Rates of Financial Exclusion")

```
## Digital Financial Inclusion Visualization

```R

# Digitally financially included
ggplot(data = data3, aes(x = factor(district), y = prop.table(stat(count)),
                        fill = factor(digital_financial_inclusion),
                        label = scales::percent(prop.table(stat(count))))) +
  geom_bar(position = "dodge") +
  geom_text(stat = 'count', position = position_dodge(0.9), vjust = -0.5, size = 3) +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "District", y = "Financially Included", fill = "District") +
  ggtitle("Rates of Financial Inclusion")
```
## Mobile Money Market Analysis
### Distribution of Mobile Money Among companies

```R
# Distribution of mobile money among companies
pie(table(data3$account_type == "Mobile Money", data3$mm_account_telco))
data3$mobile_money <- ifelse(data3$account_type == "Mobile Money", "Mobile__Money", "0")
View(data3)

ggplot(data3, aes(x = mm_account_telco, y = mobile_money, fill = mm_account_telco)) +
  geom_bar(stat = "identity", width = 1) +
  labs(x = "COMPANY", y = "DISTRIBUTION OF MOBILE MONEY", fill = "district") +
  ggtitle("DISTRIBUTION OF MOBILE MONEY AMONG COMPANY A, B, and C")
```

## Statistical Analysis
### T-Test for Failed Money Transactions
```R
# T-test for failed money transactions
data3$Urban_failed <- ifelse(data3$urban == "Urban" & data3$v240 == "yes", "1", "0")
View(data3) 
data3$Rural_failed <- ifelse(data3$urban == "Rural" & data3$v240 == "yes", "1", "0")

sd(data3$Urban_failed)
sd(data3$Rural_failed)
z.test(data3$Urban_failed, data3$Rural_failed, alternative = "two.sided")

# Calculating different means using t.test
x <- as.numeric(data3$Urban_failed)
y <- as.numeric(data3$Rural_failed)
t.test(x, y, paired = FALSE, alternative = "two.sided")
```

## Predictive Modeling

```R
# Predictive modeling for mobile money account cancellation
data3$mm_account_cancelled <- ifelse(data3$mm_account_cancelled == "yes", "1", "0")

# Data preprocessing for multilinear regression
# (Ordinal encoding and handling missing values)
# ...

# Multilinear regression model
set.seed(1234)
model <- lm(mm_account_cancelled ~ weight + account_num + district2 + urban + gender +
            age + hh_members + mm_account_telco_main2 + mm_account_telco2 +
            highest_grade_completed2 + account_type2 + mobile_money +
            digital_financial_inclusion + financially_excluded + v236 + v237 +
            v238 + v240 + v241 + v242 + v243 + v245 + v246 + mm_trust +
            agent_trust + prefer_cash, data = data4)
summary(model)
```
I extend my sincere gratitude to John Wafula for being a continual source of inspiration throughout this project.




