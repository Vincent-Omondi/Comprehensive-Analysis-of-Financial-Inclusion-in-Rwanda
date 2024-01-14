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

# Digitally financially included
ggplot(data = data3, aes(x = factor(district), y = prop.table(stat(count)),
                        fill = factor(digital_financial_inclusion),
                        label = scales::percent(prop.table(stat(count))))) +
  geom_bar(position = "dodge") +
  geom_text(stat = 'count', position = position_dodge(0.9), vjust = -0.5, size = 3) +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "District", y = "Financially Included", fill = "District") +
  ggtitle("Rates of Financial Inclusion")

