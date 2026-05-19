# 1. SETUP & IMPORT
# (You only need to install packages once, so I commented these out to save time)
# install.packages(c("tidyverse", "lubridate", "tidymodels", "readxl"))
library(readr)

# Load the dataset (Make sure the path is correct for your PC)
incom2024_delay_example_dataset <- read.csv("C:/Users/User/Desktop/DATA/Machine Learning Sem 4/incom2024_delay_example_dataset.csv")

# 2. EXPLORATORY DATA ANALYSIS (EDA)
# data structure (jenis data setiap kolom: int, chr, factor dll)
str(incom2024_delay_example_dataset)

# summary statistic (buat check awal ada NA/missing values tak)
summary(incom2024_delay_example_dataset)

# Tengok 6 baris teratas data
head(incom2024_delay_example_dataset)

# Pass to working variable
raw_data <- incom2024_delay_example_dataset

# 3. FIXING DATES & TARGET VARIABLES
# Extract Date part (YYYY-MM-DD) and calculate shipping days
raw_data$order_date <- substr(as.character(raw_data$order_date), 1, 10)
raw_data$shipping_date <- substr(as.character(raw_data$shipping_date), 1, 10)
raw_data$order_date <- as.Date(raw_data$order_date)
raw_data$shipping_date <- as.Date(raw_data$shipping_date)

raw_data$shipping_days <- abs(as.numeric(difftime(raw_data$shipping_date, raw_data$order_date, units = "days")))

# Create the text labels for Delivery Status
raw_data$delivery_status <- ifelse(raw_data$label == -1, "Delayed",
                                   ifelse(raw_data$label == 0, "Pending", "On_Time"))
raw_data$delivery_status <- as.factor(raw_data$delivery_status)

# 4. MISSING VALUES 
# Create a working copy and replace NA values with the median
New_df <- raw_data

New_df$sales <- ifelse(is.na(New_df$sales), 
                       median(New_df$sales, na.rm = TRUE), 
                       New_df$sales)

New_df$shipping_days <- ifelse(is.na(New_df$shipping_days), 
                               median(New_df$shipping_days, na.rm = TRUE), 
                               New_df$shipping_days)

# 5. OUTLIER HANDLING (SALES)
# Cap Sales outliers using IQR method
Q1_sales <- quantile(New_df$sales, 0.25, na.rm = TRUE)
Q3_sales <- quantile(New_df$sales, 0.75, na.rm = TRUE)
IQR_sales <- Q3_sales - Q1_sales

lower_bound_sales <- Q1_sales - 1.5 * IQR_sales
upper_bound_sales <- Q3_sales + 1.5 * IQR_sales

New_df$sales <- pmin(pmax(New_df$sales, lower_bound_sales), upper_bound_sales)

# 6. OUTLIER HANDLING (SHIPPING DAY)
# Cap Shipping Days outliers using IQR method
Q1_ship <- quantile(New_df$shipping_days, 0.25, na.rm = TRUE)
Q3_ship <- quantile(New_df$shipping_days, 0.75, na.rm = TRUE)
IQR_ship <- Q3_ship - Q1_ship

lower_bound_ship <- Q1_ship - 1.5 * IQR_ship
upper_bound_ship <- Q3_ship + 1.5 * IQR_ship

New_df$shipping_days <- pmin(pmax(New_df$shipping_days, lower_bound_ship), upper_bound_ship)

# 7. SAVE FINAL DATASET
saveRDS(New_df, "master_clean_dataset.rds")