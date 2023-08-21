# Load necessary libraries
library(readxl)
library(dplyr)

# Assuming the file is located in your current working directory
data_nasdaq <- read_excel("data-nasdaq-returns.xls")
head(data_nasdaq)

selected_data <- data_nasdaq %>% select(Date, NASDAQ, AAPL)
head(selected_data)
